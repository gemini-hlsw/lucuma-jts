/*
 * Copyright (c) 2016 Vivid Solutions.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */ /*
 * Copyright (c) 2016 Vivid Solutions.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.operation.buffer

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateArrays
import org.locationtech.jts.geom.PrecisionModel
import org.locationtech.jts.geomgraph.Position
import scala.annotation.nowarn

/**
 * Computes the raw offset curve for a single {link Geometry} component (ring, line or point). A raw
 * offset curve line is not noded - it may contain self-intersections (and usually will). The final
 * buffer polygon is computed by forming a topological graph of all the noded raw curves and tracing
 * outside contours. The points in the raw curve are rounded to a given {link PrecisionModel}.
 *
 * @version 1.7
 */
object OffsetCurveBuilder {
  private def copyCoordinates(pts: Array[Coordinate]) = {
    val copy = new Array[Coordinate](pts.length)
    var i    = 0
    while (i < copy.length) {
      copy(i) = new Coordinate(pts(i))
      i += 1
    }
    copy
  }
}

class OffsetCurveBuilder(var precisionModel: PrecisionModel, var bufParams: BufferParameters) {
  private var distance = 0.0

  /**
   * Gets the buffer parameters being used to generate the curve.
   *
   * return the buffer parameters being used
   */
  def getBufferParameters: BufferParameters = bufParams

  /**
   * This method handles single points as well as LineStrings. LineStrings are assumed <b>not</b> to
   * be closed (the function will not fail for closed lines, but will generate superfluous line
   * caps).
   *
   * @param inputPts
   *   the vertices of the line to offset
   * @param distance
   *   the offset distance return a Coordinate array representing the curve or null if the curve is
   *   empty
   */
  def getLineCurve(inputPts: Array[Coordinate], distance: Double): Array[Coordinate] = {
    this.distance = distance
    if (isLineOffsetEmpty(distance)) return null
    val posDistance = Math.abs(distance)
    val segGen      = getSegGen(posDistance)
    if (inputPts.length <= 1) computePointCurve(inputPts(0), segGen)
    else if (bufParams.isSingleSided) {
      val isRightSide = distance < 0.0
      computeSingleSidedBufferCurve(inputPts, isRightSide, segGen)
    } else computeLineBufferCurve(inputPts, segGen)
    val lineCoord   = segGen.getCoordinates
    lineCoord
  }

  /**
   * Tests whether the offset curve for line or point geometries at the given offset distance is
   * empty (does not exist). This is the case if: <ul> <li>the distance is zero, <li>the distance is
   * negative, except for the case of singled-sided buffers </ul>
   *
   * @param distance
   *   the offset curve distance return true if the offset curve is empty
   */
  def isLineOffsetEmpty(distance: Double): Boolean = { // a zero width buffer of a line or point is empty
    if (distance == 0.0) return true
    // a negative width buffer of a line or point is empty,
    // except for single-sided buffers, where the sign indicates the side
    if (distance < 0.0 && !bufParams.isSingleSided) return true
    false
  }

  /**
   * This method handles the degenerate cases of single points and lines, as well as valid rings.
   *
   * @param inputPts
   *   the coordinates of the ring (must not contain repeated points)
   * @param side
   *   side the side { @link Position} of the ring on which to construct the buffer line
   * @param distance
   *   the positive distance at which to create the offset return a Coordinate array representing
   *   the curve, or null if the curve is empty
   */
  def getRingCurve(inputPts: Array[Coordinate], side: Int, distance: Double): Array[Coordinate] = {
    this.distance = distance
    if (inputPts.length <= 2) return getLineCurve(inputPts, distance)
    // optimize creating ring for for zero distance
    if (distance == 0.0) return OffsetCurveBuilder.copyCoordinates(inputPts)
    val segGen = getSegGen(distance)
    computeRingBufferCurve(inputPts, side, segGen)
    segGen.getCoordinates
  }

  def getOffsetCurve(inputPts: Array[Coordinate], distance: Double): Array[Coordinate] = {
    this.distance = distance
    // a zero width offset curve is empty
    if (distance == 0.0) return null
    val isRightSide = distance < 0.0
    val posDistance = Math.abs(distance)
    val segGen      = getSegGen(posDistance)
    if (inputPts.length <= 1) computePointCurve(inputPts(0), segGen)
    else computeOffsetCurve(inputPts, isRightSide, segGen)
    val curvePts    = segGen.getCoordinates
    // for right side line is traversed in reverse direction, so have to reverse generated line
    if (isRightSide) CoordinateArrays.reverse(curvePts)
    curvePts
  }

  private def getSegGen(distance: Double) =
    new OffsetSegmentGenerator(precisionModel, bufParams, distance)

  /**
   * Computes the distance tolerance to use during input line simplification.
   *
   * @param distance
   *   the buffer distance return the simplification tolerance
   */
  private def simplifyTolerance(bufDistance: Double) = bufDistance * bufParams.getSimplifyFactor

  @nowarn
  private def computePointCurve(pt: Coordinate, segGen: OffsetSegmentGenerator) =
    bufParams.getEndCapStyle match {
      case BufferParameters.CAP_ROUND  =>
        segGen.createCircle(pt)
      case BufferParameters.CAP_SQUARE =>
        segGen.createSquare(pt)
      // otherwise curve is empty (e.g. for a butt cap);
    }

  private def computeLineBufferCurve(
    inputPts: Array[Coordinate],
    segGen:   OffsetSegmentGenerator
  ): Unit = {
    val distTol = simplifyTolerance(distance)
    // --------- compute points for left side of line
    // Simplify the appropriate side of the line before generating
    val simp1   = BufferInputLineSimplifier.simplify(inputPts, distTol)
    // MD - used for testing only (to eliminate simplification)
    //    Coordinate[] simp1 = inputPts;
    val n1      = simp1.length - 1
    segGen.initSideSegments(simp1(0), simp1(1), Position.LEFT)
    var i       = 2
    while (i <= n1) {
      segGen.addNextSegment(simp1(i), true)
      i += 1
    }
    segGen.addLastSegment()
    // add line cap for end of line
    segGen.addLineEndCap(simp1(n1 - 1), simp1(n1))
    // ---------- compute points for right side of line
    val simp2   = BufferInputLineSimplifier.simplify(inputPts, -distTol)
    //    Coordinate[] simp2 = inputPts;
    val n2      = simp2.length - 1
    // since we are traversing line in opposite order, offset position is still LEFT
    segGen.initSideSegments(simp2(n2), simp2(n2 - 1), Position.LEFT)
    i = n2 - 2
    while (i >= 0) {
      segGen.addNextSegment(simp2(i), true)
      i -= 1
    }
    segGen.addLastSegment()
    // add line cap for start of line
    segGen.addLineEndCap(simp2(1), simp2(0))
    segGen.closeRing()
  }

  private def computeSingleSidedBufferCurve(
    inputPts:    Array[Coordinate],
    isRightSide: Boolean,
    segGen:      OffsetSegmentGenerator
  ): Unit = {
    val distTol = simplifyTolerance(distance)
    if (isRightSide) { // add original line
      segGen.addSegments(inputPts, true)
      val simp2 = BufferInputLineSimplifier.simplify(inputPts, -distTol)
      val n2    = simp2.length - 1
      segGen.initSideSegments(simp2(n2), simp2(n2 - 1), Position.LEFT)
      segGen.addFirstSegment()
      var i     = n2 - 2
      while (i >= 0) {
        segGen.addNextSegment(simp2(i), true)
        i -= 1
      }
    } else {
      segGen.addSegments(inputPts, false)
      val simp1 = BufferInputLineSimplifier.simplify(inputPts, distTol)
      //      Coordinate[] simp1 = inputPts;
      val n1    = simp1.length - 1
      segGen.initSideSegments(simp1(0), simp1(1), Position.LEFT)
      segGen.addFirstSegment()
      var i     = 2
      while (i <= n1) {
        segGen.addNextSegment(simp1(i), true)
        i += 1
      }
    }
    segGen.addLastSegment()
    segGen.closeRing()
  }

  private def computeOffsetCurve(
    inputPts:    Array[Coordinate],
    isRightSide: Boolean,
    segGen:      OffsetSegmentGenerator
  ): Unit = {
    val distTol = simplifyTolerance(distance)
    if (isRightSide) {
      val simp2 = BufferInputLineSimplifier.simplify(inputPts, -distTol)
      val n2    = simp2.length - 1
      segGen.initSideSegments(simp2(n2), simp2(n2 - 1), Position.LEFT)
      segGen.addFirstSegment()
      var i     = n2 - 2
      while (i >= 0) {
        segGen.addNextSegment(simp2(i), true)
        i -= 1
      }
    } else {
      val simp1 = BufferInputLineSimplifier.simplify(inputPts, distTol)
      val n1    = simp1.length - 1
      segGen.initSideSegments(simp1(0), simp1(1), Position.LEFT)
      segGen.addFirstSegment()
      var i     = 2
      while (i <= n1) {
        segGen.addNextSegment(simp1(i), true)
        i += 1
      }
    }
    segGen.addLastSegment()
  }

  private def computeRingBufferCurve(
    inputPts: Array[Coordinate],
    side:     Int,
    segGen:   OffsetSegmentGenerator
  ): Unit = { // simplify input line to improve performance
    var distTol = simplifyTolerance(distance)
    // ensure that correct side is simplified
    if (side == Position.RIGHT) distTol = -distTol
    val simp    = BufferInputLineSimplifier.simplify(inputPts, distTol)
    //    Coordinate[] simp = inputPts;
    val n       = simp.length - 1
    segGen.initSideSegments(simp(n - 1), simp(0), side)
    var i       = 1
    while (i <= n) {
      val addStartPoint = i != 1
      segGen.addNextSegment(simp(i), addStartPoint)
      i += 1
    }
    segGen.closeRing()
  }
}
