// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

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
 */
package org.locationtech.jts.operation.buffer

import org.locationtech.jts.algorithm.Distance
import org.locationtech.jts.algorithm.Orientation
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateList

/**
 * Simplifies a buffer input line to remove concavities with shallow depth. <p> The most important
 * benefit of doing this is to reduce the number of points and the complexity of shape which will be
 * buffered. It also reduces the risk of gores created by the quantized fillet arcs (although this
 * issue should be eliminated in any case by the offset curve generation logic). <p> A key aspect of
 * the simplification is that it affects inside (concave or inward) corners only. Convex (outward)
 * corners are preserved, since they are required to ensure that the generated buffer curve lies at
 * the correct distance from the input geometry. <p> Another important heuristic used is that the
 * end segments of the input are never simplified. This ensures that the client buffer code is able
 * to generate end caps faithfully. <p> No attempt is made to avoid self-intersections in the
 * output. This is acceptable for use for generating a buffer offset curve, since the buffer
 * algorithm is insensitive to invalid polygonal geometry. However, this means that this algorithm
 * cannot be used as a general-purpose polygon simplification technique.
 *
 * @author
 *   Martin Davis
 */
object BufferInputLineSimplifier {

  /**
   * Simplify the input coordinate list. If the distance tolerance is positive, concavities on the
   * LEFT side of the line are simplified. If the supplied distance tolerance is negative,
   * concavities on the RIGHT side of the line are simplified.
   *
   * @param inputLine
   *   the coordinate list to simplify
   * @param distanceTol
   *   simplification distance tolerance to use return the simplified coordinate list
   */
  def simplify(inputLine: Array[Coordinate], distanceTol: Double): Array[Coordinate] = {
    val simp = new BufferInputLineSimplifier(inputLine)
    simp.simplify(distanceTol)
  }

//  private val INIT = 0
  private val DELETE           = 1
//  private val KEEP = 1
  private val NUM_PTS_TO_CHECK = 10
}

class BufferInputLineSimplifier(var inputLine: Array[Coordinate]) {
  private var distanceTol            = .0
  private var isDeleted: Array[Byte] = null
  private var angleOrientation       = Orientation.COUNTERCLOCKWISE

  /**
   * Simplify the input coordinate list. If the distance tolerance is positive, concavities on the
   * LEFT side of the line are simplified. If the supplied distance tolerance is negative,
   * concavities on the RIGHT side of the line are simplified.
   *
   * @param distanceTol
   *   simplification distance tolerance to use return the simplified coordinate list
   */
  def simplify(distanceTol: Double): Array[Coordinate] = {
    this.distanceTol = Math.abs(distanceTol)
    if (distanceTol < 0) angleOrientation = Orientation.CLOCKWISE
    // rely on fact that boolean array is filled with false value
    isDeleted = new Array[Byte](inputLine.length)
    var isChanged = false
    while ({ isChanged = deleteShallowConcavities; isChanged }) ()
    collapseLine
  }

  /**
   * Uses a sliding window containing 3 vertices to detect shallow angles in which the middle vertex
   * can be deleted, since it does not affect the shape of the resulting buffer in a significant
   * way.
   *
   * return
   */
  private def deleteShallowConcavities = {

    /**
     * Do not simplify end line segments of the line string. This ensures that end caps are
     * generated consistently.
     */
    var index     = 1
    var midIndex  = findNextNonDeletedIndex(index)
    var lastIndex = findNextNonDeletedIndex(midIndex)
    var isChanged = false
    while (lastIndex < inputLine.length) { // test triple for shallow concavity
      var isMiddleVertexDeleted = false
      if (isDeletable(index, midIndex, lastIndex, distanceTol)) {
        isDeleted(midIndex) = BufferInputLineSimplifier.DELETE.toByte
        isMiddleVertexDeleted = true
        isChanged = true
      }
      // move simplification window forward
      if (isMiddleVertexDeleted) index = lastIndex
      else index = midIndex
      midIndex = findNextNonDeletedIndex(index)
      lastIndex = findNextNonDeletedIndex(midIndex)
    }
    isChanged
  }

  /**
   * Finds the next non-deleted index, or the end of the point array if none
   *
   * @param index
   *   return the next non-deleted index, if any or inputLine.length if there are no more
   *   non-deleted indices
   */
  private def findNextNonDeletedIndex(index: Int) = {
    var next = index + 1
    while (next < inputLine.length && isDeleted(next) == BufferInputLineSimplifier.DELETE) {
      next += 1; next - 1
    }
    next
  }

  private def collapseLine = {
    val coordList = new CoordinateList(Array.empty)
    var i         = 0
    while (i < inputLine.length) {
      if (isDeleted(i) != BufferInputLineSimplifier.DELETE) coordList.add(inputLine(i))
      i += 1
    }
    //    if (coordList.size() < inputLine.length)      System.out.println("Simplified " + (inputLine.length - coordList.size()) + " pts");
    coordList.toCoordinateArray
  }

  private def isDeletable(i0: Int, i1: Int, i2: Int, distanceTol: Double): Boolean = {
    val p0 = inputLine(i0)
    val p1 = inputLine(i1)
    val p2 = inputLine(i2)
    if (!isConcave(p0, p1, p2)) return false
    if (!isShallow(p0, p1, p2, distanceTol)) return false
    // MD - don't use this heuristic - it's too restricting
    //  	if (p0.distance(p2) > distanceTol) return false;
    isShallowSampled(p0, p1, i0, i2, distanceTol)
  }

//  private def isShallowConcavity(p0: Coordinate, p1: Coordinate, p2: Coordinate, distanceTol: Double): Boolean = {
//    val orientation = Orientation.index(p0, p1, p2)
//    val isAngleToSimplify = orientation == angleOrientation
//    if (!isAngleToSimplify) return false
//    val dist = Distance.pointToSegment(p1, p0, p2)
//    dist < distanceTol
//  }

  /**
   * Checks for shallowness over a sample of points in the given section. This helps prevents the
   * simplification from incrementally "skipping" over points which are in fact non-shallow.
   *
   * @param p0
   *   start coordinate of section
   * @param p2
   *   end coordinate of section
   * @param i0
   *   start index of section
   * @param i2
   *   end index of section
   * @param distanceTol
   *   distance tolerance return
   */
  private def isShallowSampled(
    p0:          Coordinate,
    p2:          Coordinate,
    i0:          Int,
    i2:          Int,
    distanceTol: Double
  ): Boolean = { // check every n'th point to see if it is within tolerance
    var inc = (i2 - i0) / BufferInputLineSimplifier.NUM_PTS_TO_CHECK
    if (inc <= 0) inc = 1
    var i   = i0
    while (i < i2) {
      if (!isShallow(p0, p2, inputLine(i), distanceTol)) return false
      i += inc
    }
    true
  }

  private def isShallow(p0: Coordinate, p1: Coordinate, p2: Coordinate, distanceTol: Double) = {
    val dist = Distance.pointToSegment(p1, p0, p2)
    dist < distanceTol
  }

  private def isConcave(p0: Coordinate, p1: Coordinate, p2: Coordinate) = {
    val orientation = Orientation.index(p0, p1, p2)
    val isConcave   = orientation == angleOrientation
    isConcave
  }
}
