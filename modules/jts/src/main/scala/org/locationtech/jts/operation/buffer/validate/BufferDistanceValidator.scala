// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
package org.locationtech.jts.operation.buffer.validate

import org.locationtech.jts.algorithm.distance.DiscreteHausdorffDistance
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryCollection
import org.locationtech.jts.geom.MultiPolygon
import org.locationtech.jts.geom.Polygon
import org.locationtech.jts.geom.util.LinearComponentExtracter
import org.locationtech.jts.geom.util.PolygonExtracter
import org.locationtech.jts.operation.distance.DistanceOp

import java.util

/**
 * Validates that a given buffer curve lies an appropriate distance from the input generating it.
 * Useful only for round buffers (cap and join). Can be used for either positive or negative
 * distances. <p> This is a heuristic test, and may return false positive results (I.e. it may fail
 * to detect an invalid result.) It should never return a false negative result, however (I.e. it
 * should never report a valid result as invalid.)
 *
 * @author
 *   mbdavis
 */
object BufferDistanceValidator {
  private val VERBOSE = false

  /**
   * Maximum allowable fraction of buffer distance the actual distance can differ by. 1% sometimes
   * causes an error - 1.2% should be safe.
   */
  private val MAX_DISTANCE_DIFF_FRAC = .012
}

class BufferDistanceValidator(var input: Geometry, var bufDistance: Double, var result: Geometry) {
  private var minValidDistance          = .0
  private var maxValidDistance          = .0
  private var minDistanceFound          = .0
  private var maxDistanceFound          = .0
  private var visValid                  = true
  private var errMsg: String            = null
  private var errorLocation: Coordinate = null
  private var errorIndicator: Geometry  = null

  def isValid: Boolean = {
    val posDistance = Math.abs(bufDistance)
    val distDelta   = BufferDistanceValidator.MAX_DISTANCE_DIFF_FRAC * posDistance
    minValidDistance = posDistance - distDelta
    maxValidDistance = posDistance + distDelta
    // can't use this test if either is empty
    if (input.isEmpty || result.isEmpty) return true
    if (bufDistance > 0.0) checkPositiveValid()
    else checkNegativeValid()
    if (BufferDistanceValidator.VERBOSE)
      System.out.println(
        "Min Dist= " + minDistanceFound + "  err= " + (1.0 - minDistanceFound / bufDistance) + "  Max Dist= " + maxDistanceFound + "  err= " + (maxDistanceFound / bufDistance - 1.0)
      )
    visValid
  }

  def getErrorMessage: String = errMsg

  def getErrorLocation: Coordinate = errorLocation

  /**
   * Gets a geometry which indicates the location and nature of a validation failure. <p> The
   * indicator is a line segment showing the location and size of the distance discrepancy.
   *
   * return a geometric error indicator or null if no error was found
   */
  def getErrorIndicator: Geometry = errorIndicator

  private def checkPositiveValid(): Unit = {
    val bufCurve = result.getBoundary
    checkMinimumDistance(input, bufCurve, minValidDistance)
    if (!visValid) return
    checkMaximumDistance(input, bufCurve, maxValidDistance)
  }

  private def checkNegativeValid()
    : Unit = { // Assert: only polygonal inputs can be checked for negative buffers
    // MD - could generalize this to handle GCs too
    if (
      !(input.isInstanceOf[Polygon] || input.isInstanceOf[MultiPolygon] || input
        .isInstanceOf[GeometryCollection])
    ) return
    val inputCurve = getPolygonLines(input)
    checkMinimumDistance(inputCurve, result, minValidDistance)
    if (!visValid) return
    checkMaximumDistance(inputCurve, result, maxValidDistance)
  }

  private def getPolygonLines(g: Geometry) = {
    val lines         = new util.ArrayList[Geometry]
    val lineExtracter = new LinearComponentExtracter(lines)
    val polys         = PolygonExtracter.getPolygons(g)
    val i             = polys.iterator
    while (i.hasNext) {
      val poly = i.next.asInstanceOf[Polygon]
      poly.applyF(lineExtracter)
    }
    g.getFactory.buildGeometry(lines)
  }

  /**
   * Checks that two geometries are at least a minimum distance apart.
   *
   * @param g1
   *   a geometry
   * @param g2
   *   a geometry
   * @param minDist
   *   the minimum distance the geometries should be separated by
   */
  private def checkMinimumDistance(g1: Geometry, g2: Geometry, minDist: Double): Unit = {
    val distOp = new DistanceOp(g1, g2, minDist)
    minDistanceFound = distOp.distance
    if (minDistanceFound < minDist) {
      visValid = false
      val pts = distOp.nearestPoints
      errorLocation = distOp.nearestPoints(1)
      errorIndicator = g1.getFactory.createLineString(pts)
//      errMsg = "Distance between buffer curve and input is too small " + "(" + minDistanceFound + " at " + WKTWriter.toLineString(pts(0), pts(1)) + " )"
      errMsg =
        "Distance between buffer curve and input is too small " + "(" + minDistanceFound + " at " + (pts(
          0
        ).toString + " " + pts(1).toString) + " )"
    }
  }

  /**
   * Checks that the furthest distance from the buffer curve to the input is less than the given
   * maximum distance. This uses the Oriented Hausdorff distance metric. It corresponds to finding
   * the point on the buffer curve which is furthest from <i>some</i> point on the input.
   *
   * @param input
   *   a geometry
   * @param bufCurve
   *   a geometry
   * @param maxDist
   *   the maximum distance that a buffer result can be from the input
   */
  private def checkMaximumDistance(input: Geometry, bufCurve: Geometry, maxDist: Double): Unit = { //    BufferCurveMaximumDistanceFinder maxDistFinder = new BufferCurveMaximumDistanceFinder(input);
    //    maxDistanceFound = maxDistFinder.findDistance(bufCurve);
    val haus = new DiscreteHausdorffDistance(bufCurve, input)
    haus.setDensifyFraction(0.25)
    maxDistanceFound = haus.orientedDistance
    if (maxDistanceFound > maxDist) {
      visValid = false
      val pts = haus.getCoordinates
      errorLocation = pts(1)
      errorIndicator = input.getFactory.createLineString(pts)
//      errMsg = "Distance between buffer curve and input is too large " + "(" + maxDistanceFound + " at " + WKTWriter.toLineString(pts(0), pts(1)) + ")"
      errMsg =
        "Distance between buffer curve and input is too large " + "(" + maxDistanceFound + " at " + (pts(
          0
        ).toString + " " + pts(1).toString) + ")"
    }
  }

  /*
    private void OLDcheckMaximumDistance(Geometry input, Geometry bufCurve, double maxDist)
    {
      BufferCurveMaximumDistanceFinder maxDistFinder = new BufferCurveMaximumDistanceFinder(input);
      maxDistanceFound = maxDistFinder.findDistance(bufCurve);


      if (maxDistanceFound > maxDist) {
        isValid = false;
        PointPairDistance ptPairDist = maxDistFinder.getDistancePoints();
        errorLocation = ptPairDist.getCoordinate(1);
        errMsg = "Distance between buffer curve and input is too large "
          + "(" + ptPairDist.getDistance()
          + " at " + ptPairDist.toString() +")";
      }
    }
   */
}
