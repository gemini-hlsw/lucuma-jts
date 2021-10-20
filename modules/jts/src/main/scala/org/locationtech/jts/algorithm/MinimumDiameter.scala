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
package org.locationtech.jts.algorithm

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.LineSegment
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.Polygon

/**
 * Computes the minimum diameter of a {link Geometry}. The minimum diameter is defined to be the
 * width of the smallest band that contains the geometry, where a band is a strip of the plane
 * defined by two parallel lines. This can be thought of as the smallest hole that the geometry can
 * be moved through, with a single rotation. <p> The first step in the algorithm is computing the
 * convex hull of the Geometry. If the input Geometry is known to be convex, a hint can be supplied
 * to avoid this computation. <p> This class can also be used to compute a line segment representing
 * the minimum diameter, the supporting line segment of the minimum diameter, and a minimum
 * rectangle enclosing the input geometry. This rectangle will have width equal to the minimum
 * diameter, and have one side parallel to the supporting segment.
 *
 * @see
 *   ConvexHull
 * @version 1.7
 */
object MinimumDiameter {

  /**
   * Gets the minimum rectangle enclosing a geometry.
   *
   * @param geom
   *   the geometry return the minimum rectangle enclosing the geometry
   */
  def getMinimumRectangle(geom: Geometry): Geometry = new MinimumDiameter(geom).getMinimumRectangle

  /**
   * Gets the length of the minimum diameter enclosing a geometry
   *
   * @param geom
   *   the geometry return the length of the minimum diameter of the geometry
   */
  def getMinimumDiameter(geom: Geometry): LineString = new MinimumDiameter(geom).getDiameter

  private def nextIndex(pts: Array[Coordinate], inde: Int) = {
    var index = inde
    index += 1
    if (index >= pts.length) index = 0
    index
  }

  private def computeC(a: Double, b: Double, p: Coordinate) = a * p.y - b * p.x

  private def computeSegmentForLine(a: Double, b: Double, c: Double) = {
    var p0: Coordinate = null
    var p1: Coordinate = null
    /*
     * Line eqn is ax + by = c
     * Slope is a/b.
     * If slope is steep, use y values as the inputs
     */
    if (Math.abs(b) > Math.abs(a)) {
      p0 = new Coordinate(0.0, c / b)
      p1 = new Coordinate(1.0, c / b - a / b)
    } else {
      p0 = new Coordinate(c / a, 0.0)
      p1 = new Coordinate(c / a - b / a, 1.0)
    }
    new LineSegment(p0, p1)
  }
}

class MinimumDiameter(val inputGeom: Geometry, val isConvex: Boolean) {

  /**
   * Compute a minimum diameter for a giver {link Geometry}, with a hint if the Geometry is convex
   * (e.g. a convex Polygon or LinearRing, or a two-point LineString, or a Point).
   *
   * @param inputGeom
   *   a Geometry which is convex
   * @param isConvex
   *   <code>true</code> if the input geometry is convex
   */
  private var convexHullPts: Array[Coordinate] = null
  private var minBaseSeg                       = new LineSegment
  private var minWidthPt: Coordinate           = null
  private var minPtIndex                       = 0
  private var minWidth                         = 0.0

  /**
   * Compute a minimum diameter for a given {link Geometry}.
   *
   * @param inputGeom
   *   a Geometry
   */
  def this(inputGeom: Geometry) =
    this(inputGeom, false)

  /**
   * Gets the length of the minimum diameter of the input Geometry
   *
   * return the length of the minimum diameter
   */
  def getLength: Double = {
    computeMinimumDiameter()
    minWidth
  }

  /**
   * Gets the {link Coordinate} forming one end of the minimum diameter
   *
   * return a coordinate forming one end of the minimum diameter
   */
  def getWidthCoordinate: Coordinate = {
    computeMinimumDiameter()
    minWidthPt
  }

  /**
   * Gets the segment forming the base of the minimum diameter
   *
   * return the segment forming the base of the minimum diameter
   */
  def getSupportingSegment: LineString = {
    computeMinimumDiameter()
    inputGeom.getFactory.createLineString(Array[Coordinate](minBaseSeg.p0, minBaseSeg.p1))
  }

  /**
   * Gets a {link LineString} which is a minimum diameter
   *
   * return a { @link LineString} which is a minimum diameter
   */
  def getDiameter: LineString = {
    computeMinimumDiameter()
    // return empty linestring if no minimum width calculated
    if (minWidthPt == null) return inputGeom.getFactory.createLineString
    val basePt = minBaseSeg.project(minWidthPt)
    inputGeom.getFactory.createLineString(Array[Coordinate](basePt, minWidthPt))
  }

  private def computeMinimumDiameter(): Unit = { // check if computation is cached
    if (minWidthPt != null) return
    if (isConvex) computeWidthConvex(inputGeom)
    else {
      val convexGeom = new ConvexHull(inputGeom).getConvexHull
      computeWidthConvex(convexGeom)
    }
  }

  private def computeWidthConvex(convexGeom: Geometry): Unit = { //System.out.println("Input = " + geom);
    if (convexGeom.isInstanceOf[Polygon])
      convexHullPts = convexGeom.asInstanceOf[Polygon].getExteriorRing.getCoordinates
    else convexHullPts = convexGeom.getCoordinates
    // special cases for lines or points or degenerate rings
    if (convexHullPts.length == 0) {
      minWidth = 0.0
      minWidthPt = null
      minBaseSeg = null
    } else if (convexHullPts.length == 1) {
      minWidth = 0.0
      minWidthPt = convexHullPts(0)
      minBaseSeg.p0 = convexHullPts(0)
      minBaseSeg.p1 = convexHullPts(0)
    } else if (convexHullPts.length == 2 || convexHullPts.length == 3) {
      minWidth = 0.0
      minWidthPt = convexHullPts(0)
      minBaseSeg.p0 = convexHullPts(0)
      minBaseSeg.p1 = convexHullPts(1)
    } else computeConvexRingMinDiameter(convexHullPts)
  }

  /**
   * Compute the width information for a ring of {link Coordinate}s. Leaves the width information in
   * the instance variables.
   *
   * @param pts
   */
  private def computeConvexRingMinDiameter(pts: Array[Coordinate]): Unit = { // for each segment in the ring
    minWidth = Double.MaxValue
    var currMaxIndex = 1
    val seg          = new LineSegment
    // compute the max distance for all segments in the ring, and pick the minimum
    var i            = 0
    while (i < pts.length - 1) {
      seg.p0 = pts(i)
      seg.p1 = pts(i + 1)
      currMaxIndex = findMaxPerpDistance(pts, seg, currMaxIndex)
      i += 1
    }
  }

  private def findMaxPerpDistance(pts: Array[Coordinate], seg: LineSegment, startIndex: Int) = {
    var maxPerpDistance  = seg.distancePerpendicular(pts(startIndex))
    var nextPerpDistance = maxPerpDistance
    var maxIndex         = startIndex
    var nextIndex        = maxIndex
    while (nextPerpDistance >= maxPerpDistance) {
      maxPerpDistance = nextPerpDistance
      maxIndex = nextIndex
      nextIndex = MinimumDiameter.nextIndex(pts, maxIndex)
      nextPerpDistance = seg.distancePerpendicular(pts(nextIndex))
    }
    // found maximum width for this segment - update global min dist if appropriate
    if (maxPerpDistance < minWidth) {
      minPtIndex = maxIndex
      minWidth = maxPerpDistance
      minWidthPt = pts(minPtIndex)
      minBaseSeg = new LineSegment(seg)
      //      System.out.println(minBaseSeg);
      //      System.out.println(minWidth);
    }
    maxIndex
  }

  /**
   * Gets the minimum rectangular {link Polygon} which encloses the input geometry. The rectangle
   * has width equal to the minimum diameter, and a longer length. If the convex hull of the input
   * is degenerate (a line or point) a {link LineString} or {link Point} is returned. <p> The
   * minimum rectangle can be used as an extremely generalized representation for the given
   * geometry.
   *
   * return the minimum rectangle enclosing the input (or a line or point if degenerate)
   */
  def getMinimumRectangle: Geometry = {
    computeMinimumDiameter()
    // check if minimum rectangle is degenerate (a point or line segment)
    if (minWidth == 0.0) {
      if (minBaseSeg.p0.equals2D(minBaseSeg.p1))
        return inputGeom.getFactory.createPoint(minBaseSeg.p0)
      return minBaseSeg.toGeometry(inputGeom.getFactory)
    }
    // deltas for the base segment of the minimum diameter
    val dx          = minBaseSeg.p1.x - minBaseSeg.p0.x
    val dy          = minBaseSeg.p1.y - minBaseSeg.p0.y
    /*
        double c0 = computeC(dx, dy, minBaseSeg.p0);
        double c1 = computeC(dx, dy, minBaseSeg.p1);
     */
    var minPara     = Double.MaxValue
    var maxPara     = -Double.MaxValue
    var minPerp     = Double.MaxValue
    var maxPerp     = -Double.MaxValue
    // compute maxima and minima of lines parallel and perpendicular to base segment
    var i           = 0
    while (i < convexHullPts.length) {
      val paraC = MinimumDiameter.computeC(dx, dy, convexHullPts(i))
      if (paraC > maxPara) maxPara = paraC
      if (paraC < minPara) minPara = paraC
      val perpC = MinimumDiameter.computeC(-dy, dx, convexHullPts(i))
      if (perpC > maxPerp) maxPerp = perpC
      if (perpC < minPerp) minPerp = perpC
      i += 1
    }
    // compute lines along edges of minimum rectangle
    val maxPerpLine = MinimumDiameter.computeSegmentForLine(-dx, -dy, maxPerp)
    val minPerpLine = MinimumDiameter.computeSegmentForLine(-dx, -dy, minPerp)
    val maxParaLine = MinimumDiameter.computeSegmentForLine(-dy, dx, maxPara)
    val minParaLine = MinimumDiameter.computeSegmentForLine(-dy, dx, minPara)
    // compute vertices of rectangle (where the para/perp max & min lines intersect)
    val p0          = maxParaLine.lineIntersection(maxPerpLine)
    val p1          = minParaLine.lineIntersection(maxPerpLine)
    val p2          = minParaLine.lineIntersection(minPerpLine)
    val p3          = maxParaLine.lineIntersection(minPerpLine)
    val shell       = inputGeom.getFactory.createLinearRing(Array[Coordinate](p0, p1, p2, p3, p0))
    inputGeom.getFactory.createPolygon(shell)
  }
}
