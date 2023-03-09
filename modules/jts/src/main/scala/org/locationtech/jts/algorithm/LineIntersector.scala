// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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
package org.locationtech.jts.algorithm

/**
 * @version 1.7
 */

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.PrecisionModel
import org.locationtech.jts.util.Assert

/**
 * A <code>LineIntersector</code> is an algorithm that can both test whether two line segments
 * intersect and compute the intersection point(s) if they do. <p> There are three possible outcomes
 * when determining whether two line segments intersect: <ul> <li>{link #NO_INTERSECTION} - the
 * segments do not intersect <li>{link #POINT_INTERSECTION} - the segments intersect in a single
 * point <li>{link #COLLINEAR_INTERSECTION} - the segments are collinear and they intersect in a
 * line segment </ul> For segments which intersect in a single point, the point may be either an
 * endpoint or in the interior of each segment. If the point lies in the interior of both segments,
 * this is termed a <i>proper intersection</i>. The method {link #isProper()} test for this
 * situation. <p> The intersection point(s) may be computed in a precise or non-precise manner.
 * Computing an intersection point precisely involves rounding it via a supplied {link
 * PrecisionModel}. <p> LineIntersectors do not perform an initial envelope intersection test to
 * determine if the segments are disjoint. This is because this class is likely to be used in a
 * context where envelope overlap is already known to occur (or be likely).
 *
 * @version 1.7
 */
object LineIntersector {

  /**
   * These are deprecated, due to ambiguous naming
   */
  val DONT_INTERSECT = 0
  val DO_INTERSECT   = 1
  val COLLINEAR      = 2

  /**
   * Indicates that line segments do not intersect
   */
  val NO_INTERSECTION = 0

  /**
   * Indicates that line segments intersect in a single point
   */
  val POINT_INTERSECTION = 1

  /**
   * Indicates that line segments intersect in a line segment
   */
  val COLLINEAR_INTERSECTION = 2

  /**
   * Computes the "edge distance" of an intersection point p along a segment. The edge distance is a
   * metric of the point along the edge. The metric used is a robust and easy to compute metric
   * function. It is <b>not</b> equivalent to the usual Euclidean metric. It relies on the fact that
   * either the x or the y ordinates of the points in the edge are unique, depending on whether the
   * edge is longer in the horizontal or vertical direction. <p> NOTE: This function may produce
   * incorrect distances for inputs where p is not precisely on p1-p2 (E.g. p = (139,9) p1 =
   * (139,10), p2 = (280,1) produces distance 0.0, which is incorrect. <p> My hypothesis is that the
   * function is safe to use for points which are the result of <b>rounding</b> points which lie on
   * the line, but not safe to use for <b>truncated</b> points.
   */
  def computeEdgeDistance(p: Coordinate, p0: Coordinate, p1: Coordinate): Double = {
    val dx   = Math.abs(p1.x - p0.x)
    val dy   = Math.abs(p1.y - p0.y)
    var dist = -1.0 // sentinel value
    if (p == p0) dist = 0.0
    else if (p == p1)
      if (dx > dy) dist = dx
      else dist = dy
    else {
      val pdx = Math.abs(p.x - p0.x)
      val pdy = Math.abs(p.y - p0.y)
      if (dx > dy) dist = pdx
      else dist = pdy
      // <FIX>
      // hack to ensure that non-endpoints always have a non-zero distance
      if (dist == 0.0 && !(p == p0)) dist = Math.max(pdx, pdy)
    }
    Assert.isTrue(!(dist == 0.0 && !(p == p0)), "Bad distance calculation")
    dist
  }

  /**
   * This function is non-robust, since it may compute the square of large numbers. Currently not
   * sure how to improve this.
   */
  def nonRobustComputeEdgeDistance(p: Coordinate, p1: Coordinate, p2: Coordinate): Double = {
    val dx   = p.x - p1.x
    val dy   = p.y - p1.y
    val dist = Math.sqrt(dx * dx + dy * dy) // dummy value
    Assert.isTrue(!(dist == 0.0 && !(p == p1)), "Invalid distance calculation")
    dist
  }
}

abstract class LineIntersector() {
  protected var result                               = 0
  protected var inputLines: Array[Array[Coordinate]] = Array.ofDim[Coordinate](2, 2)
  protected var intPt: Array[Coordinate]             = Array[Coordinate](new Coordinate(), new Coordinate())

  /**
   * The indexes of the endpoints of the intersection lines, in order along the corresponding line
   */
  protected var intLineIndex: Array[Array[Int]] = null
  protected[jts] var isProperF                  = false
  protected var pa: Coordinate                  = intPt(0)
  protected var pb: Coordinate                  = intPt(1)
  // alias the intersection points for ease of reference
  /**
   * If makePrecise is true, computed intersection coordinates will be made precise using
   * Coordinate#makePrecise
   */
  protected var precisionModel: PrecisionModel  = null

  /**
   * Force computed intersection to be rounded to a given precision model
   *
   * @param precisionModel
   * @deprecated
   *   use <code>setPrecisionModel</code> instead
   */
  def setMakePrecise(precisionModel: PrecisionModel): Unit = this.precisionModel = precisionModel

  /**
   * Force computed intersection to be rounded to a given precision model. No getter is provided,
   * because the precision model is not required to be specified.
   *
   * @param precisionModel
   */
  def setPrecisionModel(precisionModel: PrecisionModel): Unit = this.precisionModel = precisionModel

  /**
   * Gets an endpoint of an input segment.
   *
   * @param segmentIndex
   *   the index of the input segment (0 or 1)
   * @param ptIndex
   *   the index of the endpoint (0 or 1) return the specified endpoint
   */
  def getEndpoint(segmentIndex: Int, ptIndex: Int): Coordinate = inputLines(segmentIndex)(ptIndex)

  /**
   * Compute the intersection of a point p and the line p1-p2. This function computes the boolean
   * value of the hasIntersection test. The actual value of the intersection (if there is one) is
   * equal to the value of <code>p</code>.
   */
  def computeIntersection(p: Coordinate, p1: Coordinate, p2: Coordinate): Unit

  protected def isCollinear: Boolean = result == LineIntersector.COLLINEAR_INTERSECTION

  /**
   * Computes the intersection of the lines p1-p2 and p3-p4. This function computes both the boolean
   * value of the hasIntersection test and the (approximate) value of the intersection point itself
   * (if there is one).
   */
  def computeIntersection(p1: Coordinate, p2: Coordinate, p3: Coordinate, p4: Coordinate): Unit = {
    inputLines(0)(0) = p1
    inputLines(0)(1) = p2
    inputLines(1)(0) = p3
    inputLines(1)(1) = p4
    result = computeIntersect(p1, p2, p3, p4)
    // numIntersects++;
  }

  protected def computeIntersect(
    p1: Coordinate,
    p2: Coordinate,
    q1: Coordinate,
    q2: Coordinate
  ): Int

//  override def toString: String = WKTWriter.toLineString(inputLines(0)(0), inputLines(0)(1)) + " - " + WKTWriter.toLineString(inputLines(1)(0), inputLines(1)(1)) + getTopologySummary
  override def toString: String =
    s"${(inputLines(0)(0), inputLines(0)(1))} - (inputLines(1)(0), inputLines(1)(1)) + getTopologySummary"

//  private def getTopologySummary = {
//    val catBuilder = new StringBuilder
//    if (isEndPoint) catBuilder.append(" endpoint")
//    if (this.isProperF) catBuilder.append(" proper")
//    if (isCollinear) catBuilder.append(" collinear")
//    catBuilder.toString
//  }

  protected def isEndPoint: Boolean = hasIntersection && !this.isProperF

  /**
   * Tests whether the input geometries intersect.
   *
   * return true if the input geometries intersect
   */
  def hasIntersection: Boolean = result != LineIntersector.NO_INTERSECTION

  /**
   * Returns the number of intersection points found. This will be either 0, 1 or 2.
   *
   * return the number of intersection points found (0, 1, or 2)
   */
  def getIntersectionNum: Int = result

  /**
   * Returns the intIndex'th intersection point
   *
   * @param intIndex
   *   is 0 or 1 return the intIndex'th intersection point
   */
  def getIntersection(intIndex: Int): Coordinate = intPt(intIndex)

  protected def computeIntLineIndex(): Unit = if (intLineIndex == null) {
    intLineIndex = Array.ofDim[Int](2, 2)
    computeIntLineIndex(0)
    computeIntLineIndex(1)
  }

  /**
   * Test whether a point is a intersection point of two line segments. Note that if the
   * intersection is a line segment, this method only tests for equality with the endpoints of the
   * intersection segment. It does <b>not</b> return true if the input point is internal to the
   * intersection segment.
   *
   * return true if the input point is one of the intersection points.
   */
  def isIntersection(pt: Coordinate): Boolean = {
    var i = 0
    while (i < result) {
      if (intPt(i).equals2D(pt)) return true
      i += 1
    }
    false
  }

  /**
   * Tests whether either intersection point is an interior point of one of the input segments.
   *
   * return <code>true</code> if either intersection point is in the interior of one of the input
   * segments
   */
  def isInteriorIntersection: Boolean = {
    if (isInteriorIntersection(0)) return true
    if (isInteriorIntersection(1)) return true
    false
  }

  /**
   * Tests whether either intersection point is an interior point of the specified input segment.
   *
   * return <code>true</code> if either intersection point is in the interior of the input segment
   */
  def isInteriorIntersection(inputLineIndex: Int): Boolean = {
    var i = 0
    while (i < result) {
      if (
        !(intPt(i).equals2D(inputLines(inputLineIndex)(0)) || intPt(i).equals2D(
          inputLines(inputLineIndex)(1)
        ))
      ) return true
      i += 1
    }
    false
  }

  /**
   * Tests whether an intersection is proper. <br> The intersection between two line segments is
   * considered proper if they intersect in a single point in the interior of both segments (e.g.
   * the intersection is a single point and is not equal to any of the endpoints). <p> The
   * intersection between a point and a line segment is considered proper if the point lies in the
   * interior of the segment (e.g. is not equal to either of the endpoints).
   *
   * return true if the intersection is proper
   */
  def isProper(): Boolean = hasIntersection && isProperF

  /**
   * Computes the intIndex'th intersection point in the direction of a specified input line segment
   *
   * @param segmentIndex
   *   is 0 or 1
   * @param intIndex
   *   is 0 or 1 return the intIndex'th intersection point in the direction of the specified input
   *   line segment
   */
  def getIntersectionAlongSegment(segmentIndex: Int, intIndex: Int): Coordinate = { // lazily compute int line array
    computeIntLineIndex()
    intPt(intLineIndex(segmentIndex)(intIndex))
  }

  /**
   * Computes the index (order) of the intIndex'th intersection point in the direction of a
   * specified input line segment
   *
   * @param segmentIndex
   *   is 0 or 1
   * @param intIndex
   *   is 0 or 1 return the index of the intersection point along the input segment (0 or 1)
   */
  def getIndexAlongSegment(segmentIndex: Int, intIndex: Int): Int = {
    computeIntLineIndex()
    intLineIndex(segmentIndex)(intIndex)
  }

  protected def computeIntLineIndex(segmentIndex: Int): Unit = {
    val dist0 = getEdgeDistance(segmentIndex, 0)
    val dist1 = getEdgeDistance(segmentIndex, 1)
    if (dist0 > dist1) {
      intLineIndex(segmentIndex)(0) = 0
      intLineIndex(segmentIndex)(1) = 1
    } else {
      intLineIndex(segmentIndex)(0) = 1
      intLineIndex(segmentIndex)(1) = 0
    }
  }

  /**
   * Computes the "edge distance" of an intersection point along the specified input line segment.
   *
   * @param segmentIndex
   *   is 0 or 1
   * @param intIndex
   *   is 0 or 1 return the edge distance of the intersection point
   */
  def getEdgeDistance(segmentIndex: Int, intIndex: Int): Double = {
    val dist = LineIntersector.computeEdgeDistance(intPt(intIndex),
                                                   inputLines(segmentIndex)(0),
                                                   inputLines(segmentIndex)(1)
    )
    dist
  }
}
