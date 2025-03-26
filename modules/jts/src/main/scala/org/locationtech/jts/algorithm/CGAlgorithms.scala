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

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateSequence
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.Location
import org.locationtech.jts.math.MathUtil

/**
 * Specifies and implements various fundamental Computational Geometric algorithms. The algorithms
 * supplied in this class are robust for double-precision floating point.
 *
 * @version 1.7
 * @deprecated
 *   See Length, Area, Distance, Orientation, PointLocation
 */
object CGAlgorithms {

  /**
   * A value that indicates an orientation of clockwise, or a right turn.
   */
  val CLOCKWISE: Int = -1
  val RIGHT: Int     = CLOCKWISE

  /**
   * A value that indicates an orientation of counterclockwise, or a left turn.
   */
  val COUNTERCLOCKWISE = 1
  val LEFT: Int        = COUNTERCLOCKWISE

  /**
   * A value that indicates an orientation of collinear, or no turn (straight).
   */
  val COLLINEAR     = 0
  val STRAIGHT: Int = COLLINEAR

  /**
   * Returns the index of the direction of the point <code>q</code> relative to a vector specified
   * by <code>p1-p2</code>.
   *
   * @param p1
   *   the origin point of the vector
   * @param p2
   *   the final point of the vector
   * @param q
   *   the point to compute the direction to return 1 if q is counter-clockwise (left) from p1-p2
   *   return -1 if q is clockwise (right) from p1-p2 return 0 if q is collinear with p1-p2
   */
  def orientationIndex(p1: Coordinate, p2: Coordinate, q: Coordinate): Int =
    /**
     * MD - 9 Aug 2010 It seems that the basic algorithm is slightly orientation dependent, when
     * computing the orientation of a point very close to a line. This is possibly due to the
     * arithmetic in the translation to the origin.
     *
     * For instance, the following situation produces identical results in spite of the inverse
     * orientation of the line segment:
     *
     * Coordinate p0 = new Coordinate(219.3649559090992, 140.84159161824724); Coordinate p1 = new
     * Coordinate(168.9018919682399, -5.713787599646864);
     *
     * Coordinate p = new Coordinate(186.80814046338352, 46.28973405831556); int orient =
     * orientationIndex(p0, p1, p); int orientInv = orientationIndex(p1, p0, p);
     *
     * A way to force consistent results is to normalize the orientation of the vector using the
     * following code. However, this may make the results of orientationIndex inconsistent through
     * the triangle of points, so it's not clear this is an appropriate patch.
     */
    CGAlgorithmsDD.orientationIndex(p1, p2, q)
  // testing only
  // return ShewchuksDeterminant.orientationIndex(p1, p2, q);
  // previous implementation - not quite fully robust
  // return RobustDeterminant.orientationIndex(p1, p2, q);

  /**
   * Tests whether a point lies inside or on a ring. The ring may be oriented in either direction. A
   * point lying exactly on the ring boundary is considered to be inside the ring. <p> This method
   * does <i>not</i> first check the point against the envelope of the ring.
   *
   * @param p
   *   point to check for ring inclusion
   * @param ring
   *   an array of coordinates representing the ring (which must have first point identical to last
   *   point) return true if p is inside ring
   * @see
   *   locatePointInRing
   */
  def isPointInRing(p: Coordinate, ring: Array[Coordinate]): Boolean =
    locatePointInRing(p, ring) != Location.EXTERIOR

  /**
   * Determines whether a point lies in the interior, on the boundary, or in the exterior of a ring.
   * The ring may be oriented in either direction. <p> This method does <i>not</i> first check the
   * point against the envelope of the ring.
   *
   * @param p
   *   point to check for ring inclusion
   * @param ring
   *   an array of coordinates representing the ring (which must have first point identical to last
   *   point) return the {@link Location} of p relative to the ring
   */
  def locatePointInRing(p: Coordinate, ring: Array[Coordinate]): Int =
    RayCrossingCounter.locatePointInRing(p, ring)

  /**
   * Tests whether a point lies on the line segments defined by a list of coordinates.
   *
   * return true if the point is a vertex of the line or lies in the interior of a line segment in
   * the linestring
   */
  def isOnLine(p: Coordinate, pt: Array[Coordinate]): Boolean = {
    val lineIntersector = new RobustLineIntersector
    var i               = 1
    while (i < pt.length) {
      val p0 = pt(i - 1)
      val p1 = pt(i)
      lineIntersector.computeIntersection(p, p0, p1)
      if (lineIntersector.hasIntersection) return true
      i += 1
    }
    false
  }

  /**
   * Computes whether a ring defined by an array of {link Coordinate}s is oriented
   * counter-clockwise. <ul> <li>The list of points is assumed to have the first and last points
   * equal. <li>This will handle coordinate lists which contain repeated points. </ul> This
   * algorithm is <b>only</b> guaranteed to work with valid rings. If the ring is invalid (e.g.
   * self-crosses or touches), the computed result may not be correct.
   *
   * @param ring
   *   an array of Coordinates forming a ring return true if the ring is oriented counter-clockwise.
   *   throws IllegalArgumentException if there are too few points to determine orientation (&lt; 4)
   */
  def isCCW(ring: Array[Coordinate]): Boolean = { // # of points without closing endpoint
    val nPts    = ring.length - 1
    // sanity check
    if (nPts < 3)
      throw new IllegalArgumentException(
        "Ring has fewer than 4 points, so orientation cannot be determined"
      )
    // find highest point
    var hiPt    = ring(0)
    var hiIndex = 0
    var i       = 1
    while (i <= nPts) {
      val p = ring(i)
      if (p.y > hiPt.y) {
        hiPt = p
        hiIndex = i
      }
      {
        i += 1; i - 1
      }
    }
    // find distinct point before highest point
    var iPrev   = hiIndex
    while ({
      {
        iPrev = iPrev - 1
        if (iPrev < 0) iPrev = nPts
      }; ring(iPrev).equals2D(hiPt) && iPrev != hiIndex
    }) ()
    // find distinct point after highest point
    var iNext   = hiIndex
    while ({ { iNext = (iNext + 1) % nPts }; ring(iNext).equals2D(hiPt) && iNext != hiIndex }) ()
    val prev    = ring(iPrev)
    val next    = ring(iNext)

    /**
     * This check catches cases where the ring contains an A-B-A configuration of points. This can
     * happen if the ring does not contain 3 distinct points (including the case where the input
     * array has fewer than 4 elements), or it contains coincident line segments.
     */
    if (prev.equals2D(hiPt) || next.equals2D(hiPt) || prev.equals2D(next)) return false
    val disc = computeOrientation(prev, hiPt, next)

    /**
     * If disc is exactly 0, lines are collinear. There are two possible cases: (1) the lines lie
     * along the x axis in opposite directions (2) the lines lie on top of one another
     *
     * (1) is handled by checking if next is left of prev ==> CCW (2) will never happen if the ring
     * is valid, so don't check for it (Might want to assert this)
     */
    var isCCW = false
    if (disc == 0) { // poly is CCW if prev x is right of next x
      isCCW = prev.x > next.x
    } else { // if area is positive, points are ordered CCW
      isCCW = disc > 0
    }
    isCCW
  }

  /**
   * Computes the orientation of a point q to the directed line segment p1-p2. The orientation of a
   * point relative to a directed line segment indicates which way you turn to get to q after
   * travelling from p1 to p2.
   *
   * @param p1
   *   the first vertex of the line segment
   * @param p2
   *   the second vertex of the line segment
   * @param q
   *   the point to compute the relative orientation of return 1 if q is counter-clockwise from
   *   p1-p2, or -1 if q is clockwise from p1-p2, or 0 if q is collinear with p1-p2
   */
  def computeOrientation(p1: Coordinate, p2: Coordinate, q: Coordinate): Int =
    orientationIndex(p1, p2, q)

  /**
   * Computes the distance from a point p to a line segment AB
   *
   * Note: NON-ROBUST!
   *
   * @param p
   *   the point to compute the distance for
   * @param A
   *   one point of the line
   * @param B
   *   another point of the line (must be different to A) return the distance from p to line segment
   *   AB
   */
  def distancePointLine(p: Coordinate, A: Coordinate, B: Coordinate): Double = { // if start = end, then just compute distance to one of the endpoints
    if ((A.x == B.x) && (A.y == B.y)) return p.distance(A)
    // otherwise use comp.graphics.algorithms Frequently Asked Questions method
    /*
     * (1) r = AC dot AB
     *         ---------
     *         ||AB||^2
     *
     * r has the following meaning:
     *   r=0 P = A
     *   r=1 P = B
     *   r<0 P is on the backward extension of AB
     *   r>1 P is on the forward extension of AB
     *   0<r<1 P is interior to AB
     */
    val len2 = (B.x - A.x) * (B.x - A.x) + (B.y - A.y) * (B.y - A.y)
    val r    = ((p.x - A.x) * (B.x - A.x) + (p.y - A.y) * (B.y - A.y)) / len2
    if (r <= 0.0) return p.distance(A)
    if (r >= 1.0) return p.distance(B)
    /*
     * (2) s = (Ay-Cy)(Bx-Ax)-(Ax-Cx)(By-Ay)
     *         -----------------------------
     *                    L^2
     *
     * Then the distance from C to P = |s|*L.
     *
     * This is the same calculation as {link #distancePointLinePerpendicular}.
     * Unrolled here for performance.
     */
    val s    = ((A.y - p.y) * (B.x - A.x) - (A.x - p.x) * (B.y - A.y)) / len2
    Math.abs(s) * Math.sqrt(len2)
  }

  /**
   * Computes the perpendicular distance from a point p to the (infinite) line containing the points
   * AB
   *
   * @param p
   *   the point to compute the distance for
   * @param A
   *   one point of the line
   * @param B
   *   another point of the line (must be different to A) return the distance from p to line AB
   */
  def distancePointLinePerpendicular(p: Coordinate, A: Coordinate, B: Coordinate): Double = { // use comp.graphics.algorithms Frequently Asked Questions method
    /*
     * (2) s = (Ay-Cy)(Bx-Ax)-(Ax-Cx)(By-Ay)
     *         -----------------------------
     *                    L^2
     *
     * Then the distance from C to P = |s|*L.
     */
    val len2 = (B.x - A.x) * (B.x - A.x) + (B.y - A.y) * (B.y - A.y)
    val s    = ((A.y - p.y) * (B.x - A.x) - (A.x - p.x) * (B.y - A.y)) / len2
    Math.abs(s) * Math.sqrt(len2)
  }

  /**
   * Computes the distance from a point to a sequence of line segments.
   *
   * @param p
   *   a point
   * @param line
   *   a sequence of contiguous line segments defined by their vertices return the minimum distance
   *   between the point and the line segments
   */
  def distancePointLine(p: Coordinate, line: Array[Coordinate]): Double = {
    if (line.length == 0)
      throw new IllegalArgumentException("Line array must contain at least one vertex")
    // this handles the case of length = 1
    var minDistance = p.distance(line(0))
    var i           = 0
    while (i < line.length - 1) {
      val dist = distancePointLine(p, line(i), line(i + 1))
      if (dist < minDistance) minDistance = dist
      i += 1
    }
    minDistance
  }

  /**
   * Computes the distance from a line segment AB to a line segment CD
   *
   * Note: NON-ROBUST!
   *
   * @param A
   *   a point of one line
   * @param B
   *   the second point of (must be different to A)
   * @param C
   *   one point of the line
   * @param D
   *   another point of the line (must be different to A)
   */
  def distanceLineLine(A: Coordinate, B: Coordinate, C: Coordinate, D: Coordinate): Double = { // check for zero-length segments
    if (A == B) return distancePointLine(A, C, D)
    if (C == D) return distancePointLine(D, A, B)
    // AB and CD are line segments
    /*
     * from comp.graphics.algo
     *
     * Solving the above for r and s yields
     *
     *     (Ay-Cy)(Dx-Cx)-(Ax-Cx)(Dy-Cy)
     * r = ----------------------------- (eqn 1)
     *     (Bx-Ax)(Dy-Cy)-(By-Ay)(Dx-Cx)
     *
     *     (Ay-Cy)(Bx-Ax)-(Ax-Cx)(By-Ay)
     * s = ----------------------------- (eqn 2)
     *     (Bx-Ax)(Dy-Cy)-(By-Ay)(Dx-Cx)
     *
     * Let P be the position vector of the
     * intersection point, then
     *   P=A+r(B-A) or
     *   Px=Ax+r(Bx-Ax)
     *   Py=Ay+r(By-Ay)
     * By examining the values of r & s, you can also determine some other limiting
     * conditions:
     *   If 0<=r<=1 & 0<=s<=1, intersection exists
     *      r<0 or r>1 or s<0 or s>1 line segments do not intersect
     *   If the denominator in eqn 1 is zero, AB & CD are parallel
     *   If the numerator in eqn 1 is also zero, AB & CD are collinear.
     */
    var noIntersection = false
    if (!Envelope.intersects(A, B, C, D)) noIntersection = true
    else {
      val denom = (B.x - A.x) * (D.y - C.y) - (B.y - A.y) * (D.x - C.x)
      if (denom == 0) noIntersection = true
      else {
        val r_num = (A.y - C.y) * (D.x - C.x) - (A.x - C.x) * (D.y - C.y)
        val s_num = (A.y - C.y) * (B.x - A.x) - (A.x - C.x) * (B.y - A.y)
        val s     = s_num / denom
        val r     = r_num / denom
        if ((r < 0) || (r > 1) || (s < 0) || (s > 1)) noIntersection = true
      }
    }
    if (noIntersection)
      return MathUtil.min(distancePointLine(A, C, D),
                          distancePointLine(B, C, D),
                          distancePointLine(C, A, B),
                          distancePointLine(D, A, B)
      )
    // segments intersect
    0.0
  }

  /**
   * Computes the signed area for a ring. The signed area is positive if the ring is oriented CW,
   * negative if the ring is oriented CCW, and zero if the ring is degenerate or flat.
   *
   * @param ring
   *   the coordinates forming the ring return the signed area of the ring
   */
  def signedArea(ring: Array[Coordinate]): Double = {
    if (ring.length < 3) return 0.0
    var sum = 0.0

    /**
     * Based on the Shoelace formula. http://en.wikipedia.org/wiki/Shoelace_formula
     */
    val x0 = ring(0).x
    var i  = 1
    while (i < ring.length - 1) {
      val x  = ring(i).x - x0
      val y1 = ring(i + 1).y
      val y2 = ring(i - 1).y
      sum += x * (y2 - y1)
      i += 1
    }
    sum / 2.0
  }

  /**
   * Computes the signed area for a ring. The signed area is: <ul> <li>positive if the ring is
   * oriented CW <li>negative if the ring is oriented CCW <li>zero if the ring is degenerate or flat
   * </ul>
   *
   * @param ring
   *   the coordinates forming the ring return the signed area of the ring
   */
  def signedArea(ring: CoordinateSequence): Double = {
    val n   = ring.size
    if (n < 3) return 0.0
    val p0  = new Coordinate
    val p1  = new Coordinate
    val p2  = new Coordinate
    ring.getCoordinate(0, p1)
    ring.getCoordinate(1, p2)
    val x0  = p1.x
    p2.x -= x0
    var sum = 0.0
    var i   = 1
    while (i < n - 1) {
      p0.y = p1.y
      p1.x = p2.x
      p1.y = p2.y
      ring.getCoordinate(i + 1, p2)
      p2.x -= x0
      sum += p1.x * (p0.y - p2.y)
      i += 1
    }
    sum / 2.0
  }

  /**
   * Computes the length of a linestring specified by a sequence of points.
   *
   * @param pts
   *   the points specifying the linestring return the length of the linestring
   */
  def length(pts: CoordinateSequence): Double = { // optimized for processing CoordinateSequences
    val n   = pts.size
    if (n <= 1) return 0.0
    var len = 0.0
    val p   = new Coordinate
    pts.getCoordinate(0, p)
    var x0  = p.x
    var y0  = p.y
    var i   = 1
    while (i < n) {
      pts.getCoordinate(i, p)
      val x1 = p.x
      val y1 = p.y
      val dx = x1 - x0
      val dy = y1 - y0
      len += Math.sqrt(dx * dx + dy * dy)
      x0 = x1
      y0 = y1
      i += 1
    }
    len
  }
}

class CGAlgorithms() {}
