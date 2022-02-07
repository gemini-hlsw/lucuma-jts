// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */ // Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */
package org.locationtech.jts.algorithm

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.math.MathUtil

/**
 * Functions to compute distance between basic geometric structures.
 *
 * @author
 *   Martin Davis
 */
object Distance {

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
  def segmentToSegment(A: Coordinate, B: Coordinate, C: Coordinate, D: Coordinate): Double = { // check for zero-length segments
    if (A == B) return Distance.pointToSegment(A, C, D)
    if (C == D) return Distance.pointToSegment(D, A, B)
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
      return MathUtil.min(Distance.pointToSegment(A, C, D),
                          Distance.pointToSegment(B, C, D),
                          Distance.pointToSegment(C, A, B),
                          Distance.pointToSegment(D, A, B)
      )
    // segments intersect
    0.0
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
  def pointToSegmentString(p: Coordinate, line: Array[Coordinate]) = {
    if (line.length == 0)
      throw new IllegalArgumentException("Line array must contain at least one vertex")
    // this handles the case of length = 1
    var minDistance = p.distance(line(0))
    var i           = 0
    while (i < line.length - 1) {
      val dist = Distance.pointToSegment(p, line(i), line(i + 1))
      if (dist < minDistance) minDistance = dist
      i += 1
    }
    minDistance
  }

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
  def pointToSegment(p: Coordinate, A: Coordinate, B: Coordinate): Double = { // if start = end, then just compute distance to one of the endpoints
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
  def pointToLinePerpendicular(p: Coordinate, A: Coordinate, B: Coordinate) = { // use comp.graphics.algorithms Frequently Asked Questions method
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
}
