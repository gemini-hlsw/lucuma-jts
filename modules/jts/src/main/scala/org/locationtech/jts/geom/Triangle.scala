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
package org.locationtech.jts.geom

import org.locationtech.jts.algorithm.Angle
import org.locationtech.jts.algorithm.HCoordinate
import org.locationtech.jts.math.DD

/**
 * Represents a planar triangle, and provides methods for calculating various properties of
 * triangles.
 *
 * @version 1.7
 */
object Triangle {

  /**
   * Tests whether a triangle is acute. A triangle is acute iff all interior angles are acute. This
   * is a strict test - right triangles will return <tt>false</tt> A triangle which is not acute is
   * either right or obtuse. <p> Note: this implementation is not robust for angles very close to 90
   * degrees.
   *
   * @param a
   *   a vertex of the triangle
   * @param b
   *   a vertex of the triangle
   * @param c
   *   a vertex of the triangle return true if the triangle is acute
   */
  def isAcute(a: Coordinate, b: Coordinate, c: Coordinate): Boolean = {
    if (!Angle.isAcute(a, b, c)) return false
    if (!Angle.isAcute(b, c, a)) return false
    if (!Angle.isAcute(c, a, b)) return false
    true
  }

  /**
   * Computes the line which is the perpendicular bisector of the line segment a-b.
   *
   * @param a
   *   a point
   * @param b
   *   another point return the perpendicular bisector, as an HCoordinate
   */
  def perpendicularBisector(a: Coordinate, b: Coordinate) = { // returns the perpendicular bisector of the line segment ab
    val dx = b.x - a.x
    val dy = b.y - a.y
    val l1 = new HCoordinate(a.x + dx / 2.0, a.y + dy / 2.0, 1.0)
    val l2 = new HCoordinate(a.x - dy + dx / 2.0, a.y + dx + dy / 2.0, 1.0)
    new HCoordinate(l1, l2)
  }

  /**
   * Computes the circumcentre of a triangle. The circumcentre is the centre of the circumcircle,
   * the smallest circle which encloses the triangle. It is also the common intersection point of
   * the perpendicular bisectors of the sides of the triangle, and is the only point which has equal
   * distance to all three vertices of the triangle. <p> The circumcentre does not necessarily lie
   * within the triangle. For example, the circumcentre of an obtuse isosceles triangle lies outside
   * the triangle. <p> This method uses an algorithm due to J.R.Shewchuk which uses normalization to
   * the origin to improve the accuracy of computation. (See <i>Lecture Notes on Geometric
   * Robustness</i>, Jonathan Richard Shewchuk, 1999).
   *
   * @param a
   *   a vertex of the triangle
   * @param b
   *   a vertex of the triangle
   * @param c
   *   a vertex of the triangle return the circumcentre of the triangle
   */
  def circumcentre(a: Coordinate, b: Coordinate, c: Coordinate) = {
    val cx    = c.x
    val cy    = c.y
    val ax    = a.x - cx
    val ay    = a.y - cy
    val bx    = b.x - cx
    val by    = b.y - cy
    val denom = 2 * det(ax, ay, bx, by)
    val numx  = det(ay, ax * ax + ay * ay, by, bx * bx + by * by)
    val numy  = det(ax, ax * ax + ay * ay, bx, bx * bx + by * by)
    val ccx   = cx - numx / denom
    val ccy   = cy + numy / denom
    new Coordinate(ccx, ccy)
  }

  /**
   * Computes the circumcentre of a triangle. The circumcentre is the centre of the circumcircle,
   * the smallest circle which encloses the triangle. It is also the common intersection point of
   * the perpendicular bisectors of the sides of the triangle, and is the only point which has equal
   * distance to all three vertices of the triangle. <p> The circumcentre does not necessarily lie
   * within the triangle. For example, the circumcentre of an obtuse isosceles triangle lies outside
   * the triangle. <p> This method uses {link DD} extended-precision arithmetic to provide more
   * accurate results than {link #circumcentre(Coordinate, Coordinate, Coordinate)}
   *
   * @param a
   *   a vertex of the triangle
   * @param b
   *   a vertex of the triangle
   * @param c
   *   a vertex of the triangle return the circumcentre of the triangle
   */
  def circumcentreDD(a: Coordinate, b: Coordinate, c: Coordinate) = {
    val ax    = DD.valueOf(a.x).subtract(c.x)
    val ay    = DD.valueOf(a.y).subtract(c.y)
    val bx    = DD.valueOf(b.x).subtract(c.x)
    val by    = DD.valueOf(b.y).subtract(c.y)
    val denom = DD.determinant(ax, ay, bx, by).multiply(2)
    val asqr  = ax.sqr.add(ay.sqr)
    val bsqr  = bx.sqr.add(by.sqr)
    val numx  = DD.determinant(ay, asqr, by, bsqr)
    val numy  = DD.determinant(ax, asqr, bx, bsqr)
    val ccx   = DD.valueOf(c.x).subtract(numx.divide(denom)).doubleValue
    val ccy   = DD.valueOf(c.y).add(numy.divide(denom)).doubleValue
    new Coordinate(ccx, ccy)
  }

  /**
   * Computes the determinant of a 2x2 matrix. Uses standard double-precision arithmetic, so is
   * susceptible to round-off error.
   *
   * @param m00
   *   the [0,0] entry of the matrix
   * @param m01
   *   the [0,1] entry of the matrix
   * @param m10
   *   the [1,0] entry of the matrix
   * @param m11
   *   the [1,1] entry of the matrix return the determinant
   */
  private def det(m00: Double, m01: Double, m10: Double, m11: Double) = m00 * m11 - m01 * m10

  /**
   * Computes the incentre of a triangle. The <i>inCentre</i> of a triangle is the point which is
   * equidistant from the sides of the triangle. It is also the point at which the bisectors of the
   * triangle's angles meet. It is the centre of the triangle's <i>incircle</i>, which is the unique
   * circle that is tangent to each of the triangle's three sides. <p> The incentre always lies
   * within the triangle.
   *
   * @param a
   *   a vertex of the triangle
   * @param b
   *   a vertex of the triangle
   * @param c
   *   a vertex of the triangle return the point which is the incentre of the triangle
   */
  def inCentre(a: Coordinate, b: Coordinate, c: Coordinate) = { // the lengths of the sides, labelled by their opposite vertex
    val len0      = b.distance(c)
    val len1      = a.distance(c)
    val len2      = a.distance(b)
    val circum    = len0 + len1 + len2
    val inCentreX = (len0 * a.x + len1 * b.x + len2 * c.x) / circum
    val inCentreY = (len0 * a.y + len1 * b.y + len2 * c.y) / circum
    new Coordinate(inCentreX, inCentreY)
  }

  /**
   * Computes the centroid (centre of mass) of a triangle. This is also the point at which the
   * triangle's three medians intersect (a triangle median is the segment from a vertex of the
   * triangle to the midpoint of the opposite side). The centroid divides each median in a ratio of
   * 2:1. <p> The centroid always lies within the triangle.
   *
   * @param a
   *   a vertex of the triangle
   * @param b
   *   a vertex of the triangle
   * @param c
   *   a vertex of the triangle return the centroid of the triangle
   */
  def centroid(a: Coordinate, b: Coordinate, c: Coordinate) = {
    val x = (a.x + b.x + c.x) / 3
    val y = (a.y + b.y + c.y) / 3
    new Coordinate(x, y)
  }

  /**
   * Computes the length of the longest side of a triangle
   *
   * @param a
   *   a vertex of the triangle
   * @param b
   *   a vertex of the triangle
   * @param c
   *   a vertex of the triangle return the length of the longest side of the triangle
   */
  def longestSideLength(a: Coordinate, b: Coordinate, c: Coordinate) = {
    val lenAB  = a.distance(b)
    val lenBC  = b.distance(c)
    val lenCA  = c.distance(a)
    var maxLen = lenAB
    if (lenBC > maxLen) maxLen = lenBC
    if (lenCA > maxLen) maxLen = lenCA
    maxLen
  }

  /**
   * Computes the point at which the bisector of the angle ABC cuts the segment AC.
   *
   * @param a
   *   a vertex of the triangle
   * @param b
   *   a vertex of the triangle
   * @param c
   *   a vertex of the triangle return the angle bisector cut point
   */
  def angleBisector(a: Coordinate, b: Coordinate, c: Coordinate) = {

    /**
     * Uses the fact that the lengths of the parts of the split segment are proportional to the
     * lengths of the adjacent triangle sides
     */
    val len0    = b.distance(a)
    val len2    = b.distance(c)
    val frac    = len0 / (len0 + len2)
    val dx      = c.x - a.x
    val dy      = c.y - a.y
    val splitPt = new Coordinate(a.x + frac * dx, a.y + frac * dy)
    splitPt
  }

  /**
   * Computes the 2D area of a triangle. The area value is always non-negative.
   *
   * @param a
   *   a vertex of the triangle
   * @param b
   *   a vertex of the triangle
   * @param c
   *   a vertex of the triangle return the area of the triangle
   * @see
   *   #signedArea(Coordinate, Coordinate, Coordinate)
   */
  def area(a: Coordinate, b: Coordinate, c: Coordinate) =
    Math.abs(((c.x - a.x) * (b.y - a.y) - (b.x - a.x) * (c.y - a.y)) / 2)

  /**
   * Computes the signed 2D area of a triangle. The area value is positive if the triangle is
   * oriented CW, and negative if it is oriented CCW. <p> The signed area value can be used to
   * determine point orientation, but the implementation in this method is susceptible to round-off
   * errors. Use {link Orientation#index(Coordinate, Coordinate, Coordinate)} for robust orientation
   * calculation.
   *
   * @param a
   *   a vertex of the triangle
   * @param b
   *   a vertex of the triangle
   * @param c
   *   a vertex of the triangle return the signed 2D area of the triangle
   * @see
   *   Orientation#index(Coordinate, Coordinate, Coordinate)
   */
  def signedArea(a: Coordinate, b: Coordinate, c: Coordinate) =
    /**
     * Uses the formula 1/2 * | u x v | where u,v are the side vectors of the triangle x is the
     * vector cross-product For 2D vectors, this formula simplifies to the expression below
     */
    ((c.x - a.x) * (b.y - a.y) - (b.x - a.x) * (c.y - a.y)) / 2

  /**
   * Computes the 3D area of a triangle. The value computed is always non-negative.
   *
   * @param a
   *   a vertex of the triangle
   * @param b
   *   a vertex of the triangle
   * @param c
   *   a vertex of the triangle return the 3D area of the triangle
   */
  def area3D(a: Coordinate, b: Coordinate, c: Coordinate) = {

    /**
     * Uses the formula 1/2 * | u x v | where u,v are the side vectors of the triangle x is the
     * vector cross-product
     */
    // side vectors u and v
    val ux     = b.x - a.x
    val uy     = b.y - a.y
    val uz     = b.getZ - a.getZ
    val vx     = c.x - a.x
    val vy     = c.y - a.y
    val vz     = c.getZ - a.getZ
    // cross-product = u x v
    val crossx = uy * vz - uz * vy
    val crossy = uz * vx - ux * vz
    val crossz = ux * vy - uy * vx
    // tri area = 1/2 * | u x v |
    val absSq  = crossx * crossx + crossy * crossy + crossz * crossz
    val area3D = Math.sqrt(absSq) / 2
    area3D
  }

  /**
   * Computes the Z-value (elevation) of an XY point on a three-dimensional plane defined by a
   * triangle whose vertices have Z-values. The defining triangle must not be degenerate (in other
   * words, the triangle must enclose a non-zero area), and must not be parallel to the Z-axis. <p>
   * This method can be used to interpolate the Z-value of a point inside a triangle (for example,
   * of a TIN facet with elevations on the vertices).
   *
   * @param p
   *   the point to compute the Z-value of
   * @param v0
   *   a vertex of a triangle, with a Z ordinate
   * @param v1
   *   a vertex of a triangle, with a Z ordinate
   * @param v2
   *   a vertex of a triangle, with a Z ordinate return the computed Z-value (elevation) of the
   *   point
   */
  def interpolateZ(p: Coordinate, v0: Coordinate, v1: Coordinate, v2: Coordinate) = {
    val x0  = v0.x
    val y0  = v0.y
    val a   = v1.x - x0
    val b   = v2.x - x0
    val c   = v1.y - y0
    val d   = v2.y - y0
    val det = a * d - b * c
    val dx  = p.x - x0
    val dy  = p.y - y0
    val t   = (d * dx - b * dy) / det
    val u   = (-c * dx + a * dy) / det
    val z   = v0.getZ + t * (v1.getZ - v0.getZ) + u * (v2.getZ - v0.getZ)
    z
  }
}

class Triangle(
  /**
   * The coordinates of the vertices of the triangle
   */
  var p0: Coordinate,
  var p1: Coordinate,
  var p2: Coordinate
) {

  /**
   * Creates a new triangle with the given vertices.
   *
   * @param p0
   *   a vertex
   * @param p1
   *   a vertex
   * @param p2
   *   a vertex
   */
  /**
   * Computes the incentre of this triangle. The <i>incentre</i> of a triangle is the point which is
   * equidistant from the sides of the triangle. It is also the point at which the bisectors of the
   * triangle's angles meet. It is the centre of the triangle's <i>incircle</i>, which is the unique
   * circle that is tangent to each of the triangle's three sides.
   *
   * return the point which is the inCentre of this triangle
   */
  def inCentre = Triangle.inCentre(p0, p1, p2)

  /**
   * Tests whether this triangle is acute. A triangle is acute iff all interior angles are acute.
   * This is a strict test - right triangles will return <tt>false</tt> A triangle which is not
   * acute is either right or obtuse. <p> Note: this implementation is not robust for angles very
   * close to 90 degrees.
   *
   * return true if this triangle is acute
   */
  def isAcute = Triangle.isAcute(this.p0, this.p1, this.p2)

  /**
   * Computes the circumcentre of this triangle. The circumcentre is the centre of the circumcircle,
   * the smallest circle which encloses the triangle. It is also the common intersection point of
   * the perpendicular bisectors of the sides of the triangle, and is the only point which has equal
   * distance to all three vertices of the triangle. <p> The circumcentre does not necessarily lie
   * within the triangle. <p> This method uses an algorithm due to J.R.Shewchuk which uses
   * normalization to the origin to improve the accuracy of computation. (See <i>Lecture Notes on
   * Geometric Robustness</i>, Jonathan Richard Shewchuk, 1999).
   *
   * return the circumcentre of this triangle
   */
  def circumcentre = Triangle.circumcentre(this.p0, this.p1, this.p2)

  /**
   * Computes the centroid (centre of mass) of this triangle. This is also the point at which the
   * triangle's three medians intersect (a triangle median is the segment from a vertex of the
   * triangle to the midpoint of the opposite side). The centroid divides each median in a ratio of
   * 2:1. <p> The centroid always lies within the triangle.
   *
   * return the centroid of this triangle
   */
  def centroid = Triangle.centroid(this.p0, this.p1, this.p2)

  /**
   * Computes the length of the longest side of this triangle
   *
   * return the length of the longest side of this triangle
   */
  def longestSideLength = Triangle.longestSideLength(this.p0, this.p1, this.p2)

  /**
   * Computes the 2D area of this triangle. The area value is always non-negative.
   *
   * return the area of this triangle
   * @see
   *   #signedArea()
   */
  def area = Triangle.area(this.p0, this.p1, this.p2)

  /**
   * Computes the signed 2D area of this triangle. The area value is positive if the triangle is
   * oriented CW, and negative if it is oriented CCW. <p> The signed area value can be used to
   * determine point orientation, but the implementation in this method is susceptible to round-off
   * errors. Use {link Orientation#index(Coordinate, Coordinate, Coordinate)} for robust orientation
   * calculation.
   *
   * return the signed 2D area of this triangle
   * @see
   *   Orientation#index(Coordinate, Coordinate, Coordinate)
   */
  def signedArea = Triangle.signedArea(this.p0, this.p1, this.p2)

  /**
   * Computes the 3D area of this triangle. The value computed is always non-negative.
   *
   * return the 3D area of this triangle
   */
  def area3D = Triangle.area3D(this.p0, this.p1, this.p2)

  /**
   * Computes the Z-value (elevation) of an XY point on a three-dimensional plane defined by this
   * triangle (whose vertices must have Z-values). This triangle must not be degenerate (in other
   * words, the triangle must enclose a non-zero area), and must not be parallel to the Z-axis. <p>
   * This method can be used to interpolate the Z-value of a point inside this triangle (for
   * example, of a TIN facet with elevations on the vertices).
   *
   * @param p
   *   the point to compute the Z-value of return the computed Z-value (elevation) of the point
   */
  def interpolateZ(p: Coordinate) = {
    if (p == null) throw new IllegalArgumentException("Supplied point is null.")
    Triangle.interpolateZ(p, this.p0, this.p1, this.p2)
  }
}
