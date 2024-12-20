// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2016 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */ /*
 * Copyright (c) 2016 Martin Davis.
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
import org.locationtech.jts.math.DD

/**
 * Implements basic computational geometry algorithms using {link DD} arithmetic.
 *
 * @author
 *   Martin Davis
 */
object CGAlgorithmsDD {

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
    return orientationIndex(p1.x, p1.y, p2.x, p2.y, q.x, q.y);

  /**
   * Returns the index of the direction of the point <code>q</code> relative to a vector specified
   * by <code>p1-p2</code>.
   *
   * @param p1x
   *   the x ordinate of the vector origin point
   * @param p1y
   *   the y ordinate of the vector origin point
   * @param p2x
   *   the x ordinate of the vector final point
   * @param p2y
   *   the y ordinate of the vector final point
   * @param qx
   *   the x ordinate of the query point
   * @param qy
   *   the y ordinate of the query point
   *
   * @return
   *   1 if q is counter-clockwise (left) from p1-p2
   * @return
   *   -1 if q is clockwise (right) from p1-p2
   * @return
   *   0 if q is collinear with p1-p2
   */
  def orientationIndex(
    p1x: Double,
    p1y: Double,
    p2x: Double,
    p2y: Double,
    qx:  Double,
    qy:  Double
  ): Int = {
    val index = orientationIndexFilter(p1x, p1y, p2x, p2y, qx, qy)
    if (index <= 1) return index
    // normalize coordinates
    val dx1   = DD.valueOf(p2x).selfAdd(-p1x)
    val dy1   = DD.valueOf(p2y).selfAdd(-p1y)
    val dx2   = DD.valueOf(qx).selfAdd(-p2x)
    val dy2   = DD.valueOf(qy).selfAdd(-p2y)
    // sign of determinant - unrolled for performance
    dx1.selfMultiply(dy2).selfSubtract(dy1.selfMultiply(dx2)).signum
  }

  /**
   * Computes the sign of the determinant of the 2x2 matrix with the given entries.
   *
   * return -1 if the determinant is negative, return 1 if the determinant is positive, return 0 if
   * the determinant is 0.
   */
  def signOfDet2x2(x1: DD, y1: DD, x2: DD, y2: DD): Int = {
    val det = x1.multiply(y2).selfSubtract(y1.multiply(x2))
    det.signum
  }

  def signOfDet2x2(dx1: Double, dy1: Double, dx2: Double, dy2: Double): Int = {
    val x1  = DD.valueOf(dx1)
    val y1  = DD.valueOf(dy1)
    val x2  = DD.valueOf(dx2)
    val y2  = DD.valueOf(dy2)
    val det = x1.multiply(y2).selfSubtract(y1.multiply(x2))
    det.signum
  }

  /**
   * A value which is safely greater than the relative round-off error in double-precision numbers
   */
  private val DP_SAFE_EPSILON = 1e-15

  /**
   * A filter for computing the orientation index of three coordinates. <p> If the orientation can
   * be computed safely using standard DP arithmetic, this routine returns the orientation index.
   * Otherwise, a value i > 1 is returned. In this case the orientation index must be computed using
   * some other more robust method. The filter is fast to compute, so can be used to avoid the use
   * of slower robust methods except when they are really needed, thus providing better average
   * performance. <p> Uses an approach due to Jonathan Shewchuk, which is in the public domain.
   *
   * @param pa
   *   a coordinate
   * @param pb
   *   a coordinate
   * @param pc
   *   a coordinate return the orientation index if it can be computed safely return i > 1 if the
   *   orientation index cannot be computed safely
   */
  def orientationIndexFilter(
    pax: Double,
    pay: Double,
    pbx: Double,
    pby: Double,
    pcx: Double,
    pcy: Double
  ): Int = {
    var detsum   = .0
    val detleft  = (pax - pcx) * (pby - pcy)
    val detright = (pay - pcy) * (pbx - pcx)
    val det      = detleft - detright
    if (detleft > 0.0)
      if (detright <= 0.0) return signum(det)
      else detsum = detleft + detright
    else if (detleft < 0.0)
      if (detright >= 0.0) return signum(det)
      else detsum = -detleft - detright
    else return signum(det)
    val errbound = DP_SAFE_EPSILON * detsum
    if ((det >= errbound) || (-det >= errbound)) return signum(det)
    2
  }

  private def signum(x: Double): Int = {
    if (x > 0) return 1
    if (x < 0) return -1
    0
  }

  /**
   * Computes an intersection point between two lines using DD arithmetic. If the lines are parallel
   * (either identical or separate) a null value is returned.
   *
   * @param p1
   *   an endpoint of line segment 1
   * @param p2
   *   an endpoint of line segment 1
   * @param q1
   *   an endpoint of line segment 2
   * @param q2
   *   an endpoint of line segment 2 return an intersection point if one exists, or null if the
   *   lines are parallel
   */
  def intersection(p1: Coordinate, p2: Coordinate, q1: Coordinate, q2: Coordinate): Coordinate = {
    val px   = new DD(p1.y).selfSubtract(p2.y)
    val py   = new DD(p2.x).selfSubtract(p1.x)
    val pw   = new DD(p1.x).selfMultiply(p2.y).selfSubtract(new DD(p2.x).selfMultiply(p1.y))
    val qx   = new DD(q1.y).selfSubtract(q2.y)
    val qy   = new DD(q2.x).selfSubtract(q1.x)
    val qw   = new DD(q1.x).selfMultiply(q2.y).selfSubtract(new DD(q2.x).selfMultiply(q1.y))
    val x    = py.multiply(qw).selfSubtract(qy.multiply(pw))
    val y    = qx.multiply(pw).selfSubtract(px.multiply(qw))
    val w    = px.multiply(qy).selfSubtract(qx.multiply(py))
    val xInt = x.selfDivide(w).doubleValue
    val yInt = y.selfDivide(w).doubleValue
    if (
      java.lang.Double.isNaN(xInt) || (java.lang.Double
        .isInfinite(xInt) || java.lang.Double.isNaN(yInt)) || java.lang.Double.isInfinite(yInt)
    ) return null
    new Coordinate(xInt, yInt)
  }
}
