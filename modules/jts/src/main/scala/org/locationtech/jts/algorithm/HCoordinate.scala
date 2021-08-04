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
package org.locationtech.jts.algorithm

import org.locationtech.jts.geom.Coordinate

/**
 * Represents a homogeneous coordinate in a 2-D coordinate space.
 * In JTS {link HCoordinate}s are used as a clean way
 * of computing intersections between line segments.
 *
 * @author David Skea
 * @version 1.7
 */
object HCoordinate {

  /**
   * Computes the (approximate) intersection point between two line segments
   * using homogeneous coordinates.
   * <p>
   * Note that this algorithm is
   * not numerically stable; i.e. it can produce intersection points which
   * lie outside the envelope of the line segments themselves.  In order
   * to increase the precision of the calculation input points should be normalized
   * before passing them to this routine.
   *
   * @deprecated use { @link Intersection#intersection(Coordinate, Coordinate, Coordinate, Coordinate)}
   */
  @throws[NotRepresentableException]
  def intersection(p1: Coordinate, p2: Coordinate, q1: Coordinate, q2: Coordinate): Coordinate = { // unrolled computation
    val px   = p1.y - p2.y
    val py   = p2.x - p1.x
    val pw   = p1.x * p2.y - p2.x * p1.y
    val qx   = q1.y - q2.y
    val qy   = q2.x - q1.x
    val qw   = q1.x * q2.y - q2.x * q1.y
    val x    = py * qw - qy * pw
    val y    = qx * pw - px * qw
    val w    = px * qy - qx * py
    val xInt = x / w
    val yInt = y / w
    if (
      java.lang.Double.isNaN(xInt) || (java.lang.Double
        .isInfinite(xInt) || java.lang.Double.isNaN(yInt)) || java.lang.Double.isInfinite(yInt)
    ) throw new NotRepresentableException
    new Coordinate(xInt, yInt)
  }

  private def xArg(p1: Coordinate, p2: Coordinate, q1: Coordinate, q2: Coordinate): Double = {
    val py = p2.x - p1.x
    val qw = q1.x * q2.y - q2.x * q1.y
    val qy = q2.x - q1.x
    val pw = p1.x * p2.y - p2.x * p1.y
    py * qw - qy * pw
  }

  private def yArg(p1: Coordinate, p2: Coordinate, q1: Coordinate, q2: Coordinate): Double = {
    val px = p1.y - p2.y
    val pw = p1.x * p2.y - p2.x * p1.y

    val qx = q1.y - q2.y
    val qw = q1.x * q2.y - q2.x * q1.y
    qx * pw - px * qw
  }

  private def zArg(p1: Coordinate, p2: Coordinate, q1: Coordinate, q2: Coordinate): Double = {
    val px = p1.y - p2.y
    val py = p2.x - p1.x

    val qx = q1.y - q2.y
    val qy = q2.x - q1.x
    px * qy - qx * py
  }
}

class HCoordinate(val x: Double = 0.0, val y: Double = 0.0, val w: Double = 1.0) {

  def this(_x: Double, _y: Double) = {
    this(_x, _y, 1.0)
  }

  def this(p: Coordinate) = {
    this(p.x, p.y, 1.0)
  }

  def this(p1: HCoordinate, p2: HCoordinate) = {
    this(p1.y * p2.w - p2.y * p1.w, p2.x * p1.w - p1.x * p2.w, p1.x * p2.y - p2.x * p1.y)
  }

  /**
   * Constructs a homogeneous coordinate which is the intersection of the lines
   * define by the homogenous coordinates represented by two
   * {link Coordinate}s.
   *
   * @param p1
   * @param p2
   */
  def this(p1: Coordinate, p2: Coordinate) = {
    // optimization when it is known that w = 1
    this(p1.y - p2.y, p2.x - p1.x, p1.x * p2.y - p2.x * p1.y)
  }

  def this(p1: Coordinate, p2: Coordinate, q1: Coordinate, q2: Coordinate) = {
    this(HCoordinate.xArg(p1, p2, q1, q2),
         HCoordinate.yArg(p1, p2, q1, q2),
         HCoordinate.zArg(p1, p2, q1, q2)
    )
  }

  @throws[NotRepresentableException]
  def getX: Double = {
    val a = x / w
    if (java.lang.Double.isNaN(a) || java.lang.Double.isInfinite(a))
      throw new NotRepresentableException
    a
  }

  @throws[NotRepresentableException]
  def getY: Double = {
    val a = y / w
    if (java.lang.Double.isNaN(a) || java.lang.Double.isInfinite(a))
      throw new NotRepresentableException
    a
  }

  @throws[NotRepresentableException]
  def getCoordinate: Coordinate = {
    val p = new Coordinate
    p.x = getX
    p.y = getY
    p
  }
}
