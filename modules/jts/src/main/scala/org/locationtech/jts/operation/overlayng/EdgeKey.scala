// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.operation.overlayng

import org.locationtech.jts.geom.Coordinate

/*
 * Copyright (c) 2019 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v20.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */

/**
 * A key for sorting and comparing edges in a noded arrangement. Relies on the fact that in a
 * correctly noded arrangement edges are identical (up to direction) iff they have their first
 * segment in common.
 *
 * @author
 *   mdavis
 */
object EdgeKey {
  def create(edge: Edge) = new EdgeKey(edge)

  /**
   * Computes a hash code for a double value, using the algorithm from Joshua Bloch's book
   * <i>Effective Java"</i>
   *
   * @param x
   *   the value to compute for
   * @return
   *   a hashcode for x
   */
  def hashCode(x: Double): Int = {
    val f = java.lang.Double.doubleToLongBits(x)
    (f ^ (f >>> 32)).toInt
  }
}

class EdgeKey private[overlayng] (val edge: Edge) extends Comparable[EdgeKey] {
  private var p0x = .0
  private var p0y = .0
  private var p1x = .0
  private var p1y = .0
  initPoints(edge)

  private def initPoints(edge: Edge): Unit = {
    val direction = edge.direction
    if (direction) init(edge.getCoordinate(0), edge.getCoordinate(1))
    else {
      val len = edge.size
      init(edge.getCoordinate(len - 1), edge.getCoordinate(len - 2))
    }
  }

  private def init(p0: Coordinate, p1: Coordinate): Unit = {
    p0x = p0.getX
    p0y = p0.getY
    p1x = p1.getX
    p1y = p1.getY
  }

  override def compareTo(ek: EdgeKey): Int = {
    if (p0x < ek.p0x) return -1
    if (p0x > ek.p0x) return 1
    if (p0y < ek.p0y) return -1
    if (p0y > ek.p0y) return 1
    // first points are equal, compare second
    if (p1x < ek.p1x) return -1
    if (p1x > ek.p1x) return 1
    if (p1y < ek.p1y) return -1
    if (p1y > ek.p1y) return 1
    0
  }

  override def equals(o: Any): Boolean = {
    if (!o.isInstanceOf[EdgeKey]) return false
    val ek = o.asInstanceOf[EdgeKey]
    p0x == ek.p0x && p0y == ek.p0y && p1x == ek.p1x && p1y == ek.p1y
  }

  /**
   * Gets a hashcode for this object.
   *
   * @return
   *   a hashcode for this object
   */
  override def hashCode: Int = { // Algorithm from Effective Java by Joshua Bloch
    var result = 17
    result = 37 * result + EdgeKey.hashCode(p0x)
    result = 37 * result + EdgeKey.hashCode(p0y)
    result = 37 * result + EdgeKey.hashCode(p1x)
    result = 37 * result + EdgeKey.hashCode(p1y)
    result
  }

  override def toString: String =
    "EdgeKey(" + /*format(p0x, p0y) +*/ ", " + /*format(p1x, p1y) */ ")"

  // private def format(x: Double, y: Double) =
  //   OrdinateFormat.DEFAULT.format(x) + " " + OrdinateFormat.DEFAULT.format(y)
}
