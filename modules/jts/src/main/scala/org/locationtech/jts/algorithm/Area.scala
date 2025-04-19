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
package org.locationtech.jts.algorithm

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateSequence

/**
 * Functions for computing area.
 *
 * @author
 *   Martin Davis
 */
object Area {

  /**
   * Computes the area for a ring.
   *
   * @param ring
   *   the coordinates forming the ring return the area of the ring
   */
  def ofRing(ring: Array[Coordinate]): Double = Math.abs(ofRingSigned(ring))

  def ofRing(ring: CoordinateSequence): Double = Math.abs(ofRingSigned(ring))

  /**
   * Computes the signed area for a ring. The signed area is positive if the ring is oriented CW,
   * negative if the ring is oriented CCW, and zero if the ring is degenerate or flat.
   *
   * @param ring
   *   the coordinates forming the ring return the signed area of the ring
   */
  def ofRingSigned(ring: Array[Coordinate]): Double = {
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
  def ofRingSigned(ring: CoordinateSequence): Double = {
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
}
