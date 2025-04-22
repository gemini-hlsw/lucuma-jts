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

/**
 * @version 1.7
 */

/**
 * Utility functions for working with quadrants, which are numbered as follows: <pre> 1 | 0 --+-- 2
 * \| 3 </pre>
 *
 * @version 1.7
 */
object Quadrant {
  val NE = 0
  val NW = 1
  val SW = 2
  val SE = 3

  /**
   * Returns the quadrant of a directed line segment (specified as x and y displacements, which
   * cannot both be 0).
   *
   * throws IllegalArgumentException if the displacements are both 0
   */
  def quadrant(dx: Double, dy: Double): Int = {
    if (dx == 0.0 && dy == 0.0)
      throw new IllegalArgumentException(
        "Cannot compute the quadrant for point ( " + dx + ", " + dy + " )"
      )
    if (dx >= 0.0)
      if (dy >= 0.0) NE
      else SE
    else if (dy >= 0.0) NW
    else SW
  }

  /**
   * Returns the quadrant of a directed line segment from p0 to p1.
   *
   * throws IllegalArgumentException if the points are equal
   */
  def quadrant(p0: Coordinate, p1: Coordinate): Int = {
    if ((p1.x == p0.x) && (p1.y == p0.y))
      throw new IllegalArgumentException(
        "Cannot compute the quadrant for two identical points " + p0
      )
    if (p1.x >= p0.x)
      if (p1.y >= p0.y) NE
      else SE
    else if (p1.y >= p0.y) NW
    else SW
  }

  /**
   * Returns true if the quadrants are 1 and 3, or 2 and 4
   */
  def isOpposite(quad1: Int, quad2: Int): Boolean = {
    if (quad1 == quad2) return false
    val diff = (quad1 - quad2 + 4) % 4
    // if quadrants are not adjacent, they are opposite
    if (diff == 2) return true
    false
  }

  /**
   * Returns the right-hand quadrant of the halfplane defined by the two quadrants, or -1 if the
   * quadrants are opposite, or the quadrant if they are identical.
   */
  def commonHalfPlane(quad1: Int, quad2: Int): Int = { // if quadrants are the same they do not determine a unique common halfplane.
    // Simply return one of the two possibilities
    if (quad1 == quad2) return quad1
    val diff = (quad1 - quad2 + 4) % 4
    // if quadrants are not adjacent, they do not share a common halfplane
    if (diff == 2) return -1
    //
    val min  =
      if (quad1 < quad2) quad1
      else quad2
    val max  =
      if (quad1 > quad2) quad1
      else quad2
    // for this one case, the righthand plane is NOT the minimum index;
    if (min == 0 && max == 3) return 3
    // in general, the halfplane index is the minimum of the two adjacent quadrants
    min
  }

  /**
   * Returns whether the given quadrant lies within the given halfplane (specified by its right-hand
   * quadrant).
   */
  def isInHalfPlane(quad: Int, halfPlane: Int): Boolean = {
    if (halfPlane == SE) return quad == SE || quad == SW
    quad == halfPlane || quad == halfPlane + 1
  }

  /**
   * Returns true if the given quadrant is 0 or 1.
   */
  def isNorthern(quad: Int): Boolean = quad == NE || quad == NW
}
