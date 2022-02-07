// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
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
package org.locationtech.jts.noding

import org.locationtech.jts.geom.Coordinate

/**
 * Methods for computing and working with octants of the Cartesian plane Octants are numbered as
 * follows: <pre> \2|1/ 3 \|/ 0
 * ---+-- 4 /|\ 7 /5|6\ </pre> If line segments lie along a coordinate axis, the octant is the lower
 * of the two possible values.
 *
 * @version 1.7
 */
object Octant {

  /**
   * Returns the octant of a directed line segment (specified as x and y displacements, which cannot
   * both be 0).
   */
  def octant(dx: Double, dy: Double): Int = {
    if (dx == 0.0 && dy == 0.0)
      throw new IllegalArgumentException(
        "Cannot compute the octant for point ( " + dx + ", " + dy + " )"
      )
    val adx = Math.abs(dx)
    val ady = Math.abs(dy)
    if (dx >= 0)
      if (dy >= 0)
        if (adx >= ady) 0
        else 1
      else { // dy < 0
        if (adx >= ady) 7
        else 6
      }
    else { // dx < 0
      if (dy >= 0)
        if (adx >= ady) 3
        else 2
      else if (adx >= ady) 4
      else 5
    }
  }

  /**
   * Returns the octant of a directed line segment from p0 to p1.
   */
  def octant(p0: Coordinate, p1: Coordinate): Int = {
    val dx = p1.x - p0.x
    val dy = p1.y - p0.y
    if (dx == 0.0 && dy == 0.0)
      throw new IllegalArgumentException("Cannot compute the octant for two identical points " + p0)
    octant(dx, dy)
  }
}
