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
 * Functions for computing length.
 *
 * @author
 *   Martin Davis
 */
object Length {

  /**
   * Computes the length of a linestring specified by a sequence of points.
   *
   * @param pts
   *   the points specifying the linestring return the length of the linestring
   */
  def ofLine(pts: CoordinateSequence): Double = { // optimized for processing CoordinateSequences
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
