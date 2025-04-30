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
package org.locationtech.jts.noding

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.util.Assert

/**
 * Implements a robust method of comparing the relative position of two points along the same
 * segment. The coordinates are assumed to lie "near" the segment. This means that this algorithm
 * will only return correct results if the input coordinates have the same precision and correspond
 * to rounded values of exact coordinates lying on the segment.
 *
 * @version 1.7
 */
object SegmentPointComparator {

  /**
   * Compares two {link Coordinate}s for their relative position along a segment lying in the
   * specified {link Octant}.
   *
   * return -1 node0 occurs first; 0 the two nodes are equal; 1 node1 occurs first
   */
  def compare(octant: Int, p0: Coordinate, p1: Coordinate): Int = { // nodes can only be equal if their coordinates are equal
    if (p0.equals2D(p1)) return 0
    val xSign = relativeSign(p0.x, p1.x)
    val ySign = relativeSign(p0.y, p1.y)
    octant match {
      case 0 =>
        return compareValue(xSign, ySign)
      case 1 =>
        return compareValue(ySign, xSign)
      case 2 =>
        return compareValue(ySign, -xSign)
      case 3 =>
        return compareValue(-xSign, ySign)
      case 4 =>
        return compareValue(-xSign, -ySign)
      case 5 =>
        return compareValue(-ySign, -xSign)
      case 6 =>
        return compareValue(-ySign, xSign)
      case 7 =>
        return compareValue(xSign, -ySign)
    }
    Assert.shouldNeverReachHere("invalid octant value")
    0
  }

  def relativeSign(x0: Double, x1: Double): Int = {
    if (x0 < x1) return -1
    if (x0 > x1) return 1
    0
  }

  private def compareValue(compareSign0: Int, compareSign1: Int): Int = {
    if (compareSign0 < 0) return -1
    if (compareSign0 > 0) return 1
    if (compareSign1 < 0) return -1
    if (compareSign1 > 0) return 1
    0
  }
}
