// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import org.locationtech.jts.geom.Envelope

/**
 * Computes whether a rectangle intersects line segments. <p> Rectangles contain a large amount of
 * inherent symmetry (or to put it another way, although they contain four coordinates they only
 * actually contain 4 ordinates worth of information). The algorithm used takes advantage of the
 * symmetry of the geometric situation to optimize performance by minimizing the number of line
 * intersection tests.
 *
 * @author
 *   Martin Davis
 */
class RectangleLineIntersector(var rectEnv: Envelope) {

  /**
   * Creates a new intersector for the given query rectangle, specified as an {link Envelope}.
   *
   * @param rectEnv
   *   the query rectangle, specified as an Envelope
   */
  /**
   * Up and Down are the diagonal orientations relative to the Left side of the rectangle. Index 0
   * is the left side, 1 is the right side.
   */
  private val diagUp0   = new Coordinate(rectEnv.getMinX, rectEnv.getMinY)
  private val diagUp1   = new Coordinate(rectEnv.getMaxX, rectEnv.getMaxY)
  private val diagDown0 = new Coordinate(rectEnv.getMinX, rectEnv.getMaxY)
  private val diagDown1 = new Coordinate(rectEnv.getMaxX, rectEnv.getMinY)
  // for intersection testing, don't need to set precision model
  private val li        = new RobustLineIntersector

  /**
   * Tests whether the query rectangle intersects a given line segment.
   *
   * @param p0
   *   the first endpoint of the segment
   * @param p1
   *   the second endpoint of the segment return true if the rectangle intersects the segment
   */
  def intersects(p0a: Coordinate, p1a: Coordinate): Boolean = { // TODO: confirm that checking envelopes first is faster
    var p0 = p0a
    var p1 = p1a

    /**
     * If the segment envelope is disjoint from the rectangle envelope, there is no intersection
     */
    val segEnv = new Envelope(p0, p1)
    if (!rectEnv.intersects(segEnv)) return false

    /**
     * If either segment endpoint lies in the rectangle, there is an intersection.
     */
    if (rectEnv.intersects(p0)) return true
    if (rectEnv.intersects(p1)) return true

    /**
     * Normalize segment. This makes p0 less than p1, so that the segment runs to the right, or
     * vertically upwards.
     */
    if (p0.compareTo(p1) > 0) {
      val tmp = p0
      p0 = p1
      p1 = tmp
    }

    /**
     * Compute angle of segment. Since the segment is normalized to run left to right, it is
     * sufficient to simply test the Y ordinate. "Upwards" means relative to the left end of the
     * segment.
     */
    var isSegUpwards = false
    if (p1.y > p0.y) isSegUpwards = true

    /**
     * Since we now know that neither segment endpoint lies in the rectangle, there are two possible
     * situations: 1) the segment is disjoint to the rectangle 2) the segment crosses the rectangle
     * completely.
     *
     * In the case of a crossing, the segment must intersect a diagonal of the rectangle.
     *
     * To distinguish these two cases, it is sufficient to test intersection with a single diagonal
     * of the rectangle, namely the one with slope "opposite" to the slope of the segment. (Note
     * that if the segment is axis-parallel, it must intersect both diagonals, so this is still
     * sufficient.)
     */
    if (isSegUpwards) li.computeIntersection(p0, p1, diagDown0, diagDown1)
    else li.computeIntersection(p0, p1, diagUp0, diagUp1)
    if (li.hasIntersection) return true
    false
  }
}
