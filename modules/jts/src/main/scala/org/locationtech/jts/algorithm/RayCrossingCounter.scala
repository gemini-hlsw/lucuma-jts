// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import org.locationtech.jts.geom.Location

/**
 * Counts the number of segments crossed by a horizontal ray extending to the right from a given
 * point, in an incremental fashion. This can be used to determine whether a point lies in a {link
 * Polygonal} geometry. The class determines the situation where the point lies exactly on a
 * segment. When being used for Point-In-Polygon determination, this case allows short-circuiting
 * the evaluation. <p> This class handles polygonal geometries with any number of shells and holes.
 * The orientation of the shell and hole rings is unimportant. In order to compute a correct
 * location for a given polygonal geometry, it is essential that <b>all</b> segments are counted
 * which <ul> <li>touch the ray <li>lie in in any ring which may contain the point </ul> The only
 * exception is when the point-on-segment situation is detected, in which case no further processing
 * is required. The implication of the above rule is that segments which can be a priori determined
 * to <i>not</i> touch the ray (i.e. by a test of their bounding box or Y-extent) do not need to be
 * counted. This allows for optimization by indexing. <p> This implementation uses the
 * extended-precision orientation test, to provide maximum robustness and consistency within other
 * algorithms.
 *
 * @author
 *   Martin Davis
 */
object RayCrossingCounter {

  /**
   * Determines the {link Location} of a point in a ring. This method is an exemplar of how to use
   * this class.
   *
   * @param p
   *   the point to test
   * @param ring
   *   an array of Coordinates forming a ring return the location of the point in the ring
   */
  def locatePointInRing(p: Coordinate, ring: Array[Coordinate]): Int = {
    val counter = new RayCrossingCounter(p)
    var i       = 1
    while (i < ring.length) {
      val p1 = ring(i)
      val p2 = ring(i - 1)
      counter.countSegment(p1, p2)
      if (counter.isOnSegment) return counter.getLocation
      i += 1
    }
    counter.getLocation
  }

  /**
   * Determines the {link Location} of a point in a ring.
   *
   * @param p
   *   the point to test
   * @param ring
   *   a coordinate sequence forming a ring return the location of the point in the ring
   */
  def locatePointInRing(p: Coordinate, ring: CoordinateSequence): Int = {
    val counter = new RayCrossingCounter(p)
    val p1      = new Coordinate
    val p2      = new Coordinate
    var i       = 1
    while (i < ring.size) {
      // ring.getCoordinate(i, p1); // throws exception if ring contains M ordinate
      p1.x = ring.getOrdinate(i, CoordinateSequence.X)
      p1.y = ring.getOrdinate(i, CoordinateSequence.Y)
      // ring.getCoordinate(i - 1, p2); // throws exception if ring contains M ordinate
      p2.x = ring.getOrdinate(i - 1, CoordinateSequence.X)
      p2.y = ring.getOrdinate(i - 1, CoordinateSequence.Y)
      counter.countSegment(p1, p2)
      if (counter.isOnSegment) return counter.getLocation
      i += 1
    }
    counter.getLocation
  }
}

class RayCrossingCounter(var p: Coordinate) {
  private var crossingCount    = 0
  // true if the test point lies on an input segment
  private var isPointOnSegment = false

  /**
   * Counts a segment
   *
   * @param p1
   *   an endpoint of the segment
   * @param p2
   *   another endpoint of the segment
   */
  def countSegment(p1: Coordinate, p2: Coordinate): Unit = {

    /**
     * For each segment, check if it crosses a horizontal ray running from the test point in the
     * positive x direction.
     */
    // check if the segment is strictly to the left of the test point
    if (p1.x < p.x && p2.x < p.x) return
    // check if the point is equal to the current ring vertex
    if ((p.x == p2.x) && (p.y == p2.y)) {
      isPointOnSegment = true
      return
    }

    /**
     * For horizontal segments, check if the point is on the segment. Otherwise, horizontal segments
     * are not counted.
     */
    if ((p1.y == p.y) && (p2.y == p.y)) {
      var minx = p1.x
      var maxx = p2.x
      if (minx > maxx) {
        minx = p2.x
        maxx = p1.x
      }
      if (p.x >= minx && p.x <= maxx) isPointOnSegment = true
      return
    }

    /**
     * Evaluate all non-horizontal segments which cross a horizontal ray to the right of the test
     * pt. To avoid double-counting shared vertices, we use the convention that <ul> <li>an upward
     * edge includes its starting endpoint, and excludes its final endpoint <li>a downward edge
     * excludes its starting endpoint, and includes its final endpoint </ul>
     */
    if (((p1.y > p.y) && (p2.y <= p.y)) || ((p2.y > p.y) && (p1.y <= p.y))) {
      var orient = Orientation.index(p1, p2, p)
      if (orient == Orientation.COLLINEAR) {
        isPointOnSegment = true
        return
      }
      // Re-orient the result if needed to ensure effective segment direction is upwards
      if (p2.y < p1.y) orient = -orient
      // The upward segment crosses the ray if the test point lies to the left (CCW) of the segment.
      if (orient == Orientation.LEFT) crossingCount += 1
    }
  }

  /**
   * Reports whether the point lies exactly on one of the supplied segments. This method may be
   * called at any time as segments are processed. If the result of this method is <tt>true</tt>, no
   * further segments need be supplied, since the result will never change again.
   *
   * return true if the point lies exactly on a segment
   */
  def isOnSegment: Boolean = isPointOnSegment

  /**
   * Gets the {link Location} of the point relative to the ring, polygon or multipolygon from which
   * the processed segments were provided. <p> This method only determines the correct location if
   * <b>all</b> relevant segments must have been processed.
   *
   * return the Location of the point
   */
  def getLocation: Int = {
    if (isPointOnSegment) return Location.BOUNDARY
    // The point is in the interior of the ring if the number of X-crossings is
    // odd.
    if ((crossingCount % 2) == 1) return Location.INTERIOR
    Location.EXTERIOR
  }

  /**
   * Tests whether the point lies in or on the ring, polygon or multipolygon from which the
   * processed segments were provided. <p> This method only determines the correct location if
   * <b>all</b> relevant segments must have been processed.
   *
   * return true if the point lies in or on the supplied polygon
   */
  def isPointInPolygon: Boolean = getLocation != Location.EXTERIOR
}
