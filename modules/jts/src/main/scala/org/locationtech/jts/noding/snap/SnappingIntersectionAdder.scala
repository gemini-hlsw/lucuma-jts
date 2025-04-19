// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.noding.snap

import org.locationtech.jts.algorithm.Distance
import org.locationtech.jts.algorithm.LineIntersector
import org.locationtech.jts.algorithm.RobustLineIntersector
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.noding.NodedSegmentString
import org.locationtech.jts.noding.SegmentIntersector
import org.locationtech.jts.noding.SegmentString

/*
 * Copyright (c) 2016 Martin Davis.
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
 * Finds intersections between line segments which are being snapped, and adds them as nodes.
 *
 * @version 1.17
 */
object SnappingIntersectionAdder {

  /**
   * Tests if segments are adjacent on the same SegmentString. Closed segStrings require a check for
   * the point shared by the beginning and end segments.
   */
  private def isAdjacent(
    ss0:       SegmentString,
    segIndex0: Int,
    ss1:       SegmentString,
    segIndex1: Int
  ): Boolean = {
    if (ss0 ne ss1) {
      return false
    }
    val isAdjacent: Boolean = Math.abs(segIndex0 - segIndex1) == 1
    if (isAdjacent) {
      return true
    }
    if (ss0.isClosed) {
      val maxSegIndex: Int = ss0.size - 1
      if (
        (segIndex0 == 0 && segIndex1 == maxSegIndex) || (segIndex1 == 0 && segIndex0 == maxSegIndex)
      ) {
        return true
      }
    }
    return false
  }
}

class SnappingIntersectionAdder(var snapTolerance: Double, var snapPointIndex: SnappingPointIndex)

/**
 * Creates an intersector which finds intersections, snaps them, and adds them as nodes.
 *
 * @param snapTolerance
 *   the snapping tolerance distance
 * @param snapPointIndex
 *   the snapPointIndex
 */
    extends SegmentIntersector {
  private val li: LineIntersector = new RobustLineIntersector

  /**
   * This method is called by clients of the {@link SegmentIntersector} class to process
   * intersections for two segments of the {@link SegmentString} s being intersected. Note that some
   * clients (such as <code>MonotoneChain</code>s) may optimize away this call for segment pairs
   * which they have determined do not intersect (e.g. by an disjoint envelope test).
   */
  override def processIntersections(
    seg0:      SegmentString,
    segIndex0: Int,
    seg1:      SegmentString,
    segIndex1: Int
  ): Unit = { // don't bother intersecting a segment with itself
    if ((seg0 eq seg1) && segIndex0 == segIndex1) {
      return
    }
    val p00: Coordinate = seg0.getCoordinates(segIndex0)
    val p01: Coordinate = seg0.getCoordinates(segIndex0 + 1)
    val p10: Coordinate = seg1.getCoordinates(segIndex1)
    val p11: Coordinate = seg1.getCoordinates(segIndex1 + 1)
    if (!SnappingIntersectionAdder.isAdjacent(seg0, segIndex0, seg1, segIndex1)) {
      li.computeIntersection(p00, p01, p10, p11);

      /**
       * Process single point intersections only. Two-point (collinear) ones are handled by the
       * near-vertex code
       */
      if (li.hasIntersection && li.getIntersectionNum == 1) {

        val intPt: Coordinate  = li.getIntersection(0)
        val snapPt: Coordinate = snapPointIndex.snap(intPt)
        seg0.asInstanceOf[NodedSegmentString].addIntersection(snapPt, segIndex0)
        seg1.asInstanceOf[NodedSegmentString].addIntersection(snapPt, segIndex1)
      }
    }

    /**
     * The segments must also be snapped to the other segment endpoints.
     */
    processNearVertex(seg0, segIndex0, p00, seg1, segIndex1, p10, p11)
    processNearVertex(seg0, segIndex0, p01, seg1, segIndex1, p10, p11)
    processNearVertex(seg1, segIndex1, p10, seg0, segIndex0, p00, p01)
    processNearVertex(seg1, segIndex1, p11, seg0, segIndex0, p00, p01)
  }

  /**
   * If an endpoint of one segment is near the <i>interior</i> of the other segment, add it as an
   * intersection. EXCEPT if the endpoint is also close to a segment endpoint (since this can
   * introduce "zigs" in the linework). <p> This resolves situations where a segment A endpoint is
   * extremely close to another segment B, but is not quite crossing. Due to robustness issues in
   * orientation detection, this can result in the snapped segment A crossing segment B without a
   * node being introduced.
   *
   * @param p
   * @param ss
   * @param segIndex
   * @param p0
   * @param p1
   */
  private def processNearVertex(
    srcSS:    SegmentString,
    srcIndex: Int,
    p:        Coordinate,
    ss:       SegmentString,
    segIndex: Int,
    p0:       Coordinate,
    p1:       Coordinate
  ): Unit = {

    /**
     * Don't add intersection if candidate vertex is near endpoints of segment. This avoids creating
     * "zig-zag" linework (since the vertex could actually be outside the segment envelope). Also,
     * this should have already been snapped.
     */
    if (p.distance(p0) < snapTolerance) {
      return
    }
    if (p.distance(p1) < snapTolerance) {
      return
    }
    val distSeg: Double = Distance.pointToSegment(p, p0, p1)
    if (distSeg < snapTolerance) { // add node to target segment
      ss.asInstanceOf[NodedSegmentString].addIntersection(p, segIndex)
      // add node at vertex to source SS
      srcSS.asInstanceOf[NodedSegmentString].addIntersection(p, srcIndex)
    }
  }

  /**
   * Always process all intersections
   *
   * @return
   *   false always
   */
  override def isDone: Boolean =
    return false
}
