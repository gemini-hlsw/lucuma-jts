// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.noding.snapround

import org.locationtech.jts.algorithm.Distance
import org.locationtech.jts.algorithm.LineIntersector
import org.locationtech.jts.algorithm.RobustLineIntersector
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.PrecisionModel
import org.locationtech.jts.noding.NodedSegmentString
import org.locationtech.jts.noding.SegmentIntersector
import org.locationtech.jts.noding.SegmentString

import java.util

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
 * Finds intersections between line segments which will be snap-rounded, and adds them as nodes to
 * the segments. <p> Intersections are detected and computed using full precision. Snapping takes
 * place in a subsequent phase. <p> The intersection points are recorded, so that HotPixels can be
 * created for them. <p> To avoid robustness issues with vertices which lie very close to line
 * segments a heuristic is used: nodes are created if a vertex lies within a tolerance distance of
 * the interior of a segment. The tolerance distance is chosen to be significantly below the
 * snap-rounding grid size. This has empirically proven to eliminate noding failures.
 *
 * @version 1.17
 */
object SnapRoundingIntersectionAdder {

  /**
   * The division factor used to determine nearness distance tolerance for interior intersection
   * detection.
   */
  private val NEARNESS_FACTOR = 100
}

class SnapRoundingIntersectionAdder(var precModel: PrecisionModel)

/**
 * Creates an intersector which finds all snapped interior intersections, and adds them as nodes.
 *
 * @param pm
 *   the precision mode to use
 */
    extends SegmentIntersector {

  /**
   * Nearness distance tolerance is a small fraction of the snap grid size
   */
  val snapGridSize: Double = 1.0 / precModel.getScale
  private var nearnessTol  = snapGridSize / SnapRoundingIntersectionAdder.NEARNESS_FACTOR

  /**
   * Intersections are detected and computed using full precision. They are snapped in a subsequent
   * phase.
   */
  private var li                  = new RobustLineIntersector
  final private var intersections = new util.ArrayList[Coordinate]

  /**
   * Gets the created intersection nodes, so they can be processed as hot pixels.
   *
   * @return
   *   a list of the intersection points
   */
  def getIntersections: util.List[Coordinate] = intersections

  /**
   * This method is called by clients of the {@link SegmentIntersector} class to process
   * intersections for two segments of the {@link SegmentString}s being intersected. Note that some
   * clients (such as <code>MonotoneChain</code>s) may optimize away this call for segment pairs
   * which they have determined do not intersect (e.g. by an disjoint envelope test).
   */
  override def processIntersections(
    e0:        SegmentString,
    segIndex0: Int,
    e1:        SegmentString,
    segIndex1: Int
  ): Unit = { // don't bother intersecting a segment with itself
    if ((e0 eq e1) && segIndex0 == segIndex1) return
    val p00 = e0.getCoordinates(segIndex0)
    val p01 = e0.getCoordinates(segIndex0 + 1)
    val p10 = e1.getCoordinates(segIndex1)
    val p11 = e1.getCoordinates(segIndex1 + 1)
    li.computeIntersection(p00, p01, p10, p11)
    // if (li.hasIntersection() && li.isProper()) Debug.println(li);
    if (li.hasIntersection) if (li.isInteriorIntersection) {
      for (intIndex <- 0 until li.getIntersectionNum)
        intersections.add(li.getIntersection(intIndex))
      e0.asInstanceOf[NodedSegmentString].addIntersections(li, segIndex0, 0)
      e1.asInstanceOf[NodedSegmentString].addIntersections(li, segIndex1, 1)
      return
    }

    /**
     * Segments did not actually intersect, within the limits of orientation index robustness.
     *
     * To avoid certain robustness issues in snap-rounding, also treat very near vertex-segment
     * situations as intersections.
     */
    processNearVertex(p00, e1, segIndex1, p10, p11)
    processNearVertex(p01, e1, segIndex1, p10, p11)
    processNearVertex(p10, e0, segIndex0, p00, p01)
    processNearVertex(p11, e0, segIndex0, p00, p01)
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
   * @param edge
   * @param segIndex
   * @param p0
   * @param p1
   */
  private def processNearVertex(
    p:        Coordinate,
    edge:     SegmentString,
    segIndex: Int,
    p0:       Coordinate,
    p1:       Coordinate
  ): Unit = {

    /**
     * Don't add intersection if candidate vertex is near endpoints of segment. This avoids creating
     * "zig-zag" linework (since the vertex could actually be outside the segment envelope).
     */
    if (p.distance(p0) < nearnessTol) return
    if (p.distance(p1) < nearnessTol) return
    val distSeg = Distance.pointToSegment(p, p0, p1)
    if (distSeg < nearnessTol) {
      intersections.add(p)
      edge.asInstanceOf[NodedSegmentString].addIntersection(p, segIndex)
    }
  }

  /**
   * Always process all intersections
   *
   * @return
   *   false always
   */
  override def isDone = false
}
