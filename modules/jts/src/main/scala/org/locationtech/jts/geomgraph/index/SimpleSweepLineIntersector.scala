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
package org.locationtech.jts.geomgraph.index

/**
 * @version 1.7
 */

import java.util
import java.util.Collections
import org.locationtech.jts.geomgraph.Edge

/**
 * Finds all intersections in one or two sets of edges, using a simple x-axis sweepline algorithm.
 * While still O(n^2) in the worst case, this algorithm drastically improves the average-case time.
 *
 * @version 1.7
 */
class SimpleSweepLineIntersector() extends EdgeSetIntersector {
  private[index] val events    = new util.ArrayList[SweepLineEvent]
  // statistics information
  private[index] var nOverlaps = 0

  override def computeIntersections(
    edges:           util.List[Edge],
    si:              SegmentIntersector,
    testAllSegments: Boolean
  ): Unit = {
    if (testAllSegments) add(edges, null)
    else add(edges)
    computeIntersections(si)
  }

  override def computeIntersections(
    edges0: util.List[Edge],
    edges1: util.List[Edge],
    si:     SegmentIntersector
  ): Unit = {
    add(edges0, edges0)
    add(edges1, edges1)
    computeIntersections(si)
  }

  private def add(edges: util.List[Edge]): Unit = {
    val i = edges.iterator
    while (i.hasNext) {
      val edge = i.next
      // edge is its own group
      add(edge, edge)
    }
  }

  private def add(edges: util.List[Edge], edgeSet: Any): Unit = {
    val i = edges.iterator
    while (i.hasNext) {
      val edge = i.next
      add(edge, edgeSet)
    }
  }

  private def add(edge: Edge, edgeSet: Any): Unit = {
    val pts = edge.getCoordinates
    var i   = 0
    while (i < pts.length - 1) {
      val ss          = new SweepLineSegment(edge, i)
      val insertEvent = new SweepLineEvent(edgeSet, ss.getMinX, null)
      events.add(insertEvent)
      events.add(new SweepLineEvent(ss.getMaxX, insertEvent))
      i += 1
    }
  }

  /**
   * Because DELETE events have a link to their corresponding INSERT event, it is possible to
   * compute exactly the range of events which must be compared to a given INSERT event object.
   */
  private def prepareEvents(): Unit = {
    Collections.sort(events)
    // set DELETE event indexes
    var i = 0
    while (i < events.size) {
      val ev = events.get(i)
      if (ev.isDelete) ev.getInsertEvent.setDeleteEventIndex(i)
      i += 1
    }
  }

  private def computeIntersections(si: SegmentIntersector): Unit = {
    nOverlaps = 0
    prepareEvents()
    var i = 0
    while (i < events.size) {
      val ev = events.get(i)
      if (ev.isInsert) processOverlaps(i, ev.getDeleteEventIndex, ev, si)
      i += 1
    }
  }

  private def processOverlaps(
    start: Int,
    end:   Int,
    ev0:   SweepLineEvent,
    si:    SegmentIntersector
  ): Unit = {
    val ss0 = ev0.getObject.asInstanceOf[SweepLineSegment]

    /**
     * Since we might need to test for self-intersections, include current INSERT event object in
     * list of event objects to test. Last index can be skipped, because it must be a Delete event.
     */
    var i = start
    while (i < end) {
      val ev1 = events.get(i)
      if (ev1.isInsert) {
        val ss1 = ev1.getObject.asInstanceOf[SweepLineSegment]
        // don't compare edges in same group, if labels are present
        if (!ev0.isSameLabel(ev1)) {
          ss0.computeIntersections(ss1, si)
          nOverlaps += 1
        }
      }
      i += 1
    }
  }
}
