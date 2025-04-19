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
package org.locationtech.jts.geomgraph.index

/**
 * @version 1.7
 */

import org.locationtech.jts.geomgraph.Edge

import java.util
import java.util.Collections

/**
 * Finds all intersections in one or two sets of edges, using an x-axis sweepline algorithm in
 * conjunction with Monotone Chains. While still O(n^2) in the worst case, this algorithm
 * drastically improves the average-case time. The use of MonotoneChains as the items in the index
 * seems to offer an improvement in performance over a sweep-line alone.
 *
 * @version 1.7
 */
class SimpleMCSweepLineIntersector()

/**
 * A SimpleMCSweepLineIntersector creates monotone chains from the edges and compares them using a
 * simple sweep-line along the x-axis.
 */
    extends EdgeSetIntersector {
  private[index] val events    = new util.ArrayList[SweepLineEvent]
  // statistics information
  private[index] var nOverlaps = 0

  override def computeIntersections(
    edges:           util.List[Edge],
    si:              SegmentIntersector,
    testAllSegments: Boolean
  ): Unit = {
    if (testAllSegments) addEdges(edges, null)
    else addEdges(edges)
    computeIntersections(si)
  }

  override def computeIntersections(
    edges0: util.List[Edge],
    edges1: util.List[Edge],
    si:     SegmentIntersector
  ): Unit = {
    addEdges(edges0, edges0)
    addEdges(edges1, edges1)
    computeIntersections(si)
  }

  private def addEdges(edges: util.List[Edge]): Unit = {
    val i = edges.iterator
    while (i.hasNext) {
      val edge = i.next
      // edge is its own group
      addEdge(edge, edge)
    }
  }

  private def addEdges(edges: util.List[Edge], edgeSet: AnyRef): Unit = {
    val i = edges.iterator
    while (i.hasNext) {
      val edge = i.next
      addEdge(edge, edgeSet)
    }
  }

  private def addEdge(edge: Edge, edgeSet: AnyRef): Unit = {
    val mce        = edge.getMonotoneChainEdge
    val startIndex = mce.getStartIndexes
    var i          = 0
    while (i < startIndex.length - 1) {
      val mc          = new MonotoneChain(mce, i)
      val insertEvent = new SweepLineEvent(edgeSet, mce.getMinX(i), mc)
      events.add(insertEvent)
      events.add(new SweepLineEvent(mce.getMaxX(i), insertEvent))
      i += 1
    }
  }

  /**
   * Because Delete Events have a link to their corresponding Insert event, it is possible to
   * compute exactly the range of events which must be compared to a given Insert event object.
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
      if (si.isDone) return () // todo: break is not supported
      i += 1
    }
  }

  private def processOverlaps(
    start: Int,
    end:   Int,
    ev0:   SweepLineEvent,
    si:    SegmentIntersector
  ): Unit = {
    val mc0 = ev0.getObject.asInstanceOf[MonotoneChain]

    /**
     * Since we might need to test for self-intersections, include current INSERT event object in
     * list of event objects to test. Last index can be skipped, because it must be a Delete event.
     */
    var i = start
    while (i < end) {
      val ev1 = events.get(i)
      if (ev1.isInsert) {
        val mc1 = ev1.getObject.asInstanceOf[MonotoneChain]
        // don't compare edges in same group, if labels are present
        if (!ev0.isSameLabel(ev1)) {
          mc0.computeIntersections(mc1, si)
          nOverlaps += 1
        }
      }
      i += 1
    }
  }
}
