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
package org.locationtech.jts.geomgraph.index

import java.util
import org.locationtech.jts.geomgraph.Edge

/**
 * Finds all intersections in one or two sets of edges, using the straightforward method of
 * comparing all segments. This algorithm is too slow for production use, but is useful for testing
 * purposes.
 *
 * @version 1.7
 */
class SimpleEdgeSetIntersector() extends EdgeSetIntersector {
  // statistics information
  private[index] var nOverlaps = 0

  override def computeIntersections(
    edges:           util.List[Edge],
    si:              SegmentIntersector,
    testAllSegments: Boolean
  ): Unit = {
    nOverlaps = 0
    val i0 = edges.iterator
    while (i0.hasNext) {
      val edge0 = i0.next
      val i1    = edges.iterator
      while (i1.hasNext) {
        val edge1 = i1.next
        if (testAllSegments || (edge0 ne edge1)) computeIntersects(edge0, edge1, si)
      }
    }
  }

  override def computeIntersections(
    edges0: util.List[Edge],
    edges1: util.List[Edge],
    si:     SegmentIntersector
  ): Unit = {
    nOverlaps = 0
    val i0 = edges0.iterator
    while (i0.hasNext) {
      val edge0 = i0.next
      val i1    = edges1.iterator
      while (i1.hasNext) {
        val edge1 = i1.next
        computeIntersects(edge0, edge1, si)
      }
    }
  }

  /**
   * Performs a brute-force comparison of every segment in each Edge. This has n^2 performance, and
   * is about 100 times slower than using monotone chains.
   */
  private def computeIntersects(e0: Edge, e1: Edge, si: SegmentIntersector): Unit = {
    val pts0 = e0.getCoordinates
    val pts1 = e1.getCoordinates
    var i0   = 0
    while (i0 < pts0.length - 1) {
      var i1 = 0
      while (i1 < pts1.length - 1) {
        si.addIntersections(e0, i0, e1, i1)
        i1 += 1
      }
      i0 += 1
    }
  }
}
