// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2022 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v20.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.operation.relateng

import org.locationtech.jts.algorithm.RobustLineIntersector
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.noding.SegmentIntersector
import org.locationtech.jts.noding.SegmentString

/**
 * Tests segments of {link RelateSegmentString}s and if they intersect adds the intersection(s) to
 * the {link TopologyComputer}.
 *
 * @author
 *   Martin Davis
 */
class EdgeSegmentIntersector(topoComputer: TopologyComputer) extends SegmentIntersector {

  private val li = new RobustLineIntersector

  override def isDone: Boolean = topoComputer.isResultKnown

  override def processIntersections(
    ss0:       SegmentString,
    segIndex0: Int,
    ss1:       SegmentString,
    segIndex1: Int
  ): Unit = {
    // don't intersect a segment with itself
    if ((ss0 eq ss1) && segIndex0 == segIndex1) return

    val rss0 = ss0.asInstanceOf[RelateSegmentString]
    val rss1 = ss1.asInstanceOf[RelateSegmentString]
    // TODO: move this ordering logic to TopologyBuilder
    if (rss0.isA) {
      addIntersections(rss0, segIndex0, rss1, segIndex1)
    } else {
      addIntersections(rss1, segIndex1, rss0, segIndex0)
    }
  }

  private def addIntersections(
    ssA:       RelateSegmentString,
    segIndexA: Int,
    ssB:       RelateSegmentString,
    segIndexB: Int
  ): Unit = {

    val a0 = ssA.getCoordinate(segIndexA)
    val a1 = ssA.getCoordinate(segIndexA + 1)
    val b0 = ssB.getCoordinate(segIndexB)
    val b1 = ssB.getCoordinate(segIndexB + 1)

    li.computeIntersection(a0, a1, b0, b1)

    if (!li.hasIntersection)
      return

    var i = 0
    while (i < li.getIntersectionNum) {
      val intPt = li.getIntersection(i)

      /**
       * Ensure endpoint intersections are added once only, for their canonical segments. Proper
       * intersections lie on a unique segment so do not need to be checked. And it is important
       * that the Containing Segment check not be used, since due to intersection computation
       * roundoff, it is not reliable in that situation.
       */
      if (
        li.isProper()
        || (ssA.isContainingSegment(segIndexA, intPt)
          && ssB.isContainingSegment(segIndexB, intPt))
      ) {
        val nsa = ssA.createNodeSection(segIndexA, intPt)
        val nsb = ssB.createNodeSection(segIndexB, intPt)
        topoComputer.addIntersection(nsa, nsb)
      }
      i += 1
    }
  }

}
