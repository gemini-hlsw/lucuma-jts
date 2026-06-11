// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2023 Martin Davis.
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

import java.util

import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.index.chain.MonotoneChain
import org.locationtech.jts.index.chain.MonotoneChainBuilder
import org.locationtech.jts.index.chain.MonotoneChainOverlapAction
import org.locationtech.jts.index.hprtree.HPRtree
import org.locationtech.jts.noding.SegmentString

class EdgeSetIntersector(
  edgesA:               util.List[RelateSegmentString],
  edgesB:               util.List[RelateSegmentString],
  private val envelope: Envelope
) {

  private val index                                = new HPRtree()
  private val monoChains: util.List[MonotoneChain] = new util.ArrayList[MonotoneChain]
  private var idCounter                            = 0

  addEdges(edgesA)
  addEdges(edgesB)
  // build index to ensure thread-safety
  index.build()

  private def addEdges(segStrings: util.Collection[RelateSegmentString]): Unit = {
    val it = segStrings.iterator
    while (it.hasNext) {
      val ss: SegmentString = it.next
      addToIndex(ss)
    }
  }

  private def addToIndex(segStr: SegmentString): Unit = {
    val segChains: util.List[MonotoneChain] =
      MonotoneChainBuilder.getChains(segStr.getCoordinates, segStr)
    val it                                  = segChains.iterator
    while (it.hasNext) {
      val mc = it.next
      if (envelope == null || envelope.intersects(mc.getEnvelope)) {
        mc.setId(idCounter)
        idCounter += 1
        index.insert(mc.getEnvelope, mc)
        monoChains.add(mc)
      }
    }
  }

  def process(intersector: EdgeSegmentIntersector): Unit = {
    val overlapAction: MonotoneChainOverlapAction = new EdgeSegmentOverlapAction(intersector)

    val queryIt = monoChains.iterator
    while (queryIt.hasNext) {
      val queryChain    = queryIt.next
      val overlapChains = index.query(queryChain.getEnvelope)
      val testIt        = overlapChains.iterator
      while (testIt.hasNext) {
        val testChain = testIt.next.asInstanceOf[MonotoneChain]

        /**
         * following test makes sure we only compare each pair of chains once and that we don't
         * compare a chain to itself
         */
        if (testChain.getId > queryChain.getId) {
          testChain.computeOverlaps(queryChain, overlapAction)
          if (intersector.isDone)
            return
        }
      }
    }
  }

}
