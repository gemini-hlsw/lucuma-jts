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

import java.util
import org.locationtech.jts.index.chain.MonotoneChain
import org.locationtech.jts.index.chain.MonotoneChainBuilder
import org.locationtech.jts.index.chain.MonotoneChainOverlapAction
import org.locationtech.jts.index.strtree.STRtree
import scala.jdk.CollectionConverters._

/**
 * Nodes a set of {link SegmentString}s using a index based on {link MonotoneChain}s and a {link
 * SpatialIndex}. The {link SpatialIndex} used should be something that supports envelope (range)
 * queries efficiently (such as a <code>Quadtree</code>} or {link STRtree} (which is the default
 * index provided).
 *
 * @version 1.7
 */
object MCIndexNoder {

  class SegmentOverlapAction(val si: SegmentIntersector) extends MonotoneChainOverlapAction {

    override def overlap(mc1: MonotoneChain, start1: Int, mc2: MonotoneChain, start2: Int): Unit = {
      val ss1 = mc1.getContext.asInstanceOf[SegmentString]
      val ss2 = mc2.getContext.asInstanceOf[SegmentString]
      si.processIntersections(ss1, start1, ss2, start2)
    }
  }

}

class MCIndexNoder(si: SegmentIntersector) extends SinglePassNoder[SegmentString](si) {
  private val monoChains                                      = new util.ArrayList[MonotoneChain]
  private val index                                           = new STRtree
  private var idCounter                                       = 0
  private var nodedSegStrings: util.Collection[SegmentString] = null
  // statistics
  private var nOverlaps                                       = 0
  def this() = this(null)

  def getMonotoneChains: util.ArrayList[MonotoneChain] = monoChains

  def getIndex: STRtree = index

  override def getNodedSubstrings: util.List[SegmentString] =
    NodedSegmentString
      .getNodedSubstrings(nodedSegStrings)
      .asScala
      .map(x => x: SegmentString)
      .toList
      .asJava

  override def computeNodes(inputSegStrings: util.Collection[SegmentString]): Unit = {
    this.nodedSegStrings =
      inputSegStrings // .asScala.map(x => x.asInstanceOf[NodedSegmentString]).toList.asJava
    val i = inputSegStrings.iterator
    while (i.hasNext) add(i.next)
    intersectChains()
    // System.out.println("MCIndexNoder: # chain overlaps = " + nOverlaps);
  }

  private def intersectChains(): Unit = {
    val overlapAction = new MCIndexNoder.SegmentOverlapAction(segInt)
    val i             = monoChains.iterator
    while (i.hasNext) {
      val queryChain    = i.next
      val overlapChains = index.query(queryChain.getEnvelope)
      val j             = overlapChains.iterator
      while (j.hasNext) {
        val testChain = j.next.asInstanceOf[MonotoneChain]

        /**
         * following test makes sure we only compare each pair of chains once and that we don't
         * compare a chain to itself
         */
        if (testChain.getId > queryChain.getId) {
          queryChain.computeOverlaps(testChain, overlapAction)
          nOverlaps += 1
        }
        // short-circuit if possible
        if (segInt.isDone) return
      }
    }
  }

  private def add(segStr: SegmentString): Unit = {
    val segChains = MonotoneChainBuilder.getChains(segStr.getCoordinates, segStr)
    val i         = segChains.iterator
    while (i.hasNext) {
      val mc = i.next
      mc.setId(idCounter)
      idCounter += 1
      index.insert(mc.getEnvelope, mc)
      monoChains.add(mc)
    }
  }
}
