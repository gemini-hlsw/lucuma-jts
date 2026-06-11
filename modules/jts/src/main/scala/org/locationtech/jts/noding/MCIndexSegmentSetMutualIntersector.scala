// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2016 Vivid Solutions.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v20.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.noding

import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.index.SpatialIndex
import org.locationtech.jts.index.chain.MonotoneChain
import org.locationtech.jts.index.chain.MonotoneChainBuilder
import org.locationtech.jts.index.chain.MonotoneChainOverlapAction
import org.locationtech.jts.index.strtree.STRtree

import java.util

object MCIndexSegmentSetMutualIntersector {

  class SegmentOverlapAction(private val si: SegmentIntersector)
      extends MonotoneChainOverlapAction {

    override def overlap(mc1: MonotoneChain, start1: Int, mc2: MonotoneChain, start2: Int)
      : Unit = {
      val ss1 = mc1.getContext.asInstanceOf[SegmentString]
      val ss2 = mc2.getContext.asInstanceOf[SegmentString]
      si.processIntersections(ss1, start1, ss2, start2)
    }
  }
}

/**
 * Intersects two sets of {link SegmentString}s using a index based on {link MonotoneChain}s and a
 * {link SpatialIndex}.
 *
 * Thread-safe and immutable.
 *
 * @version 1.7
 */
class MCIndexSegmentSetMutualIntersector private (
  baseSegStrings:   util.Collection[SegmentString],
  envelope:         Envelope,
  overlapTolerance: Double
) extends SegmentSetMutualIntersector {

  /**
   * The {link SpatialIndex} used should be something that supports envelope (range) queries
   * efficiently (such as an STRtree).
   */
  private val index = new STRtree

  initBaseSegments(baseSegStrings)

  /**
   * Constructs a new intersector for a given set of {link SegmentString}s.
   *
   * @param baseSegStrings
   *   the base segment strings to intersect
   */
  def this(baseSegStrings: util.Collection[SegmentString]) =
    this(baseSegStrings, null, 0.0)

  def this(baseSegStrings: util.Collection[SegmentString], env: Envelope) =
    this(baseSegStrings, env, 0.0)

  def this(baseSegStrings: util.Collection[SegmentString], overlapTolerance: Double) =
    this(baseSegStrings, null, overlapTolerance)

  /**
   * Gets the index constructed over the base segment strings.
   *
   * NOTE: To retain thread-safety, treat returned value as immutable!
   *
   * return the constructed index
   */
  def getIndex: SpatialIndex[Any] = index

  private def initBaseSegments(segStrings: util.Collection[SegmentString]): Unit = {
    val i = segStrings.iterator
    while (i.hasNext) {
      val ss = i.next
      if (ss.size != 0) addToIndex(ss)
    }
    // build index to ensure thread-safety
    index.build()
  }

  private def addToIndex(segStr: SegmentString): Unit = {
    val segChains = MonotoneChainBuilder.getChains(segStr.getCoordinates, segStr)
    val i         = segChains.iterator
    while (i.hasNext) {
      val mc = i.next
      if (envelope == null || envelope.intersects(mc.getEnvelope))
        index.insert(mc.getEnvelope(overlapTolerance), mc)
    }
  }

  /**
   * Calls {link SegmentIntersector#processIntersections(SegmentString, int, SegmentString, int)}
   * for all <i>candidate</i> intersections between the given collection of SegmentStrings and the
   * set of indexed segments.
   *
   * @param segStrings
   *   set of segments to intersect
   * @param segInt
   *   segment intersector to use
   */
  override def process(
    segStrings: util.Collection[SegmentString],
    segInt:     SegmentIntersector
  ): Unit = {
    val monoChains = new util.ArrayList[MonotoneChain]
    val i          = segStrings.iterator
    while (i.hasNext) addToMonoChains(i.next, monoChains)
    intersectChains(monoChains, segInt)
  }

  private def addToMonoChains(segStr: SegmentString, monoChains: util.List[MonotoneChain])
    : Unit = {
    if (segStr.size == 0) return
    val segChains = MonotoneChainBuilder.getChains(segStr.getCoordinates, segStr)
    val i         = segChains.iterator
    while (i.hasNext) {
      val mc = i.next
      if (envelope == null || envelope.intersects(mc.getEnvelope)) {
        monoChains.add(mc)
        ()
      }
    }
  }

  private def intersectChains(
    monoChains: util.List[MonotoneChain],
    segInt:     SegmentIntersector
  ): Unit = {
    val overlapAction = new MCIndexSegmentSetMutualIntersector.SegmentOverlapAction(segInt)

    val i = monoChains.iterator
    while (i.hasNext) {
      val queryChain    = i.next
      val queryEnv      = queryChain.getEnvelope(overlapTolerance)
      val overlapChains = index.query(queryEnv)
      val j             = overlapChains.iterator
      while (j.hasNext) {
        val testChain = j.next.asInstanceOf[MonotoneChain]
        queryChain.computeOverlaps(testChain, overlapTolerance, overlapAction)
        if (segInt.isDone) return
      }
    }
  }
}
