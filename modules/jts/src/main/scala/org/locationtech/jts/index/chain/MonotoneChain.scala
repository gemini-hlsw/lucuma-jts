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
package org.locationtech.jts.index.chain

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.LineSegment

/**
 * Monotone Chains are a way of partitioning the segments of a linestring to allow for fast
 * searching of intersections. They have the following properties: <ol> <li>the segments within a
 * monotone chain never intersect each other <li>the envelope of any contiguous subset of the
 * segments in a monotone chain is equal to the envelope of the endpoints of the subset. </ol>
 * Property 1 means that there is no need to test pairs of segments from within the same monotone
 * chain for intersection. <p> Property 2 allows an efficient binary search to be used to find the
 * intersection points of two monotone chains. For many types of real-world data, these properties
 * eliminate a large number of segment comparisons, producing substantial speed gains. <p> One of
 * the goals of this implementation of MonotoneChains is to be as space and time efficient as
 * possible. One design choice that aids this is that a MonotoneChain is based on a subarray of a
 * list of points. This means that new arrays of points (potentially very large) do not have to be
 * allocated. <p>
 *
 * MonotoneChains support the following kinds of queries: <ul> <li>Envelope select: determine all
 * the segments in the chain which intersect a given envelope <li>Overlap: determine all the pairs
 * of segments in two chains whose envelopes overlap </ul>
 *
 * This implementation of MonotoneChains uses the concept of internal iterators ({link
 * MonotoneChainSelectAction} and {link MonotoneChainOverlapAction}) to return the results for
 * queries. This has time and space advantages, since it is not necessary to build lists of
 * instantiated objects to represent the segments returned by the query. Queries made in this manner
 * are thread-safe.
 *
 * MonotoneChains support being assigned an integer id value to provide a total ordering for a set
 * of chains. This can be used during some kinds of processing to avoid redundant comparisons (i.e.
 * by comparing only chains where the first id is less than the second).
 *
 * @version 1.7
 */
class MonotoneChain(var pts: Array[Coordinate], val start: Int, val end: Int, val context: Any) {

  /**
   * Creates a new MonotoneChain based on the given array of points.
   *
   * @param pts
   *   the points containing the chain
   * @param start
   *   the index of the first coordinate in the chain
   * @param end
   *   the index of the last coordinate in the chain
   * @param context
   *   a user-defined data object
   */
  private var env: Envelope = null
//  private val context = null // user-defined information
  private var id            = 0 // useful for optimizing chain comparisons
  /**
   * Sets the id of this chain. Useful for assigning an ordering to a set of chains, which can be
   * used to avoid redundant processing.
   *
   * @param id
   *   an id value
   */
  def setId(id: Int): Unit  = this.id = id

  /**
   * Gets the id of this chain.
   *
   * return the id value
   */
  def getId: Int = id

  /**
   * Gets the user-defined context data value.
   *
   * return a data value
   */
  def getContext: Any = context

  /**
   * Gets the envelope of the chain.
   *
   * return the envelope of the chain
   */
  def getEnvelope: Envelope =
    getEnvelope(0.0)

  def getEnvelope(expansionDistance: Double): Envelope = {
    if (env == null) {

      /**
       * The monotonicity property allows fast envelope determination
       */
      val p0 = pts(start)
      val p1 = pts(end)
      env = new Envelope(p0, p1)
      if (expansionDistance > 0.0)
        env.expandBy(expansionDistance)
    }
    env
  }

  /**
   * Gets the index of the start of the monotone chain in the underlying array of points.
   *
   * return the start index of the chain
   */
  def getStartIndex: Int = start

  /**
   * Gets the index of the end of the monotone chain in the underlying array of points.
   *
   * return the end index of the chain
   */
  def getEndIndex: Int = end

  /**
   * Gets the line segment starting at <code>index</code>
   *
   * @param index
   *   index of segment
   * @param ls
   *   line segment to extract into
   */
  def getLineSegment(index: Int, ls: LineSegment): Unit = {
    ls.p0 = pts(index)
    ls.p1 = pts(index + 1)
  }

  /**
   * Return the subsequence of coordinates forming this chain. Allocates a new array to hold the
   * Coordinates
   */
  def getCoordinates: Array[Coordinate] = {
    val coord = new Array[Coordinate](end - start + 1)
    var index = 0
    var i     = start
    while (i <= end) {
      coord {
        index += 1; index - 1
      } = pts(i)
      i += 1
    }
    coord
  }

  /**
   * Determine all the line segments in the chain whose envelopes overlap the searchEnvelope, and
   * process them. <p> The monotone chain search algorithm attempts to optimize performance by not
   * calling the select action on chain segments which it can determine are not in the search
   * envelope. However, it *may* call the select action on segments which do not intersect the
   * search envelope. This saves on the overhead of checking envelope intersection each time, since
   * clients may be able to do this more efficiently.
   *
   * @param searchEnv
   *   the search envelope
   * @param mcs
   *   the select action to execute on selected segments
   */
  def select(searchEnv: Envelope, mcs: MonotoneChainSelectAction): Unit =
    computeSelect(searchEnv, start, end, mcs)

  private def computeSelect(
    searchEnv: Envelope,
    start0:    Int,
    end0:      Int,
    mcs:       MonotoneChainSelectAction
  ): Unit = {
    val p0  = pts(start0)
    val p1  = pts(end0)
    // Debug.println("trying:" + p0 + p1 + " [ " + start0 + ", " + end0 + " ]");
    // terminating condition for the recursion
    if (end0 - start0 == 1) { // Debug.println("computeSelect:" + p0 + p1);
      mcs.select(this, start0)
      return
    }
    // nothing to do if the envelopes don't overlap
    if (!searchEnv.intersects(p0, p1)) return
    // the chains overlap, so split each in half and iterate  (binary search)
    val mid = (start0 + end0) / 2
    // Assert: mid != start or end (since we checked above for end - start <= 1)
    // check terminating conditions before recursing
    if (start0 < mid) computeSelect(searchEnv, start0, mid, mcs)
    if (mid < end0) computeSelect(searchEnv, mid, end0, mcs)
  }

  /**
   * Determine all the line segments in two chains which may overlap, and process them. <p> The
   * monotone chain search algorithm attempts to optimize performance by not calling the overlap
   * action on chain segments which it can determine do not overlap. However, it *may* call the
   * overlap action on segments which do not actually interact. This saves on the overhead of
   * checking intersection each time, since clients may be able to do this more efficiently.
   *
   * @param searchEnv
   *   the search envelope
   * @param mco
   *   the overlap action to execute on selected segments
   */
  def computeOverlaps(mc: MonotoneChain, mco: MonotoneChainOverlapAction): Unit =
    computeOverlaps0(start, end, mc, mc.start, mc.end, 0.0, mco)

  /**
   * Determines the line segments in two chains which may overlap, using an overlap distance
   * tolerance, and passes them to an overlap action.
   *
   * @param mc
   *   the chain to compare to
   * @param overlapTolerance
   *   the distance tolerance for the overlap test
   * @param mco
   *   the overlap action to execute on selected segments
   */
  def computeOverlaps(
    mc:               MonotoneChain,
    overlapTolerance: Double,
    mco:              MonotoneChainOverlapAction
  ) =
    computeOverlaps0(start, end, mc, mc.start, mc.end, overlapTolerance, mco)

  /**
   * Uses an efficient mutual binary search strategy to determine which pairs of chain segments may
   * overlap, and calls the given overlap action on them.
   *
   * @param start0
   *   the start index of this chain section
   * @param end0
   *   the end index of this chain section
   * @param mc
   *   the target monotone chain
   * @param start1
   *   the start index of the target chain section
   * @param end1
   *   the end index of the target chain section
   * @param mco
   *   the overlap action to execute on selected segments
   */
  private def computeOverlaps0(
    start0:           Int,
    end0:             Int,
    mc:               MonotoneChain,
    start1:           Int,
    end1:             Int,
    overlapTolerance: Double,
    mco:              MonotoneChainOverlapAction
  ): Unit = { // Debug.println("computeIntersectsForChain:" + p00 + p01 + p10 + p11);
    if (end0 - start0 == 1 && end1 - start1 == 1) {
      mco.overlap(this, start0, mc, start1)
      return
    }
    // nothing to do if the envelopes of these subchains don't overlap
    if (!overlaps(start0, end0, mc, start1, end1, overlapTolerance)) return
    val mid0 = (start0 + end0) / 2
    val mid1 = (start1 + end1) / 2
    if (start0 < mid0) {
      if (start1 < mid1) computeOverlaps0(start0, mid0, mc, start1, mid1, overlapTolerance, mco)
      if (mid1 < end1) computeOverlaps0(start0, mid0, mc, mid1, end1, overlapTolerance, mco)
    }
    if (mid0 < end0) {
      if (start1 < mid1) computeOverlaps0(mid0, end0, mc, start1, mid1, overlapTolerance, mco)
      if (mid1 < end1) computeOverlaps0(mid0, end0, mc, mid1, end1, overlapTolerance, mco)
    }
  }

  /**
   * Tests whether the envelope of a section of the chain overlaps (intersects) the envelope of a
   * section of another target chain. This test is efficient due to the monotonicity property of the
   * sections (i.e. the envelopes can be are determined from the section endpoints rather than a
   * full scan).
   *
   * @param start0
   *   the start index of this chain section
   * @param end0
   *   the end index of this chain section
   * @param mc
   *   the target monotone chain
   * @param start1
   *   the start index of the target chain section
   * @param end1
   *   the end index of the target chain section return true if the section envelopes overlap
   */
  private def overlaps(
    start0:           Int,
    end0:             Int,
    mc:               MonotoneChain,
    start1:           Int,
    end1:             Int,
    overlapTolerance: Double
  ) =
    if (overlapTolerance > 0.0) {
      overlaps0(pts(start0), pts(end0), mc.pts(start1), mc.pts(end1), overlapTolerance);
    } else
      Envelope.intersects(pts(start0), pts(end0), mc.pts(start1), mc.pts(end1))

  private def overlaps0(
    p1:               Coordinate,
    p2:               Coordinate,
    q1:               Coordinate,
    q2:               Coordinate,
    overlapTolerance: Double
  ): Boolean = {
    var minq: Double = Math.min(q1.x, q2.x)
    var maxq: Double = Math.max(q1.x, q2.x)
    var minp: Double = Math.min(p1.x, p2.x)
    var maxp: Double = Math.max(p1.x, p2.x)
    if (minp > maxq + overlapTolerance) {
      return false
    }
    if (maxp < minq - overlapTolerance) {
      return false
    }
    minq = Math.min(q1.y, q2.y)
    maxq = Math.max(q1.y, q2.y)
    minp = Math.min(p1.y, p2.y)
    maxp = Math.max(p1.y, p2.y)
    if (minp > maxq + overlapTolerance) {
      return false
    }
    if (maxp < minq - overlapTolerance) {
      return false
    }
    return true
  }
}
