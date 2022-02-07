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
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geomgraph.Quadrant
import org.locationtech.jts.util.IntArrayList

/**
 * MonotoneChains are a way of partitioning the segments of an edge to allow for fast searching of
 * intersections. Specifically, a sequence of contiguous line segments is a monotone chain iff all
 * the vectors defined by the oriented segments lies in the same quadrant. <p> Monotone Chains have
 * the following useful properties: <ol> <li>the segments within a monotone chain will never
 * intersect each other <li>the envelope of any contiguous subset of the segments in a monotone
 * chain is simply the envelope of the endpoints of the subset. </ol> Property 1 means that there is
 * no need to test pairs of segments from within the same monotone chain for intersection. Property
 * 2 allows binary search to be used to find the intersection points of two monotone chains. For
 * many types of real-world data, these properties eliminate a large number of segment comparisons,
 * producing substantial speed gains. <p> Note that due to the efficient intersection test, there is
 * no need to limit the size of chains to obtain fast performance.
 *
 * @version 1.7
 */
object MonotoneChainIndexer {
  def toIntArray(list: util.List[Int]): Array[Int] = {
    val array = new Array[Int](list.size)
    var i     = 0
    while (i < array.length) {
      array(i) = list.get(i).asInstanceOf[Integer].intValue
      i += 1; i - 1
    }
    array
  }
}

class MonotoneChainIndexer() {
  def getChainStartIndices(pts: Array[Coordinate]): Array[Int] = { // find the startpoint (and endpoints) of all monotone chains in this edge
    var start          = 0
    val startIndexList = new IntArrayList(pts.length / 2)
    // use heuristic to size initial array
    // startIndexList.ensureCapacity(pts.length / 4);
    startIndexList.add(start)
    while ({
      {
        val last = findChainEnd(pts, start)
        startIndexList.add(last)
        start = last
      }; start < pts.length - 1
    }) ()
    // copy list to an array of ints, for efficiency
    startIndexList.toArray
  }

  def OLDgetChainStartIndices(pts: Array[Coordinate]): Array[Int] = {
    var start          = 0
    val startIndexList = new util.ArrayList[Int]
    startIndexList.add(start)
    while ({
      {
        val last = findChainEnd(pts, start)
        startIndexList.add(last)
        start = last
      }; start < pts.length - 1
    }) ()
    val startIndex     = MonotoneChainIndexer.toIntArray(startIndexList)
    startIndex
  }

  /**
   * return the index of the last point in the monotone chain
   */
  private def findChainEnd(pts: Array[Coordinate], start: Int): Int = { // determine quadrant for chain
    val chainQuad = Quadrant.quadrant(pts(start), pts(start + 1))
    var last      = start + 1
    while (last < pts.length) { // if (last - start > 100) break;
      // compute quadrant for next possible segment in chain
      val quad = Quadrant.quadrant(pts(last - 1), pts(last))
      if (quad != chainQuad) {
        return last - 1
      }
      last += 1
    }
    last - 1
  }
}
