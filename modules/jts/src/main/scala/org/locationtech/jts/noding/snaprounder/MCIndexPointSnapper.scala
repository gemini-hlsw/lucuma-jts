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
package org.locationtech.jts.noding.snaprounder

import org.locationtech.jts.index.SpatialIndex
import org.locationtech.jts.index.chain.MonotoneChain
import org.locationtech.jts.index.chain.MonotoneChainSelectAction
import org.locationtech.jts.index.strtree.STRtree
import org.locationtech.jts.noding.NodedSegmentString
import org.locationtech.jts.noding.SegmentString

/**
 * "Snaps" all {link SegmentString}s in a {link SpatialIndex} containing {link MonotoneChain}s to a
 * given {link HotPixel}.
 *
 * @version 1.7
 */
object MCIndexPointSnapper {

  class HotPixelSnapAction(
    var hotPixel:            HotPixel,
    var parentEdge:          SegmentString, // is -1 if hotPixel is not a vertex
    var hotPixelVertexIndex: Int
  ) extends MonotoneChainSelectAction {
    private var visNodeAdded = false

    /**
     * Reports whether the HotPixel caused a node to be added in any target segmentString (including
     * its own). If so, the HotPixel must be added as a node as well.
     *
     * return true if a node was added in any target segmentString.
     */
    def isNodeAdded: Boolean = visNodeAdded

    /**
     * Check if a segment of the monotone chain intersects the hot pixel vertex and introduce a snap
     * node if so. Optimized to avoid noding segments which contain the vertex (which otherwise
     * would cause every vertex to be noded).
     */
    override def select(mc: MonotoneChain, startIndex: Int): Unit = {
      val ss = mc.getContext.asInstanceOf[NodedSegmentString]

      /**
       * Check to avoid snapping a hotPixel vertex to the same vertex. This method is called for
       * segments which intersects the hot pixel, so need to check if either end of the segment is
       * equal to the hot pixel and if so, do not snap.
       *
       * Sep 22 2012 - MD - currently do need to snap to every vertex, since otherwise the
       * testCollapse1 test in SnapRoundingTest fails.
       */
      if (parentEdge eq ss) { // exit if hotpixel is equal to endpoint of target segment
        if (startIndex == hotPixelVertexIndex || startIndex + 1 == hotPixelVertexIndex) return
      }
      // snap and record if a node was created
      visNodeAdded |= hotPixel.addSnappedNode(ss, startIndex)
    }
  }

}

class MCIndexPointSnapper(val vindex: SpatialIndex[Any]) {
  private val index = vindex.asInstanceOf[STRtree]

  /**
   * Snaps (nodes) all interacting segments to this hot pixel. The hot pixel may represent a vertex
   * of an edge, in which case this routine uses the optimization of not noding the vertex itself
   *
   * @param hotPixel
   *   the hot pixel to snap to
   * @param parentEdge
   *   the edge containing the vertex, if applicable, or <code>null</code>
   * @param hotPixelVertexIndex
   *   the index of the hotPixel vertex, if applicable, or -1 return <code>true</code> if a node was
   *   added for this pixel
   */
  def snap(hotPixel: HotPixel, parentEdge: SegmentString, hotPixelVertexIndex: Int): Boolean = {

    val pixelEnv           = hotPixel.getSafeEnvelope
    val hotPixelSnapAction =
      new MCIndexPointSnapper.HotPixelSnapAction(hotPixel, parentEdge, hotPixelVertexIndex)
    index.query(pixelEnv,
                (item: Any) => {
                  val testChain = item.asInstanceOf[MonotoneChain]
                  testChain.select(pixelEnv, hotPixelSnapAction)
                }
    )
    hotPixelSnapAction.isNodeAdded
  }

  def snap(hotPixel: HotPixel): Boolean = snap(hotPixel, null, -1)
}
