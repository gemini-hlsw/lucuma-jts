// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.noding.snapround

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.index.ItemVisitor
import org.locationtech.jts.index.SpatialIndex
import org.locationtech.jts.index.chain.MonotoneChain
import org.locationtech.jts.index.chain.MonotoneChainSelectAction
import org.locationtech.jts.index.strtree.STRtree
import org.locationtech.jts.noding.NodedSegmentString
import org.locationtech.jts.noding.SegmentString

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

/**
 * "Snaps" all {@link SegmentString}s in a {@link SpatialIndex} containing {@link MonotoneChain}s to
 * a given {@link HotPixel}.
 *
 * @version 1.7
 */
object MCIndexPointSnapper {
  private val SAFE_ENV_EXPANSION_FACTOR = 0.75

  class HotPixelSnapAction(
    var hotPixel:            HotPixel,
    var parentEdge:          SegmentString, // is -1 if hotPixel is not a vertex
    var hotPixelVertexIndex: Int
  ) extends MonotoneChainSelectAction {
    private var isNodeAdded0 = false

    /**
     * Reports whether the HotPixel caused a node to be added in any target segmentString (including
     * its own). If so, the HotPixel must be added as a node as well.
     *
     * @return
     *   true if a node was added in any target segmentString.
     */
    def isNodeAdded: Boolean = isNodeAdded0

    /**
     * Check if a segment of the monotone chain intersects the hot pixel vertex and introduce a snap
     * node if so. Optimized to avoid noding segments which contain the vertex (which otherwise
     * would cause every vertex to be noded).
     */
    override def select(mc: MonotoneChain, startIndex: Int): Unit = {
      val ss = mc.getContext.asInstanceOf[NodedSegmentString]

      /**
       * Check to avoid snapping a hotPixel vertex to the its orginal vertex. This method is called
       * on segments which intersect the hot pixel. If either end of the segment is equal to the hot
       * pixel do not snap.
       */
      if (parentEdge != null && (ss eq parentEdge))
        if (startIndex == hotPixelVertexIndex || startIndex + 1 == hotPixelVertexIndex) return
      // records if this HotPixel caused any node to be added
      isNodeAdded0 |= addSnappedNode(hotPixel, ss, startIndex)
    }

    /**
     * Adds a new node (equal to the snap pt) to the specified segment if the segment passes through
     * the hot pixel
     *
     * @param segStr
     * @param segIndex
     * @return
     *   true if a node was added to the segment
     */
    def addSnappedNode(hotPixel: HotPixel, segStr: NodedSegmentString, segIndex: Int): Boolean = {
      val p0 = segStr.getCoordinate(segIndex)
      val p1 = segStr.getCoordinate(segIndex + 1)
      if (hotPixel.intersects(p0, p1)) { // System.out.println("snapped: " + snapPt);
        // System.out.println("POINT (" + snapPt.x + " " + snapPt.y + ")");
        segStr.addIntersection(hotPixel.getCoordinate, segIndex)
        return true
      }
      false
    }
  }
}

class MCIndexPointSnapper(val index: SpatialIndex[Any]) {
  private var index0: STRtree = index.asInstanceOf[STRtree]

  /**
   * Snaps (nodes) all interacting segments to this hot pixel. The hot pixel may represent a vertex
   * of an edge, in which case this routine uses the optimization of not noding the vertex itself
   *
   * @param hotPixel
   *   the hot pixel to snap to
   * @param parentEdge
   *   the edge containing the vertex, if applicable, or <code>null</code>
   * @param hotPixelVertexIndex
   *   the index of the hotPixel vertex, if applicable, or -1
   * @return
   *   <code>true</code> if a node was added for this pixel
   */
  def snap(hotPixel: HotPixel, parentEdge: SegmentString, hotPixelVertexIndex: Int): Boolean = {
    val pixelEnv           = getSafeEnvelope(hotPixel)
    val hotPixelSnapAction =
      new MCIndexPointSnapper.HotPixelSnapAction(hotPixel, parentEdge, hotPixelVertexIndex)
    index0.query(
      pixelEnv,
      new ItemVisitor() {
        override def visitItem(item: Any): Unit = {
          val testChain = item.asInstanceOf[MonotoneChain]
          testChain.select(pixelEnv, hotPixelSnapAction)
        }
      }
    )
    hotPixelSnapAction.isNodeAdded
  }

  def snap(hotPixel: HotPixel): Boolean = snap(hotPixel, null, -1)

  /**
   * Returns a "safe" envelope that is guaranteed to contain the hot pixel. The envelope returned is
   * larger than the exact envelope of the pixel by a safe margin.
   *
   * @return
   *   an envelope which contains the hot pixel
   */
  def getSafeEnvelope(hp: HotPixel): Envelope = {
    val safeTolerance = MCIndexPointSnapper.SAFE_ENV_EXPANSION_FACTOR / hp.getScaleFactor
    val safeEnv       = new Envelope(hp.getCoordinate)
    safeEnv.expandBy(safeTolerance)
    safeEnv
  }
}
