// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.noding.snapround

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateList
import org.locationtech.jts.geom.PrecisionModel
import org.locationtech.jts.index.kdtree.KdNode
import org.locationtech.jts.index.kdtree.KdNodeVisitor
import org.locationtech.jts.noding.MCIndexNoder
import org.locationtech.jts.noding.NodedSegmentString
import org.locationtech.jts.noding.Noder
import org.locationtech.jts.noding.SegmentString
import org.locationtech.jts.noding.snap.SnappingNoder

import java.util
import scala.jdk.CollectionConverters.*

/*
 * Copyright (c) 2019 Martin Davis.
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
 * Uses Snap Rounding to compute a rounded, fully noded arrangement from a set of {@link
 * SegmentString}s, in a performant way, and avoiding unnecessary noding. <p> Implements the Snap
 * Rounding technique described in the papers by Hobby, Guibas &amp; Marimont, and Goodrich et al.
 * Snap Rounding enforces that all output vertices lie on a uniform grid, which is determined by the
 * provided {@link PrecisionModel} . <p> Input vertices do not have to be rounded to the grid
 * beforehand; this is done during the snap-rounding process. In fact, rounding cannot be done a
 * priori, since rounding vertices by themselves can distort the rounded topology of the arrangement
 * (i.e. by moving segments away from hot pixels that would otherwise intersect them, or by moving
 * vertices across segments). <p> To minimize the number of introduced nodes, the Snap-Rounding
 * Noder avoids creating nodes at edge vertices if there is no intersection or snap at that
 * location. However, if two different input edges contain identical segments, each of the segment
 * vertices will be noded. This still provides fully-noded output. This is the same behaviour
 * provided by other noders, such as {@link MCIndexNoder} and {@link SnappingNoder} .
 *
 * @version 1.7
 */
class SnapRoundingNoder(val pm: PrecisionModel) extends Noder[SegmentString] {
  private var pixelIndex: HotPixelIndex                    = new HotPixelIndex(pm)
  private var snappedResult: util.List[NodedSegmentString] = null

  /**
   * @return
   *   a Collection of NodedSegmentStrings representing the substrings
   */
  override def getNodedSubstrings: util.Collection[SegmentString] =
    NodedSegmentString.getNodedSubstrings(snappedResult.asScala.collect { case n: SegmentString =>
      n: SegmentString
    }.asJavaCollection)

  /**
   * Computes the nodes in the snap-rounding line arrangement. The nodes are added to the {@link
   * NodedSegmentString}s provided as the input.
   *
   * @param inputSegmentStrings
   *   a Collection of NodedSegmentStrings
   */
  override def computeNodes(inputSegmentStrings: util.Collection[SegmentString]): Unit =
    snappedResult = snapRound(inputSegmentStrings.asScala.collect { case n: NodedSegmentString =>
      n: NodedSegmentString
    }.asJavaCollection)

  private def snapRound(segStrings: util.Collection[NodedSegmentString]) = {

    /**
     * Determine hot pixels for intersections and vertices. This is done BEFORE the input lines are
     * rounded, to avoid distorting the line arrangement (rounding can cause vertices to move across
     * edges).
     */
    addIntersectionPixels(segStrings)
    addVertexPixels(segStrings)
    val snapped = computeSnaps(segStrings)
    snapped
  }

  /**
   * Detects interior intersections in the collection of {@link SegmentString} s, and adds nodes for
   * them to the segment strings. Also creates HotPixel nodes for the intersection points.
   *
   * @param segStrings
   *   the input NodedSegmentStrings
   */
  private def addIntersectionPixels(segStrings: util.Collection[NodedSegmentString]): Unit = {
    val intAdder = new SnapRoundingIntersectionAdder(pm)
    val noder    = new MCIndexNoder
    noder.setSegmentIntersector(intAdder)
    noder.computeNodes(segStrings.asScala.collect { case n: SegmentString =>
      n: SegmentString
    }.asJavaCollection)
    val intPts   = intAdder.getIntersections
    pixelIndex.addNodes(intPts)
  }

  /**
   * Creates HotPixels for each vertex in the input segStrings. The HotPixels are not marked as
   * nodes, since they will only be nodes in the final line arrangement if they interact with other
   * segments (or they are already created as intersection nodes).
   *
   * @param segStrings
   *   the input NodedSegmentStrings
   */
  private def addVertexPixels(segStrings: util.Collection[NodedSegmentString]): Unit =
    for (nss <- segStrings.asScala) {
      val pts = nss.getCoordinates
      pixelIndex.add(pts)
    }

  private def round(pt: Coordinate): Coordinate = {
    val p2 = pt.copy
    pm.makePrecise(p2)
    p2
  }

  /**
   * Gets a list of the rounded coordinates. Duplicate (collapsed) coordinates are removed.
   *
   * @param pts
   *   the coordinates to round
   * @return
   *   array of rounded coordinates
   */
  private def round(pts: Array[Coordinate]): Array[Coordinate] = {
    val roundPts = new CoordinateList
    for (i <- 0 until pts.length)
      roundPts.add(round(pts(i)), false)
    roundPts.toCoordinateArray
  }

  /**
   * Computes new segment strings which are rounded and contain intersections added as a result of
   * snapping segments to snap points (hot pixels).
   *
   * @param segStrings
   *   segments to snap
   * @return
   *   the snapped segment strings
   */
  private def computeSnaps(segStrings: util.Collection[NodedSegmentString]) = {
    val snapped = new util.ArrayList[NodedSegmentString]
    for (ss <- segStrings.asScala) {
      val snappedSS = computeSegmentSnaps(ss)
      if (snappedSS != null) snapped.add(snappedSS)
    }

    /**
     * Some intersection hot pixels may have been marked as nodes in the previous loop, so add nodes
     * for them.
     */
    for (ss <- snapped.asScala)
      addVertexNodeSnaps(ss)
    snapped
  }

  /**
   * Add snapped vertices to a segment string. If the segment string collapses completely due to
   * rounding, null is returned.
   *
   * @param ss
   *   the segment string to snap
   * @return
   *   the snapped segment string, or null if it collapses completely
   */
  private def computeSegmentSnaps(ss: NodedSegmentString): NodedSegmentString = { // Coordinate[] pts = ss.getCoordinates();
    /**
     * Get edge coordinates, including added intersection nodes. The coordinates are now rounded to
     * the grid, in preparation for snapping to the Hot Pixels
     */
    val pts         = ss.getNodedCoordinates
    val ptsRound    = round(pts)
    // if complete collapse this edge can be eliminated
    if (ptsRound.length <= 1) return null
    // Create new nodedSS to allow adding any hot pixel nodes
    val snapSS      = new NodedSegmentString(ptsRound, ss.getData)
    var snapSSindex = 0
    for (i <- 0 until pts.length - 1) {
      val currSnap = snapSS.getCoordinate(snapSSindex)

      /**
       * If the segment has collapsed completely, skip it
       */
      val p1      = pts(i + 1)
      val p1Round = round(p1)
      if (!p1Round.equals2D(currSnap)) {
        val p0 = pts(i)

        /**
         * Add any Hot Pixel intersections with *original* segment to rounded segment. (It is
         * important to check original segment because rounding can move it enough to intersect
         * other hot pixels not intersecting original segment)
         */
        snapSegment(p0, p1, snapSS, snapSSindex)
        snapSSindex += 1
      }
    }
    snapSS
  }

  /**
   * Snaps a segment in a segmentString to HotPixels that it intersects.
   *
   * @param p0
   *   the segment start coordinate
   * @param p1
   *   the segment end coordinate
   * @param ss
   *   the segment string to add intersections to
   * @param segIndex
   *   the index of the segment
   */
  private def snapSegment(
    p0:       Coordinate,
    p1:       Coordinate,
    ss:       NodedSegmentString,
    segIndex: Int
  ): Unit =
    pixelIndex.query(
      p0,
      p1,
      new KdNodeVisitor() {
        override def visit(node: KdNode): Unit = {
          val hp = node.getData.asInstanceOf[HotPixel]

          /**
           * If the hot pixel is not a node, and it contains one of the segment vertices, then that
           * vertex is the source for the hot pixel. To avoid over-noding a node is not added at
           * this point. The hot pixel may be subsequently marked as a node, in which case the
           * intersection will be added during the final vertex noding phase.
           */
          if (!hp.isNode) if (hp.intersects(p0) || hp.intersects(p1)) return

          /**
           * Add a node if the segment intersects the pixel. Mark the HotPixel as a node (since it
           * may not have been one before). This ensures the vertex for it is added as a node during
           * the final vertex noding phase.
           */
          if (hp.intersects(p0, p1)) { // System.out.println("Added intersection: " + hp.getCoordinate());
            ss.addIntersection(hp.getCoordinate, segIndex)
            hp.setToNode()
          }
        }
      }
    )

  /**
   * Add nodes for any vertices in hot pixels that were added as nodes during segment noding.
   *
   * @param ss
   *   a noded segment string
   */
  private def addVertexNodeSnaps(ss: NodedSegmentString): Unit = {
    val pts = ss.getCoordinates
    for (i <- 1 until pts.length - 1) {
      val p0 = pts(i)
      snapVertexNode(p0, ss, i)
    }
  }

  private def snapVertexNode(p0: Coordinate, ss: NodedSegmentString, segIndex: Int): Unit =
    pixelIndex.query(
      p0,
      p0,
      new KdNodeVisitor() {
        override def visit(node: KdNode): Unit = {
          val hp = node.getData.asInstanceOf[HotPixel]

          /**
           * If vertex pixel is a node, add it.
           */
          if (hp.isNode && hp.getCoordinate.equals2D(p0)) ss.addIntersection(p0, segIndex)
        }
      }
    )
}
