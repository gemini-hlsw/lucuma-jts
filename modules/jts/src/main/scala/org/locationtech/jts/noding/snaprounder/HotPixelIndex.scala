// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.noding.snapround

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.PrecisionModel
import org.locationtech.jts.index.kdtree.KdNode
import org.locationtech.jts.index.kdtree.KdNodeVisitor
import org.locationtech.jts.index.kdtree.KdTree

import java.util
import scala.jdk.CollectionConverters._

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
 * An index which creates {@link HotPixel}s for provided points, and allows performing range queries
 * on them.
 *
 * @author
 *   mdavis
 */
class HotPixelIndex(var precModel: PrecisionModel) {
  scaleFactor = precModel.getScale
  private var scaleFactor = .0

  /**
   * Use a kd-tree to index the pixel centers for optimum performance. Since HotPixels have an
   * extent, queries to the index must enlarge the query range by a suitable value (using the pixel
   * width is safest).
   */
  private val index = new KdTree

  /**
   * Adds a list of points as non-node pixels.
   *
   * @param pts
   *   the points to add
   */
  def add(pts: Array[Coordinate]): Unit =
    for (pt <- pts)
      add(pt)

  /**
   * Adds a list of points as node pixels.
   *
   * @param pts
   *   the points to add
   */
  def addNodes(pts: util.List[Coordinate]): Unit =
    for (pt <- pts.asScala) {
      val hp = add(pt)
      hp.setToNode()
    }

  /**
   * Adds a point as a Hot Pixel. If the point has been added already, it is marked as a node.
   *
   * @param p
   *   the point to add
   * @return
   *   the HotPixel for the point
   */
  def add(p: Coordinate): HotPixel = { // TODO: is there a faster way of doing this?
    val pRound = round(p)
    var hp     = find(pRound)

    /**
     * Hot Pixels which are added more than once must have more than one vertex in them and thus
     * must be nodes.
     */
    if (hp != null) {
      hp.setToNode()
      return hp
    }

    /**
     * A pixel containing the point was not found, so create a new one. It is initially set to NOT
     * be a node (but may become one later on).
     */
    hp = new HotPixel(pRound, scaleFactor)
    index.insert(hp.getCoordinate, hp)
    hp
  }

  private def find(pixelPt: Coordinate): HotPixel = {
    val kdNode = index.query(pixelPt)
    if (kdNode == null) return null
    kdNode.getData.asInstanceOf[HotPixel]
  }

  private def round(pt: Coordinate) = {
    val p2 = pt.copy
    precModel.makePrecise(p2)
    p2
  }

  /**
   * Visits all the hot pixels which may intersect a segment (p0-p1). The visitor must determine
   * whether each hot pixel actually intersects the segment.
   *
   * @param p0
   *   the segment start point
   * @param p1
   *   the segment end point
   * @param visitor
   *   the visitor to apply
   */
  def query(p0: Coordinate, p1: Coordinate, visitor: KdNodeVisitor): Unit = {
    val queryEnv = new Envelope(p0, p1)
    // expand query range to account for HotPixel extent
    // expand by full width of one pixel to be safe
    queryEnv.expandBy(1.0 / scaleFactor)
    index.query(queryEnv, visitor)
  }
}
