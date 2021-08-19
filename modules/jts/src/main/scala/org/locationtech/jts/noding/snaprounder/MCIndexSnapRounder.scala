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

import java.util

import org.locationtech.jts.algorithm.LineIntersector
import org.locationtech.jts.algorithm.RobustLineIntersector
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.PrecisionModel
import org.locationtech.jts.noding.{
  InteriorIntersectionFinderAdder,
  MCIndexNoder,
  NodedSegmentString,
  Noder,
  SegmentString
}

//import scala.jdk.CollectionConverters._

/**
 * Uses Snap Rounding to compute a rounded, fully noded arrangement from a set of {link
 * SegmentString}s. Implements the Snap Rounding technique described in papers by Hobby, Guibas
 * &amp; Marimont, and Goodrich et al. Snap Rounding assumes that all vertices lie on a uniform
 * grid; hence the precision model of the input must be fixed precision, and all the input vertices
 * must be rounded to that precision. <p> This implementation uses a monotone chains and a spatial
 * index to speed up the intersection tests. <p> This implementation appears to be fully robust
 * using an integer precision model. It will function with non-integer precision models, but the
 * results are not 100% guaranteed to be correctly noded.
 *
 * @version 1.7
 */
class MCIndexSnapRounder(val pm: PrecisionModel) extends Noder[SegmentString] {
  private val li                                              = new RobustLineIntersector
  li.setPrecisionModel(pm)
  final private val scaleFactor                               = pm.getScale
  private var noder: MCIndexNoder                             = null
  private var pointSnapper: MCIndexPointSnapper               = null
  private var nodedSegStrings: util.Collection[SegmentString] = null

  override def getNodedSubstrings: util.List[SegmentString] =
    NodedSegmentString.getNodedSubstrings(
      nodedSegStrings
    ) //.asScala.map(x => x:SegmentString).toList.asJava

  override def computeNodes(inputSegmentStrings: util.Collection[SegmentString]): Unit = {
    this.nodedSegStrings =
      inputSegmentStrings //.asScala.map(x => x.asInstanceOf[NodedSegmentString]).toList.asJava
    noder = new MCIndexNoder
    pointSnapper = new MCIndexPointSnapper(noder.getIndex)
    snapRound(inputSegmentStrings, li)
    // testing purposes only - remove in final version
    //checkCorrectness(inputSegmentStrings);
  }

//  private def checkCorrectness(inputSegmentStrings: util.Collection[NodedSegmentString]): Unit = {
//    val resultSegStrings = NodedSegmentString.getNodedSubstrings(inputSegmentStrings)
//    val nv = new NodingValidator(resultSegStrings)
//    try nv.checkValid()
//    catch {
//      case ex: Exception =>
//        ex.printStackTrace()
//    }
//  }

  private def snapRound(segStrings: util.Collection[SegmentString], li: LineIntersector): Unit = {
    val intersections = findInteriorIntersections(segStrings, li)
    computeIntersectionSnaps(intersections)
    computeVertexSnaps(segStrings)
  }

  /**
   * Computes all interior intersections in the collection of {link SegmentString}s, and returns
   * their {link Coordinate}s.
   *
   * Does NOT node the segStrings.
   *
   * return a list of Coordinates for the intersections
   */
  private def findInteriorIntersections(
    segStrings: util.Collection[SegmentString],
    li:         LineIntersector
  ): util.List[Coordinate] = {
    val intFinderAdder = new InteriorIntersectionFinderAdder(li)
    noder.setSegmentIntersector(intFinderAdder)
    noder.computeNodes(segStrings)
    intFinderAdder.getInteriorIntersections
  }

  /**
   * Snaps segments to nodes created by segment intersections.
   */
  private def computeIntersectionSnaps(snapPts: util.Collection[Coordinate]): Unit = {
    val it = snapPts.iterator
    while (it.hasNext) {
      val snapPt   = it.next
      val hotPixel = new HotPixel(snapPt, scaleFactor, li)
      pointSnapper.snap(hotPixel)
    }
  }

  /**
   * Snaps segments to all vertices.
   *
   * @param edges
   *   the list of segment strings to snap together
   */
  def computeVertexSnaps(edges: util.Collection[SegmentString]): Unit = {
    val i0 = edges.iterator
    while (i0.hasNext) {
      val edge0 = i0.next
      computeVertexSnaps(edge0)
    }
  }

  /**
   * Snaps segments to the vertices of a Segment String.
   */
  private def computeVertexSnaps(e: SegmentString): Unit = {
    val pts0 = e.getCoordinates
    var i    = 0
    while (i < pts0.length) {
      val hotPixel    = new HotPixel(pts0(i), scaleFactor, li)
      val isNodeAdded = pointSnapper.snap(hotPixel, e, i)
      // if a node is created for a vertex, that vertex must be noded too
      if (isNodeAdded) e.asInstanceOf[NodedSegmentString].addIntersection(pts0(i), i)
      i += 1
    }
  }
}
