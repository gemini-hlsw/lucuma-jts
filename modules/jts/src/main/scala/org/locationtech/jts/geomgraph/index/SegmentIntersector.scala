// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
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
package org.locationtech.jts.geomgraph.index

import java.util
import org.locationtech.jts.algorithm.LineIntersector
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geomgraph.Edge
import org.locationtech.jts.geomgraph.Node

/**
 * Computes the intersection of line segments, and adds the intersection to the edges containing the
 * segments.
 *
 * @version 1.7
 */
object SegmentIntersector {
  def isAdjacentSegments(i1: Int, i2: Int): Boolean = Math.abs(i1 - i2) == 1
}

class SegmentIntersector(
  var li:             LineIntersector,
  var includeProper:  Boolean,
  var recordIsolated: Boolean
) {

  /**
   * These variables keep track of what types of intersections were found during ALL edges that have
   * been intersected.
   */
  private var vhasIntersection                       = false
  private var hasProper                              = false
  private var hasProperInterior                      = false
  // the proper intersection point found
  private var properIntersectionPoint: Coordinate    = null
//  private val isSelfIntersection = false
  // private boolean intersectionFound;
  private var numIntersections                       = 0
  // testing only
  var numTests                                       = 0
  private var bdyNodes: Array[util.Collection[Node]] = null
  var isDone                                         = false
  var isDoneWhenProperInt                            = false

  def setBoundaryNodes(bdyNodes0: util.Collection[Node], bdyNodes1: util.Collection[Node]): Unit = {
    bdyNodes = new Array[util.Collection[Node]](2)
    bdyNodes(0) = bdyNodes0
    bdyNodes(1) = bdyNodes1
  }

  def setIsDoneIfProperInt(isDoneWhenProperInt: Boolean): Unit = this.isDoneWhenProperInt =
    isDoneWhenProperInt

//  def isDone = isDone

  /**
   * return the proper intersection point, or <code>null</code> if none was found
   */
  def getProperIntersectionPoint: Coordinate = properIntersectionPoint

  def hasIntersection: Boolean = vhasIntersection

  /**
   * A proper intersection is an intersection which is interior to at least two line segments. Note
   * that a proper intersection is not necessarily in the interior of the entire Geometry, since
   * another edge may have an endpoint equal to the intersection, which according to SFS semantics
   * can result in the point being on the Boundary of the Geometry.
   */
  def hasProperIntersection: Boolean = hasProper

  /**
   * A proper interior intersection is a proper intersection which is <b>not</b> contained in the
   * set of boundary nodes set for this SegmentIntersector.
   */
  def hasProperInteriorIntersection: Boolean = hasProperInterior

  /**
   * A trivial intersection is an apparent self-intersection which in fact is simply the point
   * shared by adjacent line segments. Note that closed edges require a special check for the point
   * shared by the beginning and end segments.
   */
  private def isTrivialIntersection(e0: Edge, segIndex0: Int, e1: Edge, segIndex1: Int): Boolean = {
    if (e0 eq e1) if (li.getIntersectionNum == 1) {
      if (SegmentIntersector.isAdjacentSegments(segIndex0, segIndex1)) return true
      if (e0.isClosed) {
        val maxSegIndex = e0.getNumPoints - 1
        if (
          (segIndex0 == 0 && segIndex1 == maxSegIndex) || (segIndex1 == 0 && segIndex0 == maxSegIndex)
        ) return true
      }
    }
    false
  }

  /**
   * This method is called by clients of the EdgeIntersector class to test for and add intersections
   * for two segments of the edges being intersected. Note that clients (such as MonotoneChainEdges)
   * may choose not to intersect certain pairs of segments for efficiency reasons.
   */
  def addIntersections(e0: Edge, segIndex0: Int, e1: Edge, segIndex1: Int): Unit = {
    if ((e0 eq e1) && segIndex0 == segIndex1) return
    numTests += 1
    val p00 = e0.getCoordinates(segIndex0)
    val p01 = e0.getCoordinates(segIndex0 + 1)
    val p10 = e1.getCoordinates(segIndex1)
    val p11 = e1.getCoordinates(segIndex1 + 1)
    li.computeIntersection(p00, p01, p10, p11)
    // if (li.hasIntersection() && li.isProper()) Debug.println(li);
    /**
     * Always record any non-proper intersections. If includeProper is true, record any proper
     * intersections as well.
     */
    if (li.hasIntersection) {
      if (recordIsolated) {
        e0.setIsolated(false)
        e1.setIsolated(false)
      }
      // intersectionFound = true;
      numIntersections += 1
      // if the segments are adjacent they have at least one trivial intersection,
      // the shared endpoint.  Don't bother adding it if it is the
      // only intersection.
      if (!isTrivialIntersection(e0, segIndex0, e1, segIndex1)) {
        vhasIntersection = true
        if (includeProper || !li.isProperF) { // Debug.println(li);
          e0.addIntersections(li, segIndex0, 0)
          e1.addIntersections(li, segIndex1, 1)
        }
        if (li.isProperF) {
          properIntersectionPoint = li.getIntersection(0).copy
          hasProper = true
          if (isDoneWhenProperInt) isDone = true
          if (!isBoundaryPoint(li, bdyNodes)) hasProperInterior = true
        }
        // if (li.isCollinear())
        // hasCollinear = true;
      }
    }
  }

  private def isBoundaryPoint(
    li:       LineIntersector,
    bdyNodes: Array[util.Collection[Node]]
  ): Boolean = {
    if (bdyNodes == null) return false
    if (isBoundaryPointInternal(li, bdyNodes(0))) return true
    if (isBoundaryPointInternal(li, bdyNodes(1))) return true
    false
  }

  private def isBoundaryPointInternal(
    li:       LineIntersector,
    bdyNodes: util.Collection[Node]
  ): Boolean = {
    val i = bdyNodes.iterator
    while (i.hasNext) {
      val node = i.next
      val pt   = node.getCoordinate
      if (li.isIntersection(pt)) return true
    }
    false
  }
}
