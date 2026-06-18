// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2023 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v20.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.operation.relateng

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Dimension
import org.locationtech.jts.geom.Location
import org.locationtech.jts.geom.Position
import org.locationtech.jts.io.WKTWriter

import java.util

class RelateNode(private val nodePt: Coordinate) {

  /**
   * A list of the edges around the node in CCW order, ordered by their CCW angle with the positive
   * X-axis.
   */
  private val edges: util.ArrayList[RelateEdge] = new util.ArrayList[RelateEdge]()

  def getCoordinate: Coordinate = nodePt

  def getEdges: util.List[RelateEdge] = edges

  def addEdges(nss: util.List[NodeSection]): Unit = {
    val it = nss.iterator
    while (it.hasNext) {
      val ns = it.next()
      addEdges(ns)
    }
  }

  def addEdges(ns: NodeSection): Unit =
    // Debug.println("Adding NS: " + ns);
    ns.dimension match {
      case Dimension.L =>
        addLineEdge(ns.isA, ns.getVertex(0))
        addLineEdge(ns.isA, ns.getVertex(1))
      case Dimension.A =>
        // -- assumes node edges have CW orientation (as per JTS norm)
        // -- entering edge - interior on L
        val e0 = addAreaEdge(ns.isA, ns.getVertex(0), false)
        // -- exiting edge - interior on R
        val e1 = addAreaEdge(ns.isA, ns.getVertex(1), true)

        val index0 = edges.indexOf(e0)
        val index1 = edges.indexOf(e1)
        updateEdgesInArea(ns.isA, index0, index1)
        updateIfAreaPrev(ns.isA, index0)
        updateIfAreaNext(ns.isA, index1)
      case _           =>
    }

  private def updateEdgesInArea(isA: Boolean, indexFrom: Int, indexTo: Int): Unit = {
    var index = RelateNode.nextIndex(edges, indexFrom)
    while (index != indexTo) {
      val edge = edges.get(index)
      edge.setAreaInterior(isA)
      index = RelateNode.nextIndex(edges, index)
    }
  }

  private def updateIfAreaPrev(isA: Boolean, index: Int): Unit = {
    val indexPrev = RelateNode.prevIndex(edges, index)
    val edgePrev  = edges.get(indexPrev)
    if (edgePrev.isInterior(isA, Position.LEFT)) {
      val edge = edges.get(index)
      edge.setAreaInterior(isA)
    }
  }

  private def updateIfAreaNext(isA: Boolean, index: Int): Unit = {
    val indexNext = RelateNode.nextIndex(edges, index)
    val edgeNext  = edges.get(indexNext)
    if (edgeNext.isInterior(isA, Position.RIGHT)) {
      val edge = edges.get(index)
      edge.setAreaInterior(isA)
    }
  }

  private def addLineEdge(isA: Boolean, dirPt: Coordinate): RelateEdge =
    addEdge(isA, dirPt, Dimension.L, false)

  private def addAreaEdge(isA: Boolean, dirPt: Coordinate, isForward: Boolean): RelateEdge =
    addEdge(isA, dirPt, Dimension.A, isForward)

  /**
   * Adds or merges an edge to the node.
   *
   * @param isA
   * @param dirPt
   * @param dim
   *   dimension of the geometry element containing the edge
   * @param isForward
   *   the direction of the edge
   *
   * return the created or merged edge for this point
   */
  private def addEdge(isA: Boolean, dirPt: Coordinate, dim: Int, isForward: Boolean): RelateEdge = {
    // -- check for well-formed edge - skip null or zero-len input
    if (dirPt == null)
      return null
    if (nodePt.equals2D(dirPt))
      return null

    var insertIndex = -1
    var i           = 0
    while (i < edges.size && insertIndex < 0) {
      val e    = edges.get(i)
      val comp = e.compareToEdge(dirPt)
      if (comp == 0) {
        e.merge(isA, dirPt, dim, isForward)
        return e
      }
      if (comp == 1) {
        // -- found further edge, so insert a new edge at this position
        insertIndex = i
      }
      i += 1
    }
    // -- add a new edge
    val e           = RelateEdge.create(this, dirPt, isA, dim, isForward)
    if (insertIndex < 0) {
      // -- add edge at end of list
      edges.add(e)
    } else {
      // -- add edge before higher edge found
      edges.add(insertIndex, e)
    }
    e
  }

  /**
   * Computes the final topology for the edges around this node. Although nodes lie on the boundary
   * of areas or the interior of lines, in a mixed GC they may also lie in the interior of an area.
   * This changes the locations of the sides and line to Interior.
   *
   * @param isAreaInteriorA
   *   true if the node is in the interior of A
   * @param isAreaInteriorB
   *   true if the node is in the interior of B
   */
  def finish(isAreaInteriorA: Boolean, isAreaInteriorB: Boolean): Unit = {
    finishNode(RelateGeometry.GEOM_A, isAreaInteriorA)
    finishNode(RelateGeometry.GEOM_B, isAreaInteriorB)
  }

  private def finishNode(isA: Boolean, isAreaInterior: Boolean): Unit =
    if (isAreaInterior) {
      RelateEdge.setAreaInterior(edges, isA)
    } else {
      val startIndex = RelateEdge.findKnownEdgeIndex(edges, isA)
      // -- only interacting nodes are finished, so this should never happen
      // Assert.isTrue(startIndex >= 0l, "Node at "+ nodePt + "does not have AB interaction");
      propagateSideLocations(isA, startIndex)
    }

  private def propagateSideLocations(isA: Boolean, startIndex: Int): Unit = {
    var currLoc = edges.get(startIndex).location(isA, Position.LEFT)
    // -- edges are stored in CCW order
    var index   = RelateNode.nextIndex(edges, startIndex)
    while (index != startIndex) {
      val e = edges.get(index)
      e.setUnknownLocations(isA, currLoc)
      currLoc = e.location(isA, Position.LEFT)
      index = RelateNode.nextIndex(edges, index)
    }
  }

  override def toString: String = {
    val buf = new StringBuilder
    buf.append("Node[" + WKTWriter.toPoint(nodePt) + "]:")
    buf.append("\n")
    val it  = edges.iterator
    while (it.hasNext) {
      val e = it.next()
      buf.append(e.toString)
      buf.append("\n")
    }
    buf.toString
  }

  def hasExteriorEdge(isA: Boolean): Boolean = {
    val it = edges.iterator
    while (it.hasNext) {
      val e = it.next()
      if (
        Location.EXTERIOR == e.location(isA, Position.LEFT)
        || Location.EXTERIOR == e.location(isA, Position.RIGHT)
      ) {
        return true
      }
    }
    false
  }
}

object RelateNode {

  private def prevIndex(list: util.ArrayList[RelateEdge], index: Int): Int = {
    if (index > 0)
      return index - 1
    // -- index == 0
    list.size - 1
  }

  private def nextIndex(list: util.List[RelateEdge], i: Int): Int = {
    if (i >= list.size - 1) {
      return 0
    }
    i + 1
  }

}
