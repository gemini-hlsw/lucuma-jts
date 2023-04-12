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
package org.locationtech.jts.geomgraph

import org.locationtech.jts.geom.Location
import org.locationtech.jts.geom.Position
import org.locationtech.jts.geom.TopologyException

import java.io.PrintStream

/**
 * @version 1.7
 */
object DirectedEdge {

  /**
   * Computes the factor for the change in depth when moving from one location to another. E.g. if
   * crossing from the INTERIOR to the EXTERIOR the depth decreases, so the factor is -1
   */
  def depthFactor(currLocation: Int, nextLocation: Int): Int = {
    if (currLocation == Location.EXTERIOR && nextLocation == Location.INTERIOR) return 1
    else if (currLocation == Location.INTERIOR && nextLocation == Location.EXTERIOR) return -1
    0
  }
}

class DirectedEdge(val edg: Edge, var visForward: Boolean) extends EdgeEnd(edg) {
  if (visForward) init(edge.getCoordinate(0), edge.getCoordinate(1))
  else {
    val n = edge.getNumPoints - 1
    init(edge.getCoordinate(n), edge.getCoordinate(n - 1))
  }
  private var visInResult        = false
  private var visVisited         = false
  private var sym: DirectedEdge  = null // the symmetric edge
  private var next: DirectedEdge =
    null // the next edge in the edge ring for the polygon containing this edge
  private var nextMin: DirectedEdge =
    null // the next edge in the MinimalEdgeRing that contains this edge
  private var edgeRing: EdgeRing    = null // the EdgeRing that this edge is part of
  private var minEdgeRing: EdgeRing = null // the MinimalEdgeRing that this edge is part of
  /**
   * The depth of each side (position) of this edge. The 0 element of the array is never used.
   */
  private val depth                 = Array(0, -999, -999)
  computeDirectedLabel()

  override def getEdge: Edge = edge

  def setInResult(isInResult: Boolean): Unit = this.visInResult = isInResult

  def isInResult: Boolean = visInResult

  def isVisited: Boolean = visVisited

  def setVisited(isVisited: Boolean): Unit = this.visVisited = isVisited

  def setEdgeRing(edgeRing: EdgeRing): Unit = this.edgeRing = edgeRing

  def getEdgeRing: EdgeRing = edgeRing

  def setMinEdgeRing(minEdgeRing: EdgeRing): Unit = this.minEdgeRing = minEdgeRing

  def getMinEdgeRing: EdgeRing = minEdgeRing

  def getDepth(position: Int): Int = depth(position)

  def setDepth(position: Int, depthVal: Int): Unit = {
    if (depth(position) != -999) { //      if (depth[position] != depthVal) {
      //        Debug.print(this);
      //      }
      if (depth(position) != depthVal)
        throw new TopologyException("assigned depths do not match", getCoordinate)
      // Assert.isTrue(depth[position] == depthVal, "assigned depths do not match at " + getCoordinate());
    }
    depth(position) = depthVal
  }

  def getDepthDelta: Int = {
    var depthDelta = edge.getDepthDelta
    if (!visForward) depthDelta = -depthDelta
    depthDelta
  }

  /**
   * setVisitedEdge marks both DirectedEdges attached to a given Edge. This is used for edges
   * corresponding to lines, which will only appear oriented in a single direction in the result.
   */
  def setVisitedEdge(isVisited: Boolean): Unit = {
    setVisited(isVisited)
    sym.setVisited(isVisited)
  }

  /**
   * Each Edge gives rise to a pair of symmetric DirectedEdges, in opposite directions.
   *
   * return the DirectedEdge for the same Edge but in the opposite direction
   */
  def getSym: DirectedEdge = sym

  def isForward: Boolean = visForward

  def setSym(de: DirectedEdge): Unit = sym = de

  def getNext: DirectedEdge = next

  def setNext(next: DirectedEdge): Unit = this.next = next

  def getNextMin: DirectedEdge = nextMin

  def setNextMin(nextMin: DirectedEdge): Unit = this.nextMin = nextMin

  /**
   * This edge is a line edge if <ul> <li> at least one of the labels is a line label <li> any
   * labels which are not line labels have all Locations = EXTERIOR </ul>
   */
  def isLineEdge: Boolean = {
    val isLine            = label.isLine(0) || label.isLine(1)
    val isExteriorIfArea0 = !label.isArea(0) || label.allPositionsEqual(0, Location.EXTERIOR)
    val isExteriorIfArea1 = !label.isArea(1) || label.allPositionsEqual(1, Location.EXTERIOR)
    isLine && isExteriorIfArea0 && isExteriorIfArea1
  }

  /**
   * This is an interior Area edge if <ul> <li> its label is an Area label for both Geometries <li>
   * and for each Geometry both sides are in the interior. </ul>
   *
   * return true if this is an interior Area edge
   */
  def isInteriorAreaEdge: Boolean = {
    var isInteriorAreaEdge = true
    var i                  = 0
    while (i < 2) {
      if (
        !(label.isArea(i) && (label.getLocation(i, Position.LEFT) == Location.INTERIOR) && (label
          .getLocation(i, Position.RIGHT) == Location.INTERIOR))
      ) isInteriorAreaEdge = false
      i += 1
    }
    isInteriorAreaEdge
  }

  /**
   * Compute the label in the appropriate orientation for this DirEdge
   */
  private def computeDirectedLabel(): Unit = {
    label = new Label(edge.getLabel)
    if (!visForward) label.flip()
  }

  /**
   * Set both edge depths. One depth for a given side is provided. The other is computed depending
   * on the Location transition and the depthDelta of the edge.
   */
  def setEdgeDepths(position: Int, depth: Int): Unit = { // get the depth transition delta from R to L for this directed Edge
    var depthDelta      = getEdge.getDepthDelta
    if (!visForward) depthDelta = -depthDelta
    // if moving from L to R instead of R to L must change sign of delta
    var directionFactor = 1
    if (position == Position.LEFT) directionFactor = -1
    val oppositePos     = Position.opposite(position)
    val delta           = depthDelta * directionFactor
    // TESTINGint delta = depthDelta * DirectedEdge.depthFactor(loc, oppositeLoc);
    val oppositeDepth   = depth + delta
    setDepth(position, depth)
    setDepth(oppositePos, oppositeDepth)
  }

  override def print(out: PrintStream): Unit = {
    super.print(out)
    out.print(" " + depth(Position.LEFT) + "/" + depth(Position.RIGHT))
    out.print(" (" + getDepthDelta + ")")
    // out.print(" " + this.hashCode());
    // if (next != null) out.print(" next:" + next.hashCode());
    if (isInResult) out.print(" inResult")
  }

  def printEdge(out: PrintStream): Unit = {
    print(out)
    out.print(" ")
    if (visForward) edge.print(out)
    else edge.printReverse(out)
  }
}
