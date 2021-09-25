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

import java.io.PrintStream
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.IntersectionMatrix
import org.locationtech.jts.geom.Location

/**
 * @version 1.7
 */
class Node(
  var coord: Coordinate // only non-null if this node is precise
  ,
  var edges: EdgeEndStar
) extends GraphComponent(new Label(0, Location.NONE)) {

  override def getCoordinate: Coordinate = coord

  def getEdges: EdgeEndStar = edges

  /**
   * Tests whether any incident edge is flagged as being in the result. This test can be used to
   * determine if the node is in the result, since if any incident edge is in the result, the node
   * must be in the result as well.
   *
   * return <code>true</code> if any incident edge in the in the result
   */
  def isIncidentEdgeInResult: Boolean = {
    val it = getEdges.getEdges.iterator
    while (it.hasNext) {
      val de = it.next.asInstanceOf[DirectedEdge]
      if (de.getEdge.isInResult) return true
    }
    false
  }

  override def isIsolated: Boolean    = label.getGeometryCount == 1

  /**
   * Basic nodes do not compute IMs
   */
  override protected def computeIM(im: IntersectionMatrix): Unit = {}

  /**
   * Add the edge to the list of edges at this node
   */
  def add(e: EdgeEnd): Unit = { // Assert: start pt of e is equal to node point
    edges.insert(e)
    e.setNode(this)
  }

  def mergeLabel(n: Node): Unit = mergeLabel(n.label)

  /**
   * To merge labels for two nodes, the merged location for each LabelElement is computed. The
   * location for the corresponding node LabelElement is set to the result, as long as the location
   * is non-null.
   */
  def mergeLabel(label2: Label): Unit = {
    var i = 0
    while (i < 2) {
      val loc     = computeMergedLocation(label2, i)
      val thisLoc = label.getLocation(i)
      if (thisLoc == Location.NONE) label.setLocation(i, loc)
      i += 1
    }
  }

  def setLabel(argIndex: Int, onLocation: Int): Unit = if (label == null)
    label = new Label(argIndex, onLocation)
  else label.setLocation(argIndex, onLocation)

  /**
   * Updates the label of a node to BOUNDARY, obeying the mod-2 boundaryDetermination rule.
   */
  def setLabelBoundary(argIndex: Int): Unit = {
    if (label == null) return
    // determine the current location for the point (if any)
    var loc    = Location.NONE
    if (label != null) loc = label.getLocation(argIndex)
    // flip the loc
    var newLoc = 0
    loc match {
      case Location.BOUNDARY =>
        newLoc = Location.INTERIOR
      case Location.INTERIOR =>
        newLoc = Location.BOUNDARY
      case _                 =>
        newLoc = Location.BOUNDARY
    }
    label.setLocation(argIndex, newLoc)
  }

  /**
   * The location for a given eltIndex for a node will be one of { null, INTERIOR, BOUNDARY }. A
   * node may be on both the boundary and the interior of a geometry; in this case, the rule is that
   * the node is considered to be in the boundary. The merged location is the maximum of the two
   * input values.
   */
  private[geomgraph] def computeMergedLocation(label2: Label, eltIndex: Int): Int = {
    var loc = Location.NONE
    loc = label.getLocation(eltIndex)
    if (!label2.isNull(eltIndex)) {
      val nLoc = label2.getLocation(eltIndex)
      if (loc != Location.BOUNDARY) loc = nLoc
    }
    loc
  }

  def print(out: PrintStream): Unit = out.println("node " + coord + " lbl: " + label)
}
