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
package org.locationtech.jts.operation.overlay

import java.util
import org.locationtech.jts.algorithm.PointLocator
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geomgraph.DirectedEdge
import org.locationtech.jts.geomgraph.DirectedEdgeStar
import org.locationtech.jts.geomgraph.Edge
import org.locationtech.jts.util.Assert

/**
 * Forms JTS LineStrings out of a the graph of {link DirectedEdge}s created by an {link OverlayOp}.
 *
 * @version 1.7
 */
class LineBuilder(
  var op:              OverlayOp,
  var geometryFactory: GeometryFactory,
  var ptLocator:       PointLocator
) {
  private val lineEdgesList  = new util.ArrayList[Edge]
  private val resultLineList = new util.ArrayList[LineString]

  /**
   * return a list of the LineStrings in the result of the specified overlay operation
   */
  def build(opCode: Int): util.ArrayList[LineString] = {
    findCoveredLineEdges()
    collectLines(opCode)
    // labelIsolatedLines(lineEdgesList);
    buildLines(opCode)
    resultLineList
  }

  /**
   * Find and mark L edges which are "covered" by the result area (if any). L edges at nodes which
   * also have A edges can be checked by checking their depth at that node. L edges at nodes which
   * do not have A edges can be checked by doing a point-in-polygon test with the previously
   * computed result areas.
   */
  private def findCoveredLineEdges()
    : Unit = { // first set covered for all L edges at nodes which have A edges too
    val nodeit = op.getGraph.getNodes.iterator
    while (nodeit.hasNext) {
      val node = nodeit.next
      // node.print(System.out);
      node.getEdges.asInstanceOf[DirectedEdgeStar].findCoveredLineEdges()
    }

    /**
     * For all L edges which weren't handled by the above, use a point-in-poly test to determine
     * whether they are covered
     */
    val it = op.getGraph.getEdgeEnds.iterator
    while (it.hasNext) {
      val de = it.next.asInstanceOf[DirectedEdge]
      val e  = de.getEdge
      if (de.isLineEdge && !e.isCoveredSet) {
        val isCovered = op.isCoveredByA(de.getCoordinate)
        e.setCovered(isCovered)
      }
    }
  }

  private def collectLines(opCode: Int): Unit = {
    val it = op.getGraph.getEdgeEnds.iterator
    while (it.hasNext) {
      val de = it.next.asInstanceOf[DirectedEdge]
      collectLineEdge(de, opCode, lineEdgesList)
      collectBoundaryTouchEdge(de, opCode, lineEdgesList)
    }
  }

  /**
   * Collect line edges which are in the result. Line edges are in the result if they are not part
   * of an area boundary, if they are in the result of the overlay operation, and if they are not
   * covered by a result area.
   *
   * @param de
   *   the directed edge to test
   * @param opCode
   *   the overlap operation
   * @param edges
   *   the list of included line edges
   */
  private def collectLineEdge(de: DirectedEdge, opCode: Int, edges: util.List[Edge]): Unit = {
    val label = de.getLabel
    val e     = de.getEdge
    // include L edges which are in the result
    if (de.isLineEdge)
      if (!de.isVisited && OverlayOp.isResultOfOp(label, opCode) && !e.isCovered) { // Debug.println("de: " + de.getLabel());
        // Debug.println("edge: " + e.getLabel());
        edges.add(e)
        de.setVisitedEdge(true)
      }
  }

  /**
   * Collect edges from Area inputs which should be in the result but which have not been included
   * in a result area. This happens ONLY: <ul> <li>during an intersection when the boundaries of two
   * areas touch in a line segment <li> OR as a result of a dimensional collapse. </ul>
   */
  private def collectBoundaryTouchEdge(
    de:     DirectedEdge,
    opCode: Int,
    edges:  util.List[Edge]
  ): Unit = {
    val label = de.getLabel
    if (de.isLineEdge) return         // only interested in area edges
    if (de.isVisited) return          // already processed
    if (de.isInteriorAreaEdge) return // added to handle dimensional collapses
    if (de.getEdge.isInResult)
      return                          // if the edge linework is already included, don't include it again
    // sanity check for labelling of result edgerings
    Assert.isTrue(!(de.isInResult || de.getSym.isInResult) || !de.getEdge.isInResult)
    // include the linework if it's in the result of the operation
    if (OverlayOp.isResultOfOp(label, opCode) && opCode == OverlayOp.INTERSECTION) {
      edges.add(de.getEdge)
      de.setVisitedEdge(true)
    }
  }

  private def buildLines(opCode: Int): Unit = {
    val it = lineEdgesList.iterator
    while (it.hasNext) {
      val e    = it.next.asInstanceOf[Edge]
      // Label label = e.getLabel();
      val line = geometryFactory.createLineString(e.getCoordinates)
      resultLineList.add(line)
      e.setInResult(true)
    }
  }

//  private def labelIsolatedLines(edgesList: util.List[Edge]): Unit = {
//    val it = edgesList.iterator
//    while ( {
//      it.hasNext
//    }) {
//      val e = it.next
//      val label = e.getLabel
//      //n.print(System.out);
//      if (e.isIsolated) if (label.isNull(0)) labelIsolatedLine(e, 0)
//      else labelIsolatedLine(e, 1)
//    }
//  }

  /**
   * Label an isolated node with its relationship to the target geometry.
   */
//  private def labelIsolatedLine(e: Edge, targetIndex: Int): Unit = {
//    val loc = ptLocator.locate(e.getCoordinate, op.getArgGeometry(targetIndex))
//    e.getLabel.setLocation(targetIndex, loc)
//  }
}
