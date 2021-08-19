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
package org.locationtech.jts.operation.relate

/**
 * @version 1.7
 */

import java.util
import org.locationtech.jts.geom.Location
import org.locationtech.jts.geomgraph.Edge
import org.locationtech.jts.geomgraph.EdgeEnd
import org.locationtech.jts.geomgraph.EdgeIntersection
import org.locationtech.jts.geomgraph.GeometryGraph
import org.locationtech.jts.geomgraph.Node
import org.locationtech.jts.geomgraph.NodeMap

/**
 * Implements the simple graph of Nodes and EdgeEnd which is all that is required to determine
 * topological relationships between Geometries. Also supports building a topological graph of a
 * single Geometry, to allow verification of valid topology. <p> It is <b>not</b> necessary to
 * create a fully linked PlanarGraph to determine relationships, since it is sufficient to know how
 * the Geometries interact locally around the nodes. In fact, this is not even feasible, since it is
 * not possible to compute exact intersection points, and hence the topology around those nodes
 * cannot be computed robustly. The only Nodes that are created are for improper intersections; that
 * is, nodes which occur at existing vertices of the Geometries. Proper intersections (e.g. ones
 * which occur between the interior of line segments) have their topology determined implicitly,
 * without creating a Node object to represent them.
 *
 * @version 1.7
 */
class RelateNodeGraph() {
  private val nodes = new NodeMap(new RelateNodeFactory)

  def getNodeIterator: util.Iterator[Node] = nodes.iterator

  def build(geomGraph: GeometryGraph): Unit = { // compute nodes for intersections between previously noded edges
    computeIntersectionNodes(geomGraph, 0)

    /**
     * Copy the labelling for the nodes in the parent Geometry. These override any labels determined
     * by intersections.
     */
    copyNodesAndLabels(geomGraph, 0)

    /**
     * Build EdgeEnds for all intersections.
     */
    val eeBuilder = new EdgeEndBuilder
    val eeList    = eeBuilder.computeEdgeEnds(geomGraph.getEdgeIterator)
    insertEdgeEnds(eeList)
    //Debug.println("==== NodeList ===");
    //Debug.print(nodes);
  }

  /**
   * Insert nodes for all intersections on the edges of a Geometry. Label the created nodes the same
   * as the edge label if they do not already have a label. This allows nodes created by either
   * self-intersections or mutual intersections to be labelled. Endpoint nodes will already be
   * labelled from when they were inserted. <p> Precondition: edge intersections have been computed.
   */
  def computeIntersectionNodes(geomGraph: GeometryGraph, argIndex: Int): Unit = {
    val edgeIt = geomGraph.getEdgeIterator
    while (edgeIt.hasNext) {
      val e    = edgeIt.next.asInstanceOf[Edge]
      val eLoc = e.getLabel.getLocation(argIndex)
      val eiIt = e.getEdgeIntersectionList.iterator
      while (eiIt.hasNext) {
        val ei = eiIt.next.asInstanceOf[EdgeIntersection]
        val n  = nodes.addNode(ei.coord).asInstanceOf[RelateNode]
        if (eLoc == Location.BOUNDARY) n.setLabelBoundary(argIndex)
        else if (n.getLabel.isNull(argIndex)) n.setLabel(argIndex, Location.INTERIOR)
        //Debug.println(n);
      }
    }
  }

  /**
   * Copy all nodes from an arg geometry into this graph. The node label in the arg geometry
   * overrides any previously computed label for that argIndex. (E.g. a node may be an intersection
   * node with a computed label of BOUNDARY, but in the original arg Geometry it is actually in the
   * interior due to the Boundary Determination Rule)
   */
  def copyNodesAndLabels(geomGraph: GeometryGraph, argIndex: Int): Unit = {
    val nodeIt = geomGraph.getNodeIterator
    while (nodeIt.hasNext) {
      val graphNode = nodeIt.next.asInstanceOf[Node]
      val newNode   = nodes.addNode(graphNode.getCoordinate)
      newNode.setLabel(argIndex, graphNode.getLabel.getLocation(argIndex))
      //node.print(System.out);
    }
  }

  def insertEdgeEnds(ee: util.List[_]): Unit = {
    val i = ee.iterator
    while (i.hasNext) {
      val e = i.next.asInstanceOf[EdgeEnd]
      nodes.add(e)
    }
  }
}
