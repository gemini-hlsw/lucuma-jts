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
 */ /*
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

/**
 * @version 1.7
 */

import java.io.PrintStream
import java.util
import org.locationtech.jts.algorithm.Orientation
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Location

/**
 * The computation of the <code>IntersectionMatrix</code> relies on the use of a structure called a
 * "topology graph". The topology graph contains nodes and edges corresponding to the nodes and line
 * segments of a <code>Geometry</code>. Each node and edge in the graph is labeled with its
 * topological location relative to the source geometry. <P> Note that there is no requirement that
 * points of self-intersection be a vertex. Thus to obtain a correct topology graph,
 * <code>Geometry</code>s must be self-noded before constructing their graphs. <P> Two fundamental
 * operations are supported by topology graphs: <UL> <LI>Computing the intersections between all the
 * edges and nodes of a single graph <LI>Computing the intersections between the edges and nodes of
 * two different graphs </UL>
 *
 * @version 1.7
 */
object PlanarGraph {

  /**
   * For nodes in the Collection, link the DirectedEdges at the node that are in the result. This
   * allows clients to link only a subset of nodes in the graph, for efficiency (because they know
   * that only a subset is of interest).
   */
  def linkResultDirectedEdges(nodes: util.Collection[Node]): Unit = {
    val nodeit = nodes.iterator
    while (nodeit.hasNext) {
      val node = nodeit.next
      node.getEdges.asInstanceOf[DirectedEdgeStar].linkResultDirectedEdges()
    }
  }
}

class PlanarGraph(nodeFact: NodeFactory) {
  protected var edges       = new util.ArrayList[Edge]
  protected var nodes       = new NodeMap(nodeFact)
  protected var edgeEndList = new util.ArrayList[EdgeEnd]

  def this() = {
    this(new NodeFactory())
  }

  def getEdgeIterator: util.Iterator[Edge] = edges.iterator

  def getEdgeEnds: util.ArrayList[EdgeEnd] = edgeEndList

  def isBoundaryNode(geomIndex: Int, coord: Coordinate): Boolean = {
    val node  = nodes.find(coord)
    if (node == null) return false
    val label = node.getLabel
    if (label != null && label.getLocation(geomIndex) == Location.BOUNDARY) return true
    false
  }

  protected def insertEdge(e: Edge): Boolean                     = edges.add(e)

  def add(e: EdgeEnd): Boolean = {
    nodes.add(e)
    edgeEndList.add(e)
  }

  def getNodeIterator: util.Iterator[Node] = nodes.iterator

  def getNodes: util.Collection[Node] = nodes.values

  def addNode(node: Node): Node = nodes.addNode(node)

  def addNode(coord: Coordinate): Node = nodes.addNode(coord)

  /**
   * return the node if found; null otherwise
   */
  def find(coord: Coordinate): Node = nodes.find(coord)

  /**
   * Add a set of edges to the graph. For each edge two DirectedEdges will be created. DirectedEdges
   * are NOT linked by this method.
   */
  def addEdges(edgesToAdd: util.List[Edge]): Unit = { // create all the nodes for the edges
    val it = edgesToAdd.iterator
    while (it.hasNext) {
      val e   = it.next
      edges.add(e)
      val de1 = new DirectedEdge(e, true)
      val de2 = new DirectedEdge(e, false)
      de1.setSym(de2)
      de2.setSym(de1)
      add(de1)
      add(de2)
    }
  }

  /**
   * Link the DirectedEdges at the nodes of the graph. This allows clients to link only a subset of
   * nodes in the graph, for efficiency (because they know that only a subset is of interest).
   */
  def linkResultDirectedEdges(): Unit = {
    val nodeit = nodes.iterator
    while (nodeit.hasNext) {
      val node = nodeit.next
      node.getEdges.asInstanceOf[DirectedEdgeStar].linkResultDirectedEdges()
    }
  }

  def linkAllDirectedEdges(): Unit = {
    val nodeit = nodes.iterator
    while (nodeit.hasNext) {
      val node = nodeit.next
      node.getEdges.asInstanceOf[DirectedEdgeStar].linkAllDirectedEdges()
    }
  }

  /**
   * Returns the EdgeEnd which has edge e as its base edge (MD 18 Feb 2002 - this should return a
   * pair of edges)
   *
   * return the edge, if found <code>null</code> if the edge was not found
   */
  def findEdgeEnd(e: Edge): EdgeEnd = {
    val i = getEdgeEnds.iterator
    while (i.hasNext) {
      val ee = i.next
      if (ee.getEdge eq e) return ee
    }
    null
  }

  /**
   * Returns the edge whose first two coordinates are p0 and p1
   *
   * return the edge, if found <code>null</code> if the edge was not found
   */
  def findEdge(p0: Coordinate, p1: Coordinate): Edge = {
    var i = 0
    while (i < edges.size) {
      val e      = edges.get(i)
      val eCoord = e.getCoordinates
      if (p0 == eCoord(0) && p1 == eCoord(1)) return e
      i += 1
    }
    null
  }

  /**
   * Returns the edge which starts at p0 and whose first segment is parallel to p1
   *
   * return the edge, if found <code>null</code> if the edge was not found
   */
  def findEdgeInSameDirection(p0: Coordinate, p1: Coordinate): Edge = {
    var i = 0
    while (i < edges.size) {
      val e      = edges.get(i)
      val eCoord = e.getCoordinates
      if (matchInSameDirection(p0, p1, eCoord(0), eCoord(1))) return e
      if (matchInSameDirection(p0, p1, eCoord(eCoord.length - 1), eCoord(eCoord.length - 2)))
        return e
      i += 1
    }
    null
  }

  /**
   * The coordinate pairs match if they define line segments lying in the same direction. E.g. the
   * segments are parallel and in the same quadrant (as opposed to parallel and opposite!).
   */
  private def matchInSameDirection(
    p0:  Coordinate,
    p1:  Coordinate,
    ep0: Coordinate,
    ep1: Coordinate
  ): Boolean = {
    if (!(p0 == ep0)) return false
    if (
      Orientation.index(p0, p1, ep1) == Orientation.COLLINEAR && Quadrant.quadrant(p0,
                                                                                   p1
      ) == Quadrant.quadrant(ep0, ep1)
    ) return true
    false
  }

  def printEdges(out: PrintStream): Unit          = {
    out.println("Edges:")
    var i = 0
    while (i < edges.size) {
      out.println("edge " + i + ":")
      val e = edges.get(i)
      e.print(out)
      e.eiList.print(out)
      i += 1
    }
  }

  private[geomgraph] def debugPrint(o: Any): Unit = System.out.print(o)

  private[geomgraph] def debugPrintln(o: Any): Unit = System.out.println(o)
}
