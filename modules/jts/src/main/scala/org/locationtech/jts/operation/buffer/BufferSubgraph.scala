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
package org.locationtech.jts.operation.buffer

/**
 * @version 1.7
 */

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.Position
import org.locationtech.jts.geom.TopologyException
import org.locationtech.jts.geomgraph.DirectedEdge
import org.locationtech.jts.geomgraph.DirectedEdgeStar
import org.locationtech.jts.geomgraph.Node

import java.util

/**
 * A connected subset of the graph of {link DirectedEdge}s and {link Node}s. Its edges will generate
 * either <ul> <li> a single polygon in the complete buffer, with zero or more holes, or <li> one or
 * more connected holes </ul>
 *
 * @version 1.7
 */
class BufferSubgraph() extends Comparable[BufferSubgraph] {
  private val finder                     = new RightmostEdgeFinder
  private val dirEdgeList                = new util.ArrayList[DirectedEdge]
  private val nodes                      = new util.ArrayList[Node]
  private var rightMostCoord: Coordinate = null
  private var env: Envelope              = null

  def getDirectedEdges: util.ArrayList[DirectedEdge] = dirEdgeList

  def getNodes: util.ArrayList[Node] = nodes

  /**
   * Computes the envelope of the edges in the subgraph. The envelope is cached after being
   * computed.
   *
   * return the envelope of the graph.
   */
  def getEnvelope: Envelope = {
    if (env == null) {
      val edgeEnv = new Envelope
      val it      = dirEdgeList.iterator
      while (it.hasNext) {
        val dirEdge = it.next
        val pts     = dirEdge.getEdge.getCoordinates
        var i       = 0
        while (i < pts.length - 1) {
          edgeEnv.expandToInclude(pts(i))
          i += 1
        }
      }
      env = edgeEnv
    }
    env
  }

  /**
   * Gets the rightmost coordinate in the edges of the subgraph
   */
  def getRightmostCoordinate: Coordinate = rightMostCoord

  /**
   * Creates the subgraph consisting of all edges reachable from this node. Finds the edges in the
   * graph and the rightmost coordinate.
   *
   * @param node
   *   a node to start the graph traversal from
   */
  def create(node: Node): Unit = {
    addReachable(node)
    finder.findEdge(dirEdgeList)
    rightMostCoord = finder.getCoordinate
  }

  /**
   * Adds all nodes and edges reachable from this node to the subgraph. Uses an explicit stack to
   * avoid a large depth of recursion.
   *
   * @param node
   *   a node known to be in the subgraph
   */
  private def addReachable(startNode: Node): Unit = {
    val nodeStack = new util.Stack[Node]
    nodeStack.add(startNode)
    while (!nodeStack.empty) {
      val node = nodeStack.pop
      add(node, nodeStack)
    }
  }

  /**
   * Adds the argument node and all its out edges to the subgraph
   *
   * @param node
   *   the node to add
   * @param nodeStack
   *   the current set of nodes being traversed
   */
  private def add(node: Node, nodeStack: util.Stack[Node]): Unit = {
    node.setVisited(true)
    nodes.add(node)
    val i = node.getEdges.asInstanceOf[DirectedEdgeStar].iterator
    while (i.hasNext) {
      val de      = i.next.asInstanceOf[DirectedEdge]
      dirEdgeList.add(de)
      val sym     = de.getSym
      val symNode = sym.getNode

      /**
       * NOTE: this is a depth-first traversal of the graph. This will cause a large depth of
       * recursion. It might be better to do a breadth-first traversal.
       */
      if (!symNode.isVisited) nodeStack.push(symNode)
    }
  }

  private def clearVisitedEdges(): Unit = {
    val it = dirEdgeList.iterator
    while (it.hasNext) {
      val de = it.next
      de.setVisited(false)
    }
  }

  def computeDepth(outsideDepth: Int): Unit = {
    clearVisitedEdges()
    // find an outside edge to assign depth to
    val de = finder.getEdge
//    val n = de.getNode
//    val label = de.getLabel
    // right side of line returned by finder is on the outside
    de.setEdgeDepths(Position.RIGHT, outsideDepth)
    copySymDepths(de)
    // computeNodeDepth(n, de);
    computeDepths(de)
  }

  /**
   * Compute depths for all dirEdges via breadth-first traversal of nodes in graph
   *
   * @param startEdge
   *   edge to start processing with
   */
  // <FIX> MD - use iteration & queue rather than recursion, for speed and robustness
  private def computeDepths(startEdge: DirectedEdge): Unit = {
    val nodesVisited = new util.HashSet[Node]
    val nodeQueue    = new util.LinkedList[Node]
    val startNode    = startEdge.getNode
    nodeQueue.addLast(startNode)
    nodesVisited.add(startNode)
    startEdge.setVisited(true)
    while (!nodeQueue.isEmpty) { // System.out.println(nodes.size() + " queue: " + nodeQueue.size());
      val n = nodeQueue.removeFirst
      nodesVisited.add(n)
      // compute depths around node, starting at this edge since it has depths assigned
      computeNodeDepth(n)
      // add all adjacent nodes to process queue,
      // unless the node has been visited already
      val i = n.getEdges.asInstanceOf[DirectedEdgeStar].iterator
      while (i.hasNext) {
        val de  = i.next.asInstanceOf[DirectedEdge]
        val sym = de.getSym
        if (!sym.isVisited) {
          val adjNode = sym.getNode
          if (!nodesVisited.contains(adjNode)) {
            nodeQueue.addLast(adjNode)
            nodesVisited.add(adjNode)
          }
        }
      }
    }
  }

  private def computeNodeDepth(n: Node): Unit = { // find a visited dirEdge to start at
    var startEdge: DirectedEdge = null
    var i                       = n.getEdges.asInstanceOf[DirectedEdgeStar].iterator
    var loopBreak               = false
    while (i.hasNext && !loopBreak) {
      val de = i.next.asInstanceOf[DirectedEdge]
      if (de.isVisited || de.getSym.isVisited) {
        startEdge = de
        loopBreak = true
      }
    }
    // MD - testing  Result: breaks algorithm
    // if (startEdge == null) return;
    // only compute string append if assertion would fail
    if (startEdge == null)
      throw new TopologyException("unable to find edge to compute depths at " + n.getCoordinate)
    n.getEdges.asInstanceOf[DirectedEdgeStar].computeDepths(startEdge)
    // copy depths to sym edges
    i = n.getEdges.asInstanceOf[DirectedEdgeStar].iterator
    while (i.hasNext) {
      val de = i.next.asInstanceOf[DirectedEdge]
      de.setVisited(true)
      copySymDepths(de)
    }
  }

  private def copySymDepths(de: DirectedEdge): Unit = {
    val sym = de.getSym
    sym.setDepth(Position.LEFT, de.getDepth(Position.RIGHT))
    sym.setDepth(Position.RIGHT, de.getDepth(Position.LEFT))
  }

  /**
   * Find all edges whose depths indicates that they are in the result area(s). Since we want
   * polygon shells to be oriented CW, choose dirEdges with the interior of the result on the RHS.
   * Mark them as being in the result. Interior Area edges are the result of dimensional collapses.
   * They do not form part of the result area boundary.
   */
  def findResultEdges(): Unit = {
    val it = dirEdgeList.iterator
    while (it.hasNext) {
      val de = it.next

      /**
       * Select edges which have an interior depth on the RHS and an exterior depth on the LHS. Note
       * that because of weird rounding effects there may be edges which have negative depths!
       * Negative depths count as "outside".
       */
      // <FIX> - handle negative depths
      if (
        de.getDepth(Position.RIGHT) >= 1 && de.getDepth(
          Position.LEFT
        ) <= 0 && !de.isInteriorAreaEdge
      ) {
        de.setInResult(true)
        // Debug.print("in result "); Debug.println(de);
      }
    }
  }

  /**
   * BufferSubgraphs are compared on the x-value of their rightmost Coordinate. This defines a
   * partial ordering on the graphs such that: <p> g1 >= g2 <==> Ring(g2) does not contain Ring(g1)
   * <p> where Polygon(g) is the buffer polygon that is built from g. <p> This relationship is used
   * to sort the BufferSubgraphs so that shells are guaranteed to be built before holes.
   */
  override def compareTo(o: BufferSubgraph): Int = {
    val graph = o.asInstanceOf[BufferSubgraph]
    if (this.rightMostCoord.x < graph.rightMostCoord.x) return -1
    if (this.rightMostCoord.x > graph.rightMostCoord.x) return 1
    0
  }
  /*
    // DEBUGGING only - comment out
      private static final String SAVE_DIREDGES = "saveDirEdges";
      private static int saveCount = 0;
      public void saveDirEdges()
      {
        GeometryFactory fact = new GeometryFactory();
        for (Iterator it = dirEdgeList.iterator(); it.hasNext(); ) {
          DirectedEdge de = (DirectedEdge) it.next();
          double dx = de.getDx();
          double dy = de.getDy();
          Coordinate p0 = de.getCoordinate();
          double ang = Math.atan2(dy, dx);
          Coordinate p1 = new Coordinate(
              p0.x + .4 * Math.cos(ang),
              p0.y + .4 * Math.sin(ang));
    //      DebugFeature.add(SAVE_DIREDGES,
    //                       fact.createLineString(new Coordinate[] { p0, p1 } ),
    //                       de.getDepth(Position.LEFT) + "/" + de.getDepth(Position.RIGHT)
    //                       );
        }
      String filepath = "x:\\jts\\testBuffer\\dirEdges" + saveCount++ + ".jml";
        DebugFeature.saveFeatures(SAVE_DIREDGES, filepath);
      }
   */
}
