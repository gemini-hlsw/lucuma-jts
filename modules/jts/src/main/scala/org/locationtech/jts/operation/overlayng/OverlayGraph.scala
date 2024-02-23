// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.operation.overlayng

import org.locationtech.jts.geom.Coordinate

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
 * A planar graph of edges, representing the topology resulting from an overlay operation. Each
 * source edge is represented by a pair of {@link OverlayEdge} s, with opposite (symmetric)
 * orientation. The pair of OverlayEdges share the edge coordinates and a single {@link
 * OverlayLabel}.
 *
 * @author
 *   Martin Davis
 */
class OverlayGraph() {

  /**
   * Creates an empty graph.
   */
  private val edges: util.List[OverlayEdge]              = new util.ArrayList[OverlayEdge]
  private val nodeMap: util.Map[Coordinate, OverlayEdge] = new util.HashMap[Coordinate, OverlayEdge]

  /**
   * Gets the set of edges in this graph. Only one of each symmetric pair of OverlayEdges is
   * included. The opposing edge can be found by using {@link OverlayEdge# sym ( )} .
   *
   * @return
   *   the collection of representative edges in this graph
   */
  def getEdges: util.Collection[OverlayEdge] =
    return edges

  /**
   * Gets the collection of edges representing the nodes in this graph. For each star of edges
   * originating at a node a single representative edge is included. The other edges around the node
   * can be found by following the next and prev links.
   *
   * @return
   *   the collection of representative node edges
   */
  def getNodeEdges: util.Collection[OverlayEdge] =
    return nodeMap.values

  /**
   * Gets an edge originating at the given node point.
   *
   * @param nodePt
   *   the node coordinate to query
   * @return
   *   an edge originating at the point, or null if none exists
   */
  def getNodeEdge(nodePt: Coordinate): OverlayEdge =
    return nodeMap.get(nodePt)

  /**
   * Gets the representative edges marked as being in the result area.
   *
   * @return
   *   the result area edges
   */
  def getResultAreaEdges: util.List[OverlayEdge] = {
    val resultEdges: util.List[OverlayEdge] = new util.ArrayList[OverlayEdge]
    for (edge <- getEdges.asScala)
      if (edge.isInResultArea) {
        resultEdges.add(edge)
      }
    return resultEdges
  }

  /**
   * Adds a new edge to this graph, for the given linework and topology information. A pair of
   * {@link OverlayEdge} s with opposite (symmetric) orientation is added, sharing the same {@link
   * OverlayLabel}.
   *
   * @param pts
   *   the edge vertices
   * @param label
   *   the edge topology information
   * @return
   *   the created graph edge with same orientation as the linework
   */
  def addEdge(pts: Array[Coordinate], label: OverlayLabel): OverlayEdge = { // if (! isValidEdge(orig, dest)) return null;
    val e: OverlayEdge = OverlayEdge.createEdgePair(pts, label)
    // Debug.println("added edge: " + e);
    insert(e)
    insert(e.symOE)
    return e
  }

  /**
   * Inserts a single half-edge into the graph. The sym edge must also be inserted.
   *
   * @param e
   *   the half-edge to insert
   */
  private def insert(e: OverlayEdge): Unit = {
    edges.add(e)

    /**
     * If the edge origin node is already in the graph, insert the edge into the star of edges
     * around the node. Otherwise, add a new node for the origin.
     */
    val nodeEdge: OverlayEdge = nodeMap.get(e.orig).asInstanceOf[OverlayEdge]
    if (nodeEdge != null) {
      nodeEdge.insert(e)
    } else {
      nodeMap.put(e.orig, e)
    }
    ()
  }
}
