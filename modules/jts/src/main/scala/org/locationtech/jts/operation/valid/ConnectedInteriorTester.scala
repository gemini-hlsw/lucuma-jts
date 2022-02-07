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
package org.locationtech.jts.operation.valid

import java.util

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.Location
import org.locationtech.jts.geom.MultiPolygon
import org.locationtech.jts.geom.Polygon
import org.locationtech.jts.geomgraph.{
  DirectedEdge,
  Edge,
  EdgeEnd,
  GeometryGraph,
  PlanarGraph,
  Position
}
import org.locationtech.jts.operation.overlay.{
  MaximalEdgeRing,
  MinimalEdgeRing,
  OverlayNodeFactory
}
import org.locationtech.jts.util.Assert

/**
 * This class tests that the interior of an area {link Geometry} ( {link Polygon} or {link
 * MultiPolygon} ) is connected. This can happen if: <ul> <li>a shell self-intersects <li>one or
 * more holes form a connected chain touching a shell at two different points <li>one or more holes
 * form a ring around a subset of the interior </ul> If a disconnected situation is found the
 * location of the problem is recorded.
 *
 * @version 1.7
 */
object ConnectedInteriorTester {
  def findDifferentPoint(coord: Array[Coordinate], pt: Coordinate): Coordinate = {
    var i = 0
    while (i < coord.length) {
      if (!(coord(i) == pt)) return coord(i)
      i += 1
    }
    null
  }
}

class ConnectedInteriorTester(var geomGraph: GeometryGraph) {
  private val geometryFactory                   = new GeometryFactory
  // save a coordinate for any disconnected interior found
  // the coordinate will be somewhere on the ring surrounding the disconnected interior
  private var disconnectedRingcoord: Coordinate = null

  def getCoordinate: Coordinate = disconnectedRingcoord

  def isInteriorsConnected: Boolean = { // node the edges, in case holes touch the shell
    val splitEdges = new util.ArrayList[Edge]
    geomGraph.computeSplitEdges(splitEdges)
    // form the edges into rings
    val graph      = new PlanarGraph(new OverlayNodeFactory)
    graph.addEdges(splitEdges)
    setInteriorEdgesInResult(graph)
    graph.linkResultDirectedEdges()
    val edgeRings  = buildEdgeRings(graph.getEdgeEnds)

    /**
     * Mark all the edges for the edgeRings corresponding to the shells of the input polygons. Note
     * only ONE ring gets marked for each shell.
     */
    visitShellInteriors(geomGraph.getGeometry, graph)

    /**
     * If there are any unvisited shell edges (i.e. a ring which is not a hole and which has the
     * interior of the parent area on the RHS) this means that one or more holes must have split the
     * interior of the polygon into at least two pieces. The polygon is thus invalid.
     */
    !hasUnvisitedShellEdge(edgeRings)
  }

  private def setInteriorEdgesInResult(graph: PlanarGraph): Unit = {
    val it = graph.getEdgeEnds.iterator
    while (it.hasNext) {
      val de = it.next.asInstanceOf[DirectedEdge]
      if (de.getLabel.getLocation(0, Position.RIGHT) == Location.INTERIOR) de.setInResult(true)
    }
  }

  /**
   * Form DirectedEdges in graph into Minimal EdgeRings. (Minimal Edgerings must be used, because
   * only they are guaranteed to provide a correct isHole computation)
   */
  private def buildEdgeRings(
    dirEdges: util.Collection[EdgeEnd]
  ): util.ArrayList[MinimalEdgeRing] = {
    val edgeRings = new util.ArrayList[MinimalEdgeRing]
    val it        = dirEdges.iterator
    while (it.hasNext) {
      val de = it.next.asInstanceOf[DirectedEdge]
      // if this edge has not yet been processed
      if (de.isInResult && de.getEdgeRing == null) {
        val er           = new MaximalEdgeRing(de, geometryFactory)
        er.linkDirectedEdgesForMinimalEdgeRings()
        val minEdgeRings = er.buildMinimalRings
        edgeRings.addAll(minEdgeRings)
      }
    }
    edgeRings
  }

  /**
   * Mark all the edges for the edgeRings corresponding to the shells of the input polygons. Only
   * ONE ring gets marked for each shell - if there are others which remain unmarked this indicates
   * a disconnected interior.
   */
  private def visitShellInteriors(g: Geometry, graph: PlanarGraph): Unit = {
    if (g.isInstanceOf[Polygon]) {
      val p = g.asInstanceOf[Polygon]
      visitInteriorRing(p.getExteriorRing, graph)
    }
    if (g.isInstanceOf[MultiPolygon]) {
      val mp = g.asInstanceOf[MultiPolygon]
      var i  = 0
      while (i < mp.getNumGeometries) {
        val p = mp.getGeometryN(i).asInstanceOf[Polygon]
        visitInteriorRing(p.getExteriorRing, graph)
        i += 1
      }
    }
  }

  private def visitInteriorRing(ring: LineString, graph: PlanarGraph): Unit = {
    if (ring.isEmpty) return
    val pts = ring.getCoordinates
    val pt0 = pts(0)

    /**
     * Find first point in coord list different to initial point. Need special check since the first
     * point may be repeated.
     */
    val pt1                 = ConnectedInteriorTester.findDifferentPoint(pts, pt0)
    val e                   = graph.findEdgeInSameDirection(pt0, pt1)
    val de                  = graph.findEdgeEnd(e).asInstanceOf[DirectedEdge]
    var intDe: DirectedEdge = null
    if (de.getLabel.getLocation(0, Position.RIGHT) == Location.INTERIOR) intDe = de
    else if (de.getSym.getLabel.getLocation(0, Position.RIGHT) == Location.INTERIOR)
      intDe = de.getSym
    Assert.isTrue(intDe != null, "unable to find dirEdge with Interior on RHS")
    visitLinkedDirectedEdges(intDe)
  }

  protected def visitLinkedDirectedEdges(start: DirectedEdge) = {
    val startDe = start
    var de      = start
    while ({
      {
        Assert.isTrue(de != null, "found null Directed Edge")
        de.setVisited(true)
        de = de.getNext
      }; de ne startDe
    }) ()
  }

  /**
   * Check if any shell ring has an unvisited edge. A shell ring is a ring which is not a hole and
   * which has the interior of the parent area on the RHS. (Note that there may be non-hole rings
   * with the interior on the LHS, since the interior of holes will also be polygonized into CW
   * rings by the linkAllDirectedEdges() step)
   *
   * return true if there is an unvisited edge in a non-hole ring
   */
  private def hasUnvisitedShellEdge(edgeRings: util.List[MinimalEdgeRing]): Boolean = {
    var i = 0
    while (i < edgeRings.size) {
      val er = edgeRings.get(i)
      // don't check hole rings
      if (!er.isHole) {
        val edges = er.getEdges
        var de    = edges.get(0).asInstanceOf[DirectedEdge]
        // don't check CW rings which are holes
        // (MD - this check may now be irrelevant)
        if (de.getLabel.getLocation(0, Position.RIGHT) == Location.INTERIOR) {

          /**
           * the edgeRing is CW ring which surrounds the INT of the area, so check all edges have
           * been visited. If any are unvisited, this is a disconnected part of the interior
           */
          var j = 0
          while (j < edges.size) {
            de = edges.get(j).asInstanceOf[DirectedEdge]
            // Debug.print("visted? "); Debug.println(de);
            if (!de.isVisited) { // Debug.print("not visited "); Debug.println(de);
              disconnectedRingcoord = de.getCoordinate
              return true
            }
            j += 1
          }
        }
      }
      i += 1
    }
    false
  }
}
