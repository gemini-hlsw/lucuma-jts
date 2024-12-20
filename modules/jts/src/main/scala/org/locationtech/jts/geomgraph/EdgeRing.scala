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

import org.locationtech.jts.algorithm.Orientation
import org.locationtech.jts.algorithm.PointLocation
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.LinearRing
import org.locationtech.jts.geom.Location
import org.locationtech.jts.geom.Polygon
import org.locationtech.jts.geom.Position
import org.locationtech.jts.geom.TopologyException
import org.locationtech.jts.util.Assert

import java.util

/**
 * @version 1.7
 */
abstract class EdgeRing(val start: DirectedEdge, var geometryFactory: GeometryFactory) {
  protected var startDe: DirectedEdge =
    null // the directed edge which starts the list of edges for this EdgeRing
  private var maxNodeDegree    = -1
  private val edges            = new util.ArrayList[DirectedEdge] // the DirectedEdges making up this EdgeRing
  private val pts              = new util.ArrayList[Coordinate]
  private val label            =
    new Label(
      Location.NONE
    ) // label stores the locations of each geometry on the face surrounded by this ring
  private var ring: LinearRing = null                             // the ring created for this EdgeRing
  private var visHole          = false
  private var shell: EdgeRing  =
    null // if non-null, the ring is a hole and this EdgeRing is its containing shell
  private val holes            =
    new util.ArrayList[EdgeRing] // a list of EdgeRings which are holes in this EdgeRing
  def isIsolated: Boolean      = label.getGeometryCount == 1
  computePoints(start)
  computeRing()

  def isHole: Boolean = // computePoints();
    visHole

  def getCoordinate(i: Int): Coordinate = pts.get(i)

  def getLinearRing: LinearRing = ring

  def getLabel: Label = label

  def isShell: Boolean = shell == null

  def getShell: EdgeRing = shell

  def setShell(shell: EdgeRing): Unit = {
    this.shell = shell
    if (shell != null) shell.addHole(this)
    ()
  }

  def addHole(ring: EdgeRing): Boolean = holes.add(ring)

  def toPolygon(geometryFactory: GeometryFactory): Polygon = {
    val holeLR = new Array[LinearRing](holes.size)
    var i      = 0
    while (i < holes.size) {
      holeLR(i) = holes.get(i).getLinearRing
      i += 1
    }
    val poly   = geometryFactory.createPolygon(getLinearRing, holeLR)
    poly
  }

  /**
   * Compute a LinearRing from the point list previously collected. Test if the ring is a hole (i.e.
   * if it is CCW) and set the hole flag accordingly.
   */
  def computeRing(): Unit = {
    if (ring != null) return // don't compute more than once
    val coord = new Array[Coordinate](pts.size)
    var i     = 0
    while (i < pts.size) {
      coord(i) = pts.get(i).asInstanceOf[Coordinate]
      i += 1
    }
    ring = geometryFactory.createLinearRing(coord)
    visHole = Orientation.isCCW(ring.getCoordinates)
    // Debug.println( (isHole ? "hole - " : "shell - ") + WKTWriter.toLineString(new CoordinateArraySequence(ring.getCoordinates())));
  }

  def getNext(de: DirectedEdge): DirectedEdge

  def setEdgeRing(de: DirectedEdge, er: EdgeRing): Unit

  /**
   * Returns the list of DirectedEdges that make up this EdgeRing
   */
  def getEdges: util.ArrayList[_] = edges

  /**
   * Collect all the points from the DirectedEdges of this ring into a contiguous list
   */
  protected def computePoints(start: DirectedEdge): Unit = { // System.out.println("buildRing");
    startDe = start
    var de          = start
    var isFirstEdge = true
    while ({
      { //      Assert.isTrue(de != null, "found null Directed Edge");
        if (de == null) throw new TopologyException("Found null DirectedEdge")
        if (de.getEdgeRing eq this)
          throw new TopologyException(
            "Directed Edge visited twice during ring-building at " + de.getCoordinate
          )
        edges.add(de)
        // Debug.println(de);
        // Debug.println(de.getEdge());
        val label = de.getLabel
        Assert.isTrue(label.isArea)
        mergeLabel(label)
        addPoints(de.getEdge, de.isForward, isFirstEdge)
        isFirstEdge = false
        setEdgeRing(de, this)
        de = getNext(de)
      }; de != startDe
    }) ()
  }

  def getMaxNodeDegree: Int = {
    if (maxNodeDegree < 0) computeMaxNodeDegree()
    maxNodeDegree
  }

  private def computeMaxNodeDegree(): Unit = {
    maxNodeDegree = 0
    var de = startDe
    while ({
      {
        val node   = de.getNode
        val degree = node.getEdges.asInstanceOf[DirectedEdgeStar].getOutgoingDegree(this)
        if (degree > maxNodeDegree) maxNodeDegree = degree
        de = getNext(de)
      }; de != startDe
    }) ()
    maxNodeDegree *= 2
  }

  def setInResult(): Unit = {
    var de = startDe
    while ({
      {
        de.getEdge.setInResult(true)
        de = de.getNext
      }; de ne startDe
    }) ()
  }

  protected def mergeLabel(deLabel: Label): Unit = {
    mergeLabel(deLabel, 0)
    mergeLabel(deLabel, 1)
  }

  /**
   * Merge the RHS label from a DirectedEdge into the label for this EdgeRing. The DirectedEdge
   * label may be null. This is acceptable - it results from a node which is NOT an intersection
   * node between the Geometries (e.g. the end node of a LinearRing). In this case the DirectedEdge
   * label does not contribute any information to the overall labelling, and is simply skipped.
   */
  protected def mergeLabel(deLabel: Label, geomIndex: Int): Unit = {
    val loc = deLabel.getLocation(geomIndex, Position.RIGHT)
    // no information to be had from this label
    if (loc == Location.NONE) return
    // if there is no current RHS value, set it
    if (label.getLocation(geomIndex) == Location.NONE) {
      label.setLocation(geomIndex, loc)
      return
    }
  }

  protected def addPoints(edge: Edge, isForward: Boolean, isFirstEdge: Boolean): Unit = {
    val edgePts = edge.getCoordinates
    if (isForward) {
      var startIndex = 1
      if (isFirstEdge) startIndex = 0
      var i          = startIndex
      while (i < edgePts.length) {
        pts.add(edgePts(i))
        i += 1
      }
    } else { // is backward
      var startIndex = edgePts.length - 2
      if (isFirstEdge) startIndex = edgePts.length - 1
      var i          = startIndex
      while (i >= 0) {
        pts.add(edgePts(i))
        i -= 1
      }
    }
  }

  /**
   * This method will cause the ring to be computed. It will also check any holes, if they have been
   * assigned.
   */
  def containsPoint(p: Coordinate): Boolean = {
    val shell = getLinearRing
    val env   = shell.getEnvelopeInternal
    if (!env.contains(p)) return false
    if (!PointLocation.isInRing(p, shell.getCoordinates)) return false
    val i     = holes.iterator
    while (i.hasNext) {
      val hole = i.next
      if (hole.containsPoint(p)) return false
    }
    true
  }
}
