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

import org.locationtech.jts.algorithm.LineIntersector
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.IntersectionMatrix
import org.locationtech.jts.geom.Position
import org.locationtech.jts.geomgraph.index.MonotoneChainEdge

import java.io.PrintStream

/**
 * @version 1.7
 */
object Edge {

  /**
   * Updates an IM from the label for an edge. Handles edges from both L and A geometries.
   */
  def updateIM(label: Label, im: IntersectionMatrix): Unit = {
    im.setAtLeastIfValid(label.getLocation(0, Position.ON), label.getLocation(1, Position.ON), 1)
    if (label.isArea) {
      im.setAtLeastIfValid(label.getLocation(0, Position.LEFT),
                           label.getLocation(1, Position.LEFT),
                           2
      )
      im.setAtLeastIfValid(label.getLocation(0, Position.RIGHT),
                           label.getLocation(1, Position.RIGHT),
                           2
      )
    }
  }
}

class Edge(var pts: Array[Coordinate], val labelArg: Label) extends GraphComponent(labelArg) {
  private var env: Envelope          = null
  private[geomgraph] val eiList      = new EdgeIntersectionList(this)
  private var name: String           = null
  private var mce: MonotoneChainEdge = null
  private var visIsolated            = true
  private val depth                  = new Depth
  private var depthDelta             = 0 // the change in area depth from the R to L side of this edge
  def this(pts: Array[Coordinate]) =
    this(pts, null)

  def getNumPoints: Int = pts.length

  def setName(name: String): Unit = this.name = name

  def getCoordinates: Array[Coordinate] = pts

  def getCoordinate(i: Int): Coordinate = pts(i)

  override def getCoordinate: Coordinate = {
    if (pts.nonEmpty) return pts(0)
    null
  }

  def getEnvelope: Envelope = { // compute envelope lazily
    if (env == null) {
      env = new Envelope
      var i = 0
      while (i < pts.length) {
        env.expandToInclude(pts(i))
        i += 1
      }
    }
    env
  }

  def getDepth: Depth = depth

  /**
   * The depthDelta is the change in depth as an edge is crossed from R to L
   *
   * return the change in depth as the edge is crossed from R to L
   */
  def getDepthDelta: Int = depthDelta

  def setDepthDelta(depthDelta: Int): Unit = this.depthDelta = depthDelta

  def getMaximumSegmentIndex: Int = pts.length - 1

  def getEdgeIntersectionList: EdgeIntersectionList = eiList

  def getMonotoneChainEdge: MonotoneChainEdge = {
    if (mce == null) mce = new MonotoneChainEdge(this)
    mce
  }

  def isClosed: Boolean = pts(0) == pts(pts.length - 1)

  /**
   * An Edge is collapsed if it is an Area edge and it consists of two segments which are equal and
   * opposite (eg a zero-width V).
   */
  def isCollapsed: Boolean = {
    if (!label.isArea) return false
    if (pts.length != 3) return false
    if (pts(0) == pts(2)) return true
    false
  }

  def getCollapsedEdge: Edge = {
    val newPts = new Array[Coordinate](2)
    newPts(0) = pts(0)
    newPts(1) = pts(1)
    val newe   = new Edge(newPts, Label.toLineLabel(label))
    newe
  }

  def setIsolated(isIsolated: Boolean): Unit = this.visIsolated = isIsolated

  override def isIsolated: Boolean = visIsolated

  /**
   * Adds EdgeIntersections for one or both intersections found for a segment of an edge to the edge
   * intersection list.
   */
  def addIntersections(li: LineIntersector, segmentIndex: Int, geomIndex: Int): Unit = {
    var i = 0
    while (i < li.getIntersectionNum) {
      addIntersection(li, segmentIndex, geomIndex, i)
      i += 1
    }
  }

  /**
   * Add an EdgeIntersection for intersection intIndex. An intersection that falls exactly on a
   * vertex of the edge is normalized to use the higher of the two possible segmentIndexes
   */
  def addIntersection(
    li:           LineIntersector,
    segmentIndex: Int,
    geomIndex:    Int,
    intIndex:     Int
  ): Unit = {
    val intPt                  = new Coordinate(li.getIntersection(intIndex))
    var normalizedSegmentIndex = segmentIndex
    var dist                   = li.getEdgeDistance(geomIndex, intIndex)
    // Debug.println("edge intpt: " + intPt + " dist: " + dist);
    // normalize the intersection point location
    val nextSegIndex           = normalizedSegmentIndex + 1
    if (nextSegIndex < pts.length) {
      val nextPt = pts(nextSegIndex)
      // Debug.println("next pt: " + nextPt);
      // Normalize segment index if intPt falls on vertex
      // The check for point equality is 2D only - Z values are ignored
      if (intPt.equals2D(nextPt)) { // Debug.println("normalized distance");
        normalizedSegmentIndex = nextSegIndex
        dist = 0.0
      }
    }

    /**
     * Add the intersection point to edge intersection list.
     */
    eiList.add(intPt, normalizedSegmentIndex, dist)
    ()
    // ei.print(System.out);
  }

  /**
   * Update the IM with the contribution for this component. A component only contributes if it has
   * a labelling for both parent geometries
   */
  override def computeIM(im: IntersectionMatrix): Unit = Edge.updateIM(label, im)

  /**
   * equals is defined to be: <p> e1 equals e2 <b>iff</b> the coordinates of e1 are the same or the
   * reverse of the coordinates in e2
   */
  override def equals(o: Any): Boolean = {
    if (!o.isInstanceOf[Edge]) return false
    val e              = o.asInstanceOf[Edge]
    if (pts.length != e.pts.length) return false
    var isEqualForward = true
    var isEqualReverse = true
    var iRev           = pts.length
    var i              = 0
    while (i < pts.length) {
      if (!pts(i).equals2D(e.pts(i))) isEqualForward = false
      if (
        !pts(i).equals2D(e.pts {
          iRev -= 1; iRev
        })
      ) isEqualReverse = false
      if (!isEqualForward && !isEqualReverse) return false
      i += 1
    }
    true
  }

  /**
   * return true if the coordinate sequences of the Edges are identical
   */
  def isPointwiseEqual(e: Edge): Boolean = {
    if (pts.length != e.pts.length) return false
    var i = 0
    while (i < pts.length) {
      if (!pts(i).equals2D(e.pts(i))) return false
      i += 1
    }
    true
  }

  override def toString: String = {
    val builder = new StringBuilder
    builder.append("edge " + name + ": ")
    builder.append("LINESTRING (")
    var i       = 0
    while (i < pts.length) {
      if (i > 0) builder.append(",")
      builder.append(s"${pts(i).x} ${pts(i).y}")
      i += 1
    }
    builder.append(")  " + label + " " + depthDelta)
    builder.toString
  }

  def print(out: PrintStream): Unit = {
    out.print("edge " + name + ": ")
    out.print("LINESTRING (")
    var i = 0
    while (i < pts.length) {
      if (i > 0) out.print(",")
      out.print(s"${pts(i).x} ${pts(i).y}")
      i += 1
    }
    out.print(")  " + label + " " + depthDelta)
  }

  def printReverse(out: PrintStream): Unit = {
    out.print("edge " + name + ": ")
    var i = pts.length - 1
    while (i >= 0) {
      out.print(s"${pts(i)} ")
      i -= 1
    }
    out.println("")
  }
}
