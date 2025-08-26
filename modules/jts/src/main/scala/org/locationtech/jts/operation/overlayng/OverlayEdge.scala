// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.operation.overlayng

import org.locationtech.jts.edgegraph.HalfEdge
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateArrays
import org.locationtech.jts.geom.CoordinateList
import org.locationtech.jts.io.WKTWriter

import java.util.Comparator

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

class OverlayEdge(
  override val orig: Coordinate,
  var dirPt:         Coordinate,

  /**
   * <code>true</code> indicates direction is forward along segString <code>false</code> is reverse
   * direction The label must be interpreted accordingly.
   */
  var direction: Boolean,
  var label:     OverlayLabel,
  var pts:       Array[Coordinate]
) extends HalfEdge(orig) {
  private var isInResultArea0 = false
  private var isInResultLine0 = false
  private var isVisited0      = false

  /**
   * Link to next edge in the result ring. The origin of the edge is the dest of this edge.
   */
  private var nextResultEdge: OverlayEdge    = null
  private var edgeRing: OverlayEdgeRing      = null
  private var maxEdgeRing: MaximalEdgeRing   = null
  private var nextResultMaxEdge: OverlayEdge = null

  def isForward: Boolean = direction

  override def directionPt: Coordinate = dirPt

  def getLabel: OverlayLabel = label

  def getLocation(index: Int, position: Int): Int = label.getLocation(index, position, direction)

  def getCoordinate: Coordinate = orig

  def getCoordinates: Array[Coordinate] = pts

  def getCoordinatesOriented: Array[Coordinate] = {
    if (direction) return pts
    val copy = pts.clone
    CoordinateArrays.reverse(copy)
    copy
  }

  /**
   * Adds the coordinates of this edge to the given list, in the direction of the edge. Duplicate
   * coordinates are removed (which means that this is safe to use for a path of connected edges in
   * the topology graph).
   *
   * @param coords
   *   the coordinate list to add to
   */
  def addCoordinates(coords: CoordinateList): Unit = {
    val isFirstEdge = coords.size > 0
    if (direction) {
      var startIndex = 1
      if (isFirstEdge) startIndex = 0
      for (i <- startIndex until pts.length)
        coords.add(pts(i), false)
    } else { // is backward
      var startIndex = pts.length - 2
      if (isFirstEdge) startIndex = pts.length - 1
      for (i <- startIndex to 0 by -1)
        coords.add(pts(i), false)
    }
  }

  /**
   * Gets the symmetric pair edge of this edge.
   *
   * @return
   *   the symmetric pair edge
   */
  def symOE: OverlayEdge = sym.asInstanceOf[OverlayEdge]

  /**
   * Gets the next edge CCW around the origin of this edge, with the same origin. If the origin
   * vertex has degree 1 then this is the edge itself.
   *
   * @return
   *   the next edge around the origin
   */
  def oNextOE: OverlayEdge = oNext.asInstanceOf[OverlayEdge]

  def isInResultArea: Boolean = isInResultArea0

  def isInResultAreaBoth: Boolean = isInResultArea0 && symOE.isInResultArea0

  def unmarkFromResultAreaBoth(): Unit = {
    isInResultArea0 = false
    symOE.isInResultArea0 = false
  }

  def markInResultArea(): Unit =
    isInResultArea0 = true

  def markInResultAreaBoth(): Unit = {
    isInResultArea0 = true
    symOE.isInResultArea0 = true
  }

  def isInResultLine: Boolean = isInResultLine0

  def markInResultLine(): Unit = {
    isInResultLine0 = true
    symOE.isInResultLine0 = true
  }

  def isInResult: Boolean = isInResultArea0 || isInResultLine0

  def isInResultEither: Boolean = isInResult || symOE.isInResult

  def setNextResult(e: OverlayEdge): Unit = // Assert: e.orig() == this.dest();
    nextResultEdge = e

  def nextResult: OverlayEdge = nextResultEdge

  def isResultLinked: Boolean = nextResultEdge != null

  def setNextResultMax(e: OverlayEdge): Unit =
    nextResultMaxEdge = e

  def nextResultMax: OverlayEdge = nextResultMaxEdge

  def isResultMaxLinked: Boolean = nextResultMaxEdge != null

  def isVisited: Boolean = isVisited0

  private def markVisited(): Unit =
    isVisited0 = true

  def markVisitedBoth(): Unit = {
    markVisited()
    symOE.markVisited()
  }

  def setEdgeRing(edgeRing: OverlayEdgeRing): Unit =
    this.edgeRing = edgeRing

  def getEdgeRing: OverlayEdgeRing = edgeRing

  def getEdgeRingMax: MaximalEdgeRing = maxEdgeRing

  def setEdgeRingMax(maximalEdgeRing: MaximalEdgeRing): Unit =
    maxEdgeRing = maximalEdgeRing

  override def toString = {
    // val orig     = orig()
    // val dest     = dest()
    val dirPtStr = if (pts.length > 2) {
      ", " + WKTWriter.format(directionPt)
    } else ""

    "OE( " + WKTWriter.format(orig) + dirPtStr + " .. " + WKTWriter.format(dest) + " ) " + label
      .toString(direction) + resultSymbol + " / Sym: " + symOE.getLabel
      .toString(symOE.direction) + symOE.resultSymbol
  }

  private def resultSymbol: String = {
    if (isInResultArea0) return " resA"
    if (isInResultLine0) return " resL"
    ""
  }
}

object OverlayEdge {

  /**
   * Creates a single OverlayEdge.
   *
   * @param pts
   * @param lbl
   * @param direction
   * @return
   *   a new edge based on the given coordinates and direction
   */
  def createEdge(pts: Array[Coordinate], lbl: OverlayLabel, direction: Boolean): OverlayEdge = {
    var origin: Coordinate = null
    var dirPt: Coordinate  = null
    if (direction) {
      origin = pts(0)
      dirPt = pts(1)
    } else {
      val ilast = pts.length - 1
      origin = pts(ilast)
      dirPt = pts(ilast - 1)
    }
    new OverlayEdge(origin, dirPt, direction, lbl, pts)
  }

  def createEdgePair(pts: Array[Coordinate], lbl: OverlayLabel): OverlayEdge = {
    val e0 = OverlayEdge.createEdge(pts, lbl, true)
    val e1 = OverlayEdge.createEdge(pts, lbl, false)
    e0.link(e1)
    e0
  }

  /**
   * Gets a {@link Comparator} which sorts by the origin Coordinates.
   *
   * @return
   *   a Comparator sorting by origin coordinate
   */
  def nodeComparator: Comparator[OverlayEdge] = new Comparator[OverlayEdge]() {
    override def compare(e1: OverlayEdge, e2: OverlayEdge): Int = return e1.orig.compareTo(e2.orig)
  }
}
