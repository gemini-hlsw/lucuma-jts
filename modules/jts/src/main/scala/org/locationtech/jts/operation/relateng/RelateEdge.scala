// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2023 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v20.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.operation.relateng

import java.util

import org.locationtech.jts.algorithm.PolygonNodeTopology
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Dimension
import org.locationtech.jts.geom.Location
import org.locationtech.jts.geom.Position
import org.locationtech.jts.io.WKTWriter
import org.locationtech.jts.util.Assert

class RelateEdge private (private val node: RelateNode, private val dirPt: Coordinate) {

  private var aDim      = RelateEdge.DIM_UNKNOWN
  private var aLocLeft  = RelateEdge.LOC_UNKNOWN
  private var aLocRight = RelateEdge.LOC_UNKNOWN
  private var aLocLine  = RelateEdge.LOC_UNKNOWN

  private var bDim      = RelateEdge.DIM_UNKNOWN
  private var bLocLeft  = RelateEdge.LOC_UNKNOWN
  private var bLocRight = RelateEdge.LOC_UNKNOWN
  private var bLocLine  = RelateEdge.LOC_UNKNOWN

  def this(node: RelateNode, pt: Coordinate, isA: Boolean, isForward: Boolean) = {
    this(node, pt)
    setLocationsArea(isA, isForward)
  }

  def this(node: RelateNode, pt: Coordinate, isA: Boolean) = {
    this(node, pt)
    setLocationsLine(isA)
  }

  def this(
    node:     RelateNode,
    pt:       Coordinate,
    isA:      Boolean,
    locLeft:  Int,
    locRight: Int,
    locLine:  Int
  ) = {
    this(node, pt)
    setLocations(isA, locLeft, locRight, locLine)
  }

  private def setLocations(isA: Boolean, locLeft: Int, locRight: Int, locLine: Int): Unit =
    if (isA) {
      aDim = 2
      aLocLeft = locLeft
      aLocRight = locRight
      aLocLine = locLine
    } else {
      bDim = 2
      bLocLeft = locLeft
      bLocRight = locRight
      bLocLine = locLine
    }

  private def setLocationsLine(isA: Boolean): Unit =
    if (isA) {
      aDim = 1
      aLocLeft = Location.EXTERIOR
      aLocRight = Location.EXTERIOR
      aLocLine = Location.INTERIOR
    } else {
      bDim = 1
      bLocLeft = Location.EXTERIOR
      bLocRight = Location.EXTERIOR
      bLocLine = Location.INTERIOR
    }

  private def setLocationsArea(isA: Boolean, isForward: Boolean): Unit = {
    val locLeft  = if (isForward) Location.EXTERIOR else Location.INTERIOR
    val locRight = if (isForward) Location.INTERIOR else Location.EXTERIOR
    if (isA) {
      aDim = 2
      aLocLeft = locLeft
      aLocRight = locRight
      aLocLine = Location.BOUNDARY
    } else {
      bDim = 2
      bLocLeft = locLeft
      bLocRight = locRight
      bLocLine = Location.BOUNDARY
    }
  }

  def compareToEdge(edgeDirPt: Coordinate): Int =
    PolygonNodeTopology.compareAngle(node.getCoordinate, this.dirPt, edgeDirPt)

  def merge(isA: Boolean, dirPt: Coordinate, dim: Int, isForward: Boolean): Unit = {
    var locEdge  = Location.INTERIOR
    var locLeft  = Location.EXTERIOR
    var locRight = Location.EXTERIOR
    if (dim == Dimension.A) {
      locEdge = Location.BOUNDARY
      locLeft = if (isForward) Location.EXTERIOR else Location.INTERIOR
      locRight = if (isForward) Location.INTERIOR else Location.EXTERIOR
    }

    if (!isKnown(isA)) {
      setDimension(isA, dim)
      setOn(isA, locEdge)
      setLeft(isA, locLeft)
      setRight(isA, locRight)
      return
    }

    // Assert: node-dirpt is collinear with node-pt
    mergeDimEdgeLoc(isA, locEdge)
    mergeSideLocation(isA, Position.LEFT, locLeft)
    mergeSideLocation(isA, Position.RIGHT, locRight)
  }

  /**
   * Area edges override Line edges. Merging edges of same dimension is a no-op for the dimension
   * and on location. But merging an area edge into a line edge sets the dimension to A and the
   * location to BOUNDARY.
   *
   * @param isA
   * @param locEdge
   */
  private def mergeDimEdgeLoc(isA: Boolean, locEdge: Int): Unit = {
    // TODO: this logic needs work - ie handling A edges marked as Interior
    val dim = if (locEdge == Location.BOUNDARY) Dimension.A else Dimension.L
    if (dim == Dimension.A && dimension(isA) == Dimension.L) {
      setDimension(isA, dim)
      setOn(isA, Location.BOUNDARY)
    }
  }

  private def mergeSideLocation(isA: Boolean, pos: Int, loc: Int): Unit = {
    val currLoc = location(isA, pos)
    // -- INTERIOR takes precedence over EXTERIOR
    if (currLoc != Location.INTERIOR) {
      setLocation(isA, pos, loc)
    }
  }

  private def setDimension(isA: Boolean, dimension: Int): Unit =
    if (isA) {
      aDim = dimension
    } else {
      bDim = dimension
    }

  def setLocation(isA: Boolean, pos: Int, loc: Int): Unit =
    pos match {
      case Position.LEFT  =>
        setLeft(isA, loc)
      case Position.RIGHT =>
        setRight(isA, loc)
      case Position.ON    =>
        setOn(isA, loc)
      case _              =>
    }

  def setAllLocations(isA: Boolean, loc: Int): Unit = {
    setLeft(isA, loc)
    setRight(isA, loc)
    setOn(isA, loc)
  }

  def setUnknownLocations(isA: Boolean, loc: Int): Unit = {
    if (!isKnown(isA, Position.LEFT)) {
      setLocation(isA, Position.LEFT, loc)
    }
    if (!isKnown(isA, Position.RIGHT)) {
      setLocation(isA, Position.RIGHT, loc)
    }
    if (!isKnown(isA, Position.ON)) {
      setLocation(isA, Position.ON, loc)
    }
  }

  private def setLeft(isA: Boolean, loc: Int): Unit =
    if (isA) {
      aLocLeft = loc
    } else {
      bLocLeft = loc
    }

  private def setRight(isA: Boolean, loc: Int): Unit =
    if (isA) {
      aLocRight = loc
    } else {
      bLocRight = loc
    }

  private def setOn(isA: Boolean, loc: Int): Unit =
    if (isA) {
      aLocLine = loc
    } else {
      bLocLine = loc
    }

  def location(isA: Boolean, position: Int): Int = {
    if (isA) {
      position match {
        case Position.LEFT  => return aLocLeft
        case Position.RIGHT => return aLocRight
        case Position.ON    => return aLocLine
        case _              =>
      }
    } else {
      position match {
        case Position.LEFT  => return bLocLeft
        case Position.RIGHT => return bLocRight
        case Position.ON    => return bLocLine
        case _              =>
      }
    }
    Assert.shouldNeverReachHere()
    RelateEdge.LOC_UNKNOWN
  }

  private def dimension(isA: Boolean): Int =
    if (isA) aDim else bDim

  private def isKnown(isA: Boolean): Boolean = {
    if (isA)
      return aDim != RelateEdge.DIM_UNKNOWN
    bDim != RelateEdge.DIM_UNKNOWN
  }

  private def isKnown(isA: Boolean, pos: Int): Boolean =
    location(isA, pos) != RelateEdge.LOC_UNKNOWN

  def isInterior(isA: Boolean, position: Int): Boolean =
    location(isA, position) == Location.INTERIOR

  def setDimLocations(isA: Boolean, dim: Int, loc: Int): Unit =
    if (isA) {
      aDim = dim
      aLocLeft = loc
      aLocRight = loc
      aLocLine = loc
    } else {
      bDim = dim
      bLocLeft = loc
      bLocRight = loc
      bLocLine = loc
    }

  def setAreaInterior(isA: Boolean): Unit =
    if (isA) {
      aLocLeft = Location.INTERIOR
      aLocRight = Location.INTERIOR
      aLocLine = Location.INTERIOR
    } else {
      bLocLeft = Location.INTERIOR
      bLocRight = Location.INTERIOR
      bLocLine = Location.INTERIOR
    }

  override def toString: String =
    WKTWriter.toLineString(node.getCoordinate, dirPt) + " - " + labelString

  private def labelString: String = {
    val buf = new StringBuilder
    buf.append("A:")
    buf.append(locationString(RelateGeometry.GEOM_A))
    buf.append("/B:")
    buf.append(locationString(RelateGeometry.GEOM_B))
    buf.toString
  }

  private def locationString(isA: Boolean): String = {
    val buf = new StringBuilder
    buf.append(Location.toLocationSymbol(location(isA, Position.LEFT)))
    buf.append(Location.toLocationSymbol(location(isA, Position.ON)))
    buf.append(Location.toLocationSymbol(location(isA, Position.RIGHT)))
    buf.toString
  }

}

object RelateEdge {

  val IS_FORWARD = true
  val IS_REVERSE = false

  def create(
    node:      RelateNode,
    dirPt:     Coordinate,
    isA:       Boolean,
    dim:       Int,
    isForward: Boolean
  ): RelateEdge = {
    if (dim == Dimension.A)
      // -- create an area edge
      return new RelateEdge(node, dirPt, isA, isForward)
    // -- create line edge
    new RelateEdge(node, dirPt, isA)
  }

  def findKnownEdgeIndex(edges: util.List[RelateEdge], isA: Boolean): Int = {
    var i = 0
    while (i < edges.size) {
      val e = edges.get(i)
      if (e.isKnown(isA))
        return i
      i += 1
    }
    -1
  }

  def setAreaInterior(edges: util.List[RelateEdge], isA: Boolean): Unit = {
    val it = edges.iterator
    while (it.hasNext) {
      val e = it.next()
      e.setAreaInterior(isA)
    }
  }

  /**
   * The dimension of an input geometry which is not known
   */
  val DIM_UNKNOWN: Int = -1

  /**
   * Indicates that the location is currently unknown
   */
  private val LOC_UNKNOWN: Int = Location.NONE

}
