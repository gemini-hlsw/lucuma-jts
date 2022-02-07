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
package org.locationtech.jts.geomgraph

import org.locationtech.jts.geom.Location

/**
 * A TopologyLocation is the labelling of a GraphComponent's topological relationship to a single
 * Geometry. <p> If the parent component is an area edge, each side and the edge itself have a
 * topological location. These locations are named <ul> <li> ON: on the edge <li> LEFT: left-hand
 * side of the edge <li> RIGHT: right-hand side </ul> If the parent component is a line edge or
 * node, there is a single topological relationship attribute, ON. <p> The possible values of a
 * topological location are {Location.NONE, Location.EXTERIOR, Location.BOUNDARY, Location.INTERIOR}
 * <p> The labelling is stored in an array location[j] where where j has the values ON, LEFT, RIGHT
 *
 * @version 1.7
 */
class TopologyLocation(var location: Array[Int] = Array.empty[Int]) {
  init(location.length)

  /**
   * Constructs a TopologyLocation specifying how points on, to the left of, and to the right of
   * some GraphComponent relate to some Geometry. Possible values for the parameters are
   * Location.NULL, Location.EXTERIOR, Location.BOUNDARY, and Location.INTERIOR.
   *
   * @see
   *   Location
   */
  def this(on: Int, left: Int, right: Int) = {
    this()
    init(3)
    location(Position.ON) = on
    location(Position.LEFT) = left
    location(Position.RIGHT) = right
  }

  def this(on: Int) = {
    this()
    init(1)
    location(Position.ON) = on
  }

  def this(gl: TopologyLocation) = {
    this()
    init(gl.location.length)
    if (gl != null) {
      var i = 0
      while (i < location.length) {
        location(i) = gl.location(i)
        i += 1
      }
    }
  }

  private def init(size: Int): Unit = {
    location = new Array[Int](size)
    setAllLocations(Location.NONE)
  }

  def get(posIndex: Int): Int = {
    if (posIndex < location.length) return location(posIndex)
    Location.NONE
  }

  /**
   * return true if all locations are NULL
   */
  def isNull: Boolean = {
    var i = 0
    while (i < location.length) {
      if (location(i) != Location.NONE) return false
      i += 1
    }
    true
  }

  /**
   * return true if any locations are NULL
   */
  def isAnyNull: Boolean = {
    var i = 0
    while (i < location.length) {
      if (location(i) == Location.NONE) return true
      i += 1
    }
    false
  }

  def isEqualOnSide(le: TopologyLocation, locIndex: Int): Boolean =
    location(locIndex) == le.location(locIndex)

  def isArea: Boolean = location.length > 1

  def isLine: Boolean = location.length == 1

  def flip(): Unit = {
    if (location.length <= 1) return
    val temp = location(Position.LEFT)
    location(Position.LEFT) = location(Position.RIGHT)
    location(Position.RIGHT) = temp
  }

  def setAllLocations(locValue: Int): Unit = {
    var i = 0
    while (i < location.length) {
      location(i) = locValue
      i += 1
    }
  }

  def setAllLocationsIfNull(locValue: Int): Unit = {
    var i = 0
    while (i < location.length) {
      if (location(i) == Location.NONE) location(i) = locValue
      i += 1
    }
  }

  def setLocation(locIndex: Int, locValue: Int): Unit = location(locIndex) = locValue

  def setLocation(locValue: Int): Unit = setLocation(Position.ON, locValue)

  def getLocations: Array[Int] = location

  def setLocations(on: Int, left: Int, right: Int): Unit = {
    location(Position.ON) = on
    location(Position.LEFT) = left
    location(Position.RIGHT) = right
  }

  def allPositionsEqual(loc: Int): Boolean = {
    var i = 0
    while (i < location.length) {
      if (location(i) != loc) return false
      i += 1
    }
    true
  }

  /**
   * merge updates only the NULL attributes of this object with the attributes of another.
   */
  def merge(gl: TopologyLocation): Unit = { // if the src is an Area label & and the dest is not, increase the dest to be an Area
    if (gl.location.length > location.length) {
      val newLoc = new Array[Int](3)
      newLoc(Position.ON) = location(Position.ON)
      newLoc(Position.LEFT) = Location.NONE
      newLoc(Position.RIGHT) = Location.NONE
      location = newLoc
    }
    var i = 0
    while (i < location.length) {
      if (location(i) == Location.NONE && i < gl.location.length) location(i) = gl.location(i)
      i += 1
    }
  }

  override def toString: String = {
    val buf = new StringBuffer
    if (location.length > 1) buf.append(Location.toLocationSymbol(location(Position.LEFT)))
    buf.append(Location.toLocationSymbol(location(Position.ON)))
    if (location.length > 1) buf.append(Location.toLocationSymbol(location(Position.RIGHT)))
    buf.toString
  }
}
