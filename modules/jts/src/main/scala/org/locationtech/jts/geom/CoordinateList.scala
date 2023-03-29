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
package org.locationtech.jts.geom

/**
 * A list of {link Coordinate}s, which may be set to prevent repeated coordinates from occurring in
 * the list.
 *
 * @version 1.7
 */
@SerialVersionUID(-1626110935756089896L)
object CoordinateList { // With contributions from Markus Schaber [schabios@logi-track.com]
  // [Jon Aquino 2004-03-25]
  private val coordArrayType = new Array[Coordinate](0)
}

@SerialVersionUID(-1626110935756089896L)
class CoordinateList(coord: Array[Coordinate], allowRepeated: Boolean)
    extends java.util.ArrayList[Coordinate] {
  add(coord, allowRepeated)

  /**
   * Constructs a new list from an array of Coordinates, allowing repeated points. (I.e. this
   * constructor produces a {link CoordinateList} with exactly the same set of points as the input
   * array.)
   *
   * @param coord
   *   the initial coordinates
   */
  def this(coord: Array[Coordinate] = Array.empty) = {
    this(coord, true)
    ensureCapacity(coord.length)
  }

  override def add(coord: Coordinate): Boolean = super.add(coord)

  def getCoordinate(i: Int): Coordinate = get(i)

  /**
   * Adds a section of an array of coordinates to the list.
   *
   * @param coord
   *   The coordinates
   * @param allowRepeated
   *   if set to false, repeated coordinates are collapsed
   * @param start
   *   the index to start from
   * @param end
   *   the index to add up to but not including return true (as by general collection contract)
   */
  def add(coord: Array[Coordinate], allowRepeated: Boolean, start: Int, end: Int): Boolean = {
    var inc = 1
    if (start > end) inc = -1
    var i   = start
    while (i != end) {
      add(coord(i), allowRepeated)
      i += inc
    }
    true
  }

  /**
   * Adds an array of coordinates to the list.
   *
   * @param coord
   *   The coordinates
   * @param allowRepeated
   *   if set to false, repeated coordinates are collapsed
   * @param direction
   *   if false, the array is added in reverse order return true (as by general collection contract)
   */
  def add(coord: Array[Coordinate], allowRepeated: Boolean, direction: Boolean): Boolean = {
    if (direction) {
      coord.foreach(add(_, allowRepeated))
    } else {
      coord.reverse.foreach(add(_, allowRepeated))
//      var i = coord.length - 1
//      while ( {
//        i >= 0
//      }) {
//        add(coord(i), allowRepeated)
//        {
//          i -= 1; i + 1
//        }
//      }
    }
    true
  }

  /**
   * Adds an array of coordinates to the list.
   *
   * @param coord
   *   The coordinates
   * @param allowRepeated
   *   if set to false, repeated coordinates are collapsed return true (as by general collection
   *   contract)
   */
  def add(coord: Array[Coordinate], allowRepeated: Boolean): Boolean = {
    add(coord, allowRepeated, true)
    true
  }

  /**
   * Adds a coordinate to the list.
   *
   * @param obj
   *   The coordinate to add
   * @param allowRepeated
   *   if set to false, repeated coordinates are collapsed return true (as by general collection
   *   contract)
   */
  def add(obj: Any, allowRepeated: Boolean): Boolean = {
    add(obj.asInstanceOf[Coordinate], allowRepeated)
    true
  }

  /**
   * Adds a coordinate to the end of the list.
   *
   * @param coord
   *   The coordinates
   * @param allowRepeated
   *   if set to false, repeated coordinates are collapsed
   */
  def add(coord: Coordinate, allowRepeated: Boolean): Unit = { // don't add duplicate coordinates
    if (!allowRepeated) if (size >= 1) {
      val last = get(size - 1)
      if (last.equals2D(coord)) return
    }
    super.add(coord)
    ()
  }

  /**
   * Inserts the specified coordinate at the specified position in this list.
   *
   * @param i
   *   the position at which to insert
   * @param coord
   *   the coordinate to insert
   * @param allowRepeated
   *   if set to false, repeated coordinates are collapsed
   */
  def add(i: Int, coord: Coordinate, allowRepeated: Boolean): Unit = {
    if (!allowRepeated) {
      val size = this.size()
      if (size > 0) {
        if (i > 0) {
          val prev = get(i - 1)
          if (prev.equals2D(coord)) return
        }
        if (i < size) {
          val next = get(i)
          if (next.equals2D(coord)) return
        }
      }
    }
    super.add(i, coord)
  }

  /**
   * Add an array of coordinates
   *
   * @param coll
   *   The coordinates
   * @param allowRepeated
   *   if set to false, repeated coordinates are collapsed return true (as by general collection
   *   contract)
   */
  def addAll(coll: java.util.Collection[_ <: Coordinate], allowRepeated: Boolean): Boolean = {
    var isChanged = false
    val i         = coll.iterator
    while (i.hasNext) {
      add(i.next, allowRepeated)
      isChanged = true
    }
    isChanged
  }

  /**
   * Ensure this coordList is a ring, by adding the start point if necessary
   */
  def closeRing(): Unit = if (size > 0) {
    val duplicate = get(0).copy
    add(duplicate, false)
  }

  /**
   * Returns the Coordinates in this collection.
   *
   * return the coordinates
   */
  def toCoordinateArray: Array[Coordinate] = toArray(CoordinateList.coordArrayType)

  /**
   * Creates an array containing the coordinates in this list, oriented in the given direction
   * (forward or reverse).
   *
   * @param direction
   *   the direction value: true for forward, false for reverse return an oriented array of
   *   coordinates
   */
  def toCoordinateArray(isForward: Boolean): Array[Coordinate] =
    if (isForward) toArray(CoordinateList.coordArrayType)
    else {
      val pts = new Array[Coordinate](this.size())
      for {
        i <- 0 until this.size()
      } yield {
        pts(i) = get(this.size() - i - 1)
        clone.add(i, this.get(i).clone)
      }
      pts
    }

  /**
   * Returns a deep copy of this <tt>CoordinateList</tt> instance.
   *
   * return a clone of this <tt>CoordinateList</tt> instance
   */
  override def clone: CoordinateList = {
    val clone = super.clone.asInstanceOf[CoordinateList]
    for {
      i <- 0 until this.size()
    } yield clone.add(i, this.get(i).clone)
    clone
  }
}
