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
package org.locationtech.jts.geom.impl

import java.io.Serializable
import org.locationtech.jts.geom._

/**
 * A {link CoordinateSequence} backed by an array of {link Coordinate}s. This is the implementation
 * that {link Geometry}s use by default. Coordinates returned by #toArray and #getCoordinate are
 * live -- modifications to them are actually changing the CoordinateSequence's underlying data. A
 * dimension may be specified for the coordinates in the sequence, which may be 2 or 3. The actual
 * coordinates will always have 3 ordinates, but the dimension is useful as metadata in some
 * situations.
 *
 * @version 1.7
 */
@SerialVersionUID(-915438501601840650L)
class CoordinateArraySequence(
  coordinatesArg: Array[Coordinate],
  dimension:      Int = 3,
  measures:       Int = 0
) extends CoordinateSequence
    with Serializable {
  private var coordinates: Array[Coordinate] =
    if (coordinatesArg == null) new Array[Coordinate](0)
    else enforceArrayConsistency(coordinatesArg)

  /**
   * The actual dimension of the coordinates in the sequence. Allowable values are 2, 3 or 4.
   */
//    private var dimension = 3
  /**
   * The number of measures of the coordinates in the sequence. Allowable values are 0 or 1.
   */
//  private var measures = 0

  /**
   * Constructs a sequence based on the given array of {link Coordinate}s (the array is not copied).
   * The coordinate dimension defaults to 3.
   *
   * @param coordinates
   *   the coordinate array that will be referenced.
   */
  def this(coordinates: Array[Coordinate]) =
    this(coordinates,
         CoordinateArrays.dimension(coordinates),
         CoordinateArrays.measures(coordinates)
    )

  /**
   * Constructs a sequence based on the given array of {link Coordinate}s (the array is not copied).
   *
   * @param coordinates
   *   the coordinate array that will be referenced.
   * @param dimension
   *   the dimension of the coordinates
   */
  def this(coordinates: Array[Coordinate], dimension: Int) =
    this(coordinates, dimension, CoordinateArrays.measures(coordinates))

  /**
   * Constructs a sequence of a given size, populated with new {link Coordinate}s.
   *
   * @param size
   *   the size of the sequence to create
   */
  def this(size: Int) = {
    this(new Array[Coordinate](size))
    var i = 0
    while (i < size)
      coordinates(i) = new Coordinate {
        i += 1; i - 1
      }
  }

  /**
   * Constructs a sequence of a given size, populated with new {link Coordinate}s.
   *
   * @param size
   *   the size of the sequence to create
   * @param dimension
   *   the dimension of the coordinates
   */
  def this(size: Int, dimension: Int) = {
    this(coordinates = new Array[Coordinate](size), dimension = dimension)
    coordinates = Array.fill(size)(Coordinates.create(dimension))
  }

  def this(size: Int, dimension: Int, measures: Int) = {
    this(new Array[Coordinate](size), dimension = dimension, measures = measures)
    coordinates = Array.fill(size)(createCoordinate)
  }

  /**
   * Creates a new sequence based on a deep copy of the given {link CoordinateSequence}. The
   * coordinate dimension is set to equal the dimension of the input.
   *
   * @param coordSeq
   *   the coordinate sequence that will be copied.
   */
  def this(coordSeq: CoordinateSequence) = {
    this(new Array[Coordinate](coordSeq.size), coordSeq.getDimension, coordSeq.getMeasures)

    var i = 0
    while (i < coordinates.length) {
      coordinates(i) = coordSeq.getCoordinateCopy(i)

      {
        i += 1;
        i - 1
      }
    }
  }

  /**
   * Ensure array contents of the same type, making use of {link #createCoordinate()} as needed. <p>
   * A new array will be created if needed to return a consistent result. </p>
   *
   * @param array
   *   array containing consistent coordinate instances
   */
  protected def enforceArrayConsistency(array: Array[Coordinate]): Array[Coordinate] = {
    val sample       = createCoordinate
    val `type`       = sample.getClass
    val isConsistent = array.foldLeft(true) { (consistent, c) =>
      if (c != null && !(c.getClass == `type`)) false else consistent
    }
    if (isConsistent) array
    else {
      array.map { c =>
        if (c != null && !(c.getClass == `type`)) {
          val duplicate = createCoordinate
          duplicate.setCoordinate(c)
          duplicate
        } else c
      }
    }
  }

  /**
   * @see
   *   org.locationtech.jts.geom.CoordinateSequence#getDimension()
   */
  override def getDimension: Int = dimension

  override def getMeasures: Int = measures

  /**
   * Get the Coordinate with index i.
   *
   * @param i
   *   the index of the coordinate return the requested Coordinate instance
   */
  override def getCoordinate(i: Int): Coordinate = coordinates(i)

  /**
   * Get a copy of the Coordinate with index i.
   *
   * @param i
   *   the index of the coordinate return a copy of the requested Coordinate
   */
  override def getCoordinateCopy(i: Int): Coordinate = {
    val copy = createCoordinate
    copy.setCoordinate(coordinates(i))
    copy
  }

  /**
   * @see
   *   org.locationtech.jts.geom.CoordinateSequence#getX(int)
   */
  override def getCoordinate(index: Int, coord: Coordinate): Unit =
    coord.setCoordinate(coordinates(index))

  override def getX(index: Int): Double = coordinates(index).x

  /**
   * @see
   *   org.locationtech.jts.geom.CoordinateSequence#getY(int)
   */
  override def getY(index: Int): Double = coordinates(index).y

  /**
   * @see
   *   org.locationtech.jts.geom.CoordinateSequence#getZ(int)
   */
  override def getZ(index: Int): Double = if (hasZ) coordinates(index).getZ
  else Double.NaN

  /**
   * @see
   *   org.locationtech.jts.geom.CoordinateSequence#getM(int)
   */
  override def getM(index: Int): Double = if (hasM) coordinates(index).getM
  else Double.NaN

  /**
   * @see
   *   org.locationtech.jts.geom.CoordinateSequence#getOrdinate(int, int)
   */
  override def getOrdinate(index: Int, ordinateIndex: Int): Double = ordinateIndex match {
    case CoordinateSequence.X =>
      coordinates(index).x
    case CoordinateSequence.Y =>
      coordinates(index).y
    case _                    =>
      coordinates(index).getOrdinate(ordinateIndex)
  }

  /**
   * Creates a deep copy of the Object
   *
   * return The deep copy
   * @deprecated
   */
  override def clone: Object = copy

  /**
   * Creates a deep copy of the CoordinateArraySequence
   *
   * return The deep copy
   */
  override def copy: CoordinateArraySequence = {
    val cloneCoordinates = new Array[Coordinate](size)
    coordinates.zipWithIndex.foreach { case (c, i) =>
      val duplicate = createCoordinate
      duplicate.setCoordinate(c)
      cloneCoordinates(i) = duplicate
    }
    new CoordinateArraySequence(cloneCoordinates, dimension, measures)
  }

  /**
   * Returns the size of the coordinate sequence
   *
   * return the number of coordinates
   */
  override def size: Int = coordinates.length

  /**
   * @see
   *   org.locationtech.jts.geom.CoordinateSequence#setOrdinate(int, int, double)
   */
  override def setOrdinate(index: Int, ordinateIndex: Int, value: Double): Unit =
    ordinateIndex match {
      case CoordinateSequence.X =>
        coordinates(index).x = value
      case CoordinateSequence.Y =>
        coordinates(index).y = value
      case _                    =>
        coordinates(index).setOrdinate(ordinateIndex, value)
    }

  /**
   * This method exposes the internal Array of Coordinate Objects
   *
   * return the Coordinate[] array.
   */
  override def toCoordinateArray: Array[Coordinate] = coordinates

  override def expandEnvelope(env: Envelope): Envelope = {
    coordinates.foreach(env.expandToInclude)
    env
  }

  /**
   * Returns the string Representation of the coordinate array
   *
   * return The string
   */
  override def toString: String = if (coordinates.length > 0) {
    val strBuilder = new StringBuilder(17 * coordinates.length)
    strBuilder.append('(')
    strBuilder.append(coordinates(0))
    var i          = 1
    while (i < coordinates.length) {
      strBuilder.append(", ")
      strBuilder.append(coordinates(i)) {
        i += 1; i - 1
      }
    }
    strBuilder.append(')')
    strBuilder.toString
  } else "()"
}
