// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2016 Vivid Solutions.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v20.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.geom.impl

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateSequence
import org.locationtech.jts.geom.CoordinateSequenceFactory
import org.locationtech.jts.geom.Coordinates

import java.io.Serializable

/**
 * Builds packed array coordinate sequences. The array data type can be either <code>double</code>
 * or <code>float</code>, and defaults to <code>double</code>.
 */
@SerialVersionUID(-3558264771905224525L)
object PackedCoordinateSequenceFactory {

  /**
   * Type code for arrays of type <code>double</code>.
   */
  val DOUBLE = 0

  /**
   * Type code for arrays of type <code>float</code>.
   */
  val FLOAT = 1

  /**
   * A factory using array type {@link # DOUBLE}
   */
  val DOUBLE_FACTORY = new PackedCoordinateSequenceFactory(DOUBLE)

  /**
   * A factory using array type {@link # FLOAT}
   */
  val FLOAT_FACTORY = new PackedCoordinateSequenceFactory(FLOAT)

  private val DEFAULT_MEASURES  = 0
  private val DEFAULT_DIMENSION = 3
}

@SerialVersionUID(-3558264771905224525L)
class PackedCoordinateSequenceFactory(val `type`: Int)

/**
 * Creates a new PackedCoordinateSequenceFactory of the given type. Acceptable type values are
 * {@linkplain PackedCoordinateSequenceFactory# FLOAT}or {@linkplain
 * PackedCoordinateSequenceFactory# DOUBLE}
 */
    extends CoordinateSequenceFactory
    with Serializable {

  /**
   * Creates a new PackedCoordinateSequenceFactory of type DOUBLE.
   */
  def this() =
    this(PackedCoordinateSequenceFactory.DOUBLE)

  /**
   * Gets the type of packed coordinate sequence this factory builds, either {@linkplain
   * PackedCoordinateSequenceFactory# FLOAT} or {@linkplain PackedCoordinateSequenceFactory# DOUBLE}
   *
   * @return
   *   the type of packed array built
   */
  def getType: Int = `type`

  /**
   * @see
   *   CoordinateSequenceFactory#create(Coordinate[])
   */
  override def create(coordinates: Array[Coordinate]): CoordinateSequence = {
    var dimension = PackedCoordinateSequenceFactory.DEFAULT_DIMENSION
    var measures  = PackedCoordinateSequenceFactory.DEFAULT_MEASURES
    if (coordinates != null && coordinates.length > 0 && coordinates(0) != null) {
      val first = coordinates(0)
      dimension = Coordinates.dimension(first)
      measures = Coordinates.measures(first)
    }
    if (`type` == PackedCoordinateSequenceFactory.DOUBLE)
      new PackedCoordinateSequence.Double(coordinates, dimension, measures)
    else new PackedCoordinateSequence.Float(coordinates, dimension, measures)
  }

  /**
   * @see
   *   CoordinateSequenceFactory#create(CoordinateSequence)
   */
  override def create(coordSeq: CoordinateSequence): CoordinateSequence = {
    val dimension = coordSeq.getDimension
    val measures  = coordSeq.getMeasures
    if (`type` == PackedCoordinateSequenceFactory.DOUBLE)
      new PackedCoordinateSequence.Double(coordSeq.toCoordinateArray, dimension, measures)
    else new PackedCoordinateSequence.Float(coordSeq.toCoordinateArray, dimension, measures)
  }

  /**
   * Creates a packed coordinate sequence of type {@link # DOUBLE} from the provided array using the
   * given coordinate dimension and a measure count of 0.
   *
   * @param packedCoordinates
   *   the array containing coordinate values
   * @param dimension
   *   the coordinate dimension
   * @return
   *   a packed coordinate sequence of type {@link # DOUBLE}
   */
  def create(packedCoordinates: Array[Double], dimension: Int): CoordinateSequence =
    create(packedCoordinates, dimension, PackedCoordinateSequenceFactory.DEFAULT_MEASURES)

  /**
   * Creates a packed coordinate sequence of type {@link # DOUBLE} from the provided array using the
   * given coordinate dimension and measure count.
   *
   * @param packedCoordinates
   *   the array containing coordinate values
   * @param dimension
   *   the coordinate dimension
   * @param measures
   *   the coordinate measure count
   * @return
   *   a packed coordinate sequence of type {@link # DOUBLE}
   */
  def create(packedCoordinates: Array[Double], dimension: Int, measures: Int): CoordinateSequence =
    if (`type` == PackedCoordinateSequenceFactory.DOUBLE)
      new PackedCoordinateSequence.Double(packedCoordinates, dimension, measures)
    else new PackedCoordinateSequence.Float(packedCoordinates, dimension, measures)

  /**
   * Creates a packed coordinate sequence of type {@link # FLOAT} from the provided array.
   *
   * @param packedCoordinates
   *   the array containing coordinate values
   * @param dimension
   *   the coordinate dimension
   * @return
   *   a packed coordinate sequence of type {@link # FLOAT}
   */
  def create(packedCoordinates: Array[Float], dimension: Int): CoordinateSequence = create(
    packedCoordinates,
    dimension,
    Math.max(PackedCoordinateSequenceFactory.DEFAULT_MEASURES, dimension - 3)
  )

  /**
   * Creates a packed coordinate sequence of type {@link # FLOAT} from the provided array.
   *
   * @param packedCoordinates
   *   the array containing coordinate values
   * @param dimension
   *   the coordinate dimension
   * @param measures
   *   the coordinate measure count
   * @return
   *   a packed coordinate sequence of type {@link # FLOAT}
   */
  def create(packedCoordinates: Array[Float], dimension: Int, measures: Int): CoordinateSequence =
    if (`type` == PackedCoordinateSequenceFactory.DOUBLE)
      new PackedCoordinateSequence.Double(packedCoordinates, dimension, measures)
    else new PackedCoordinateSequence.Float(packedCoordinates, dimension, measures)

  /**
   * @see
   *   org.locationtech.jts.geom.CoordinateSequenceFactory#create(int, int)
   */
  override def create(size: Int, dimension: Int): CoordinateSequence = if (
    `type` == PackedCoordinateSequenceFactory.DOUBLE
  )
    new PackedCoordinateSequence.Double(
      size,
      dimension,
      Math.max(PackedCoordinateSequenceFactory.DEFAULT_MEASURES, dimension - 3)
    )
  else
    new PackedCoordinateSequence.Float(
      size,
      dimension,
      Math.max(PackedCoordinateSequenceFactory.DEFAULT_MEASURES, dimension - 3)
    )

  /**
   * @see
   *   org.locationtech.jts.geom.CoordinateSequenceFactory#create(int, int, int)
   */
  override def create(size: Int, dimension: Int, measures: Int): CoordinateSequence = if (
    `type` == PackedCoordinateSequenceFactory.DOUBLE
  ) new PackedCoordinateSequence.Double(size, dimension, measures)
  else new PackedCoordinateSequence.Float(size, dimension, measures)
}
