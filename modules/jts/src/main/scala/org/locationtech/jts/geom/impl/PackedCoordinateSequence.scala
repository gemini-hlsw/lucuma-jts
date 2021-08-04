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
 */ /*
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

import java.io.ObjectStreamException
import java.io.Serializable
import java.lang.ref.SoftReference
import java.util
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateSequence
import org.locationtech.jts.geom.CoordinateSequences
import org.locationtech.jts.geom.CoordinateXY
import org.locationtech.jts.geom.CoordinateXYM
import org.locationtech.jts.geom.CoordinateXYZM
import org.locationtech.jts.geom.Envelope

/**
 * A {link CoordinateSequence} implementation based on a packed arrays.
 * In this implementation, {link Coordinate}s returned by #toArray and #get are copies
 * of the internal values.
 * To change the actual values, use the provided setters.
 * <p>
 * For efficiency, created Coordinate arrays
 * are cached using a soft reference.
 * The cache is cleared each time the coordinate sequence contents are
 * modified through a setter method.
 *
 * @version 1.7
 */
@SerialVersionUID(-3151899011275603L)
object PackedCoordinateSequence {

  /**
   * Packed coordinate sequence implementation based on doubles
   */
  @SerialVersionUID(5777450686367912719L)
  class Double(private val coords: Array[scala.Double], dim: Int, meas: Int)
      extends PackedCoordinateSequence(dim, meas) {

    /**
     * The packed coordinate array
     */
//      private[impl] var coords = null

    /**
     * Builds a new packed coordinate sequence
     *
     * @param coords    an array of <c>double</c> values that contains the ordinate values of the sequence
     * @param dimension the total number of ordinates that make up a { @link Coordinate} in this sequence.
     * @param measures the number of measure-ordinates each { @link Coordinate} in this sequence has.
     */
    if (coords.length % dimension != 0)
      throw new IllegalArgumentException(
        "Packed array does not contain " + "an integral number of coordinates"
      )

    /**
     * Builds a new packed coordinate sequence out of a float coordinate array
     *
     * @param coords    an array of <c>float</c> values that contains the ordinate values of the sequence
     * @param dimension the total number of ordinates that make up a { @link Coordinate} in this sequence.
     * @param measures the number of measure-ordinates each { @link Coordinate} in this sequence has.
     */
    def this(coords: Array[scala.Float], dimension: Int, measures: Int) = {
      this(coords.map(_.toDouble), dimension, measures)
    }

    /**
     * Builds a new packed coordinate sequence out of a coordinate array
     *
     * @param coordinates an array of { @link Coordinate}s
     * @param dimension the total number of ordinates that make up a { @link Coordinate} in this sequence.
     * @param measures the number of measure-ordinates each { @link Coordinate} in this sequence has.
     */
    def this(coordinates: Array[Coordinate], dimension: Int, measures: Int) = {
      this(toDoubleArray(coordinates, dimension), dimension, measures)
    }

    /**
     * Builds a new packed coordinate sequence out of a coordinate array
     *
     * @param coordinates an array of { @link Coordinate}s
     * @param dimension the total number of ordinates that make up a { @link Coordinate} in this sequence.
     */
    def this(coordinates: Array[Coordinate], dimension: Int) = {
      this(coordinates, dimension, 0)
    }

    /**
     * Builds a new packed coordinate sequence out of a coordinate array
     *
     * @param coordinates an array of { @link Coordinate}s
     */
    def this(coordinates: Array[Coordinate]) = {
      this(coordinates, 3, 0)
    }

    /**
     * Builds a new empty packed coordinate sequence of a given size and dimension
     *
     * @param size      the number of coordinates in this sequence
     * @param dimension the total number of ordinates that make up a { @link Coordinate} in this sequence.
     * @param measures the number of measure-ordinates each { @link Coordinate} in this sequence has.
     */
    def this(size: Int, dimension: Int, measures: Int) = {
      this(new Array[scala.Double](size * dimension), dimension, measures)
    }

    /**
     * @see PackedCoordinateSequence#getCoordinate(int)
     */
    override def getCoordinateInternal(i: Int): Coordinate = {
      val x = coords(i * dimension)
      val y = coords(i * dimension + 1)
      if (dimension == 2 && measures == 0) return new CoordinateXY(x, y)
      else if (dimension == 3 && measures == 0) {
        val z = coords(i * dimension + 2)
        return new Coordinate(x, y, z)
      } else if (dimension == 3 && measures == 1) {
        val m = coords(i * dimension + 2)
        return new CoordinateXYM(x, y, m)
      } else if (dimension == 4 && measures == 1) {
        val z = coords(i * dimension + 2)
        val m = coords(i * dimension + 3)
        return new CoordinateXYZM(x, y, z, m)
      }
      new Coordinate(x, y)
    }

    /**
     * Gets the underlying array containing the coordinate values.
     *
     * return the array of coordinate values
     */
    def getRawCoordinates: Array[scala.Double] = coords

    /**
     * @see CoordinateSequence#size()
     */
    override def size: Int = coords.length / dimension

    /**
     * @see java.lang.Object#clone()
     * @see PackedCoordinateSequence#clone()
     * @deprecated
     */
    override def clone: Object = copy

    /**
     * @see PackedCoordinateSequence#size()
     */
    override def copy: CoordinateSequence = {
      val clone = util.Arrays.copyOf(coords, coords.length)
      new PackedCoordinateSequence.Double(clone, dimension, measures)
    }

    /**
     * @see PackedCoordinateSequence#getOrdinate(int, int)
     *      Beware, for performance reasons the ordinate index is not checked, if
     *      it's over dimensions you may not get an exception but a meaningless
     *      value.
     */
    override def getOrdinate(index: Int, ordinate: Int): scala.Double = coords(
      index * dimension + ordinate
    )

    /**
     * @see PackedCoordinateSequence#setOrdinate(int, int, double)
     */
    override def setOrdinate(index: Int, ordinate: Int, value: scala.Double): Unit = {
      coordRef = null
      coords(index * dimension + ordinate) = value
    }

    /**
     * @see CoordinateSequence#expandEnvelope(Envelope)
     */
    override def expandEnvelope(env: Envelope): Envelope = {
      var i = 0
      while (i < coords.length) {
        env.expandToInclude(coords(i), coords(i + 1))
        i += dimension
      }
      env
    }
  }

  def toFloatArray(coordRef: Array[Coordinate], dimension: Int): Array[scala.Float] = {
    val coordinates = if (coordRef == null) new Array[Coordinate](0) else coordRef
    val coords      = new Array[scala.Float](coordinates.length * dimension)
    coordinates.zipWithIndex.foreach { case (c, i) =>
      val offset = i * dimension
      coords(offset) = c.x.toFloat
      coords(offset + 1) = c.y.toFloat
      if (dimension >= 3) coords(offset + 2) = coordinates(i).getOrdinate(2).toFloat
      if (dimension >= 4) coords(offset + 3) = coordinates(i).getOrdinate(3).toFloat
    }
    coords
  }

  def toDoubleArray(coordRef: Array[Coordinate], dimension: Int): Array[scala.Double] = {
    val coordinates = if (coordRef == null) new Array[Coordinate](0) else coordRef
    val coords      = new Array[scala.Double](coordinates.length * dimension)
    coordinates.zipWithIndex.foreach { case (c, i) =>
      val offset = i * dimension
      coords(offset) = c.x
      coords(offset + 1) = c.y
      if (dimension >= 3) coords(offset + 2) = coordinates(i).getOrdinate(2)
      if (dimension >= 4) coords(offset + 3) = coordinates(i).getOrdinate(3)
    }
    coords
  }

  /**
   * Packed coordinate sequence implementation based on floats
   */
  @SerialVersionUID(-2902252401427938986L)
  class Float(private val coords: Array[scala.Float], dim: Int, meas: Int)
      extends PackedCoordinateSequence(dim, meas) {
//    private[impl] var coords = null

    /**
     * Constructs a packed coordinate sequence from an array of <code>float</code>s
     *
     * @param coords    an array of <c>float</c> values that contains the ordinate values of the sequence
     * @param dimension the total number of ordinates that make up a { @link Coordinate} in this sequence.
     * @param measures the number of measure-ordinates each { @link Coordinate} in this sequence has.
     */

    if (coords.length % dimension != 0)
      throw new IllegalArgumentException(
        "Packed array does not contain " + "an integral number of coordinates"
      )

    /**
     * Constructs a packed coordinate sequence from an array of <code>double</code>s
     *
     * @param coords    an array of <c>double</c> values that contains the ordinate values of the sequence
     * @param dimension the total number of ordinates that make up a { @link Coordinate} in this sequence.
     * @param measures the number of measure-ordinates each { @link Coordinate} in this sequence has.
     */
    def this(coords: Array[scala.Double], dimension: Int, measures: Int) = {
      this(coords.map(_.toFloat), dimension, measures)
    }

    /**
     * Constructs a packed coordinate sequence out of a coordinate array
     *
     * @param coordinates an array of { @link Coordinate}s
     * @param dimension the total number of ordinates that make up a { @link Coordinate} in this sequence.
     * @param measures the number of measure-ordinates each { @link Coordinate} in this sequence has.
     */
    def this(coordinates: Array[Coordinate], dimension: Int, measures: Int) = {
      this(toFloatArray(coordinates, dimension), dimension, measures)
    }

    /**
     * Builds a new packed coordinate sequence out of a coordinate array
     *
     * @param coordinates an array of { @link Coordinate}s
     * @param dimension the total number of ordinates that make up a { @link Coordinate} in this sequence.
     */
    def this(coordinates: Array[Coordinate], dimension: Int) = {
      this(coordinates, dimension, 0)
    }

    /**
     * Constructs an empty packed coordinate sequence of a given size and dimension
     *
     * @param size      the number of coordinates in this sequence
     * @param dimension the total number of ordinates that make up a { @link Coordinate} in this sequence.
     * @param measures the number of measure-ordinates each { @link Coordinate} in this sequence has.
     */
    def this(size: Int, dimension: Int, measures: Int) = {
      this(new Array[scala.Float](size * dimension), dimension, measures)
    }

    override def getCoordinateInternal(i: Int): Coordinate = {
      val x = coords(i * dimension)
      val y = coords(i * dimension + 1)
      if (dimension == 2 && measures == 0) return new CoordinateXY(x.toDouble, y.toDouble)
      else if (dimension == 3 && measures == 0) {
        val z = coords(i * dimension + 2)
        return new Coordinate(x.toDouble, y.toDouble, z.toDouble)
      } else if (dimension == 3 && measures == 1) {
        val m = coords(i * dimension + 2)
        return new CoordinateXYM(x.toDouble, y.toDouble, m.toDouble)
      } else if (dimension == 4 && measures == 1) {
        val z = coords(i * dimension + 2)
        val m = coords(i * dimension + 3)
        return new CoordinateXYZM(x.toDouble, y.toDouble, z.toDouble, m.toDouble)
      }
      new Coordinate(x.toDouble, y.toDouble)
    }

    def getRawCoordinates: Array[scala.Float] = coords

    override def size: Int = coords.length / dimension

    override def clone: Float = copy

    /**
     * @see PackedCoordinateSequence#copy()
     */
    override def copy: Float = {
      val clone = util.Arrays.copyOf(coords, coords.length)
      new PackedCoordinateSequence.Float(clone, dimension, measures)
    }

    /**
     * @see PackedCoordinateSequence#getOrdinate(int, int)
     *      For performance reasons the ordinate index is not checked.
     *      If it is larger than the dimension a meaningless
     *      value may be returned.
     */
    override def getOrdinate(index: Int, ordinate: Int): scala.Double = coords(
      index * dimension + ordinate
    ).toDouble

    override def setOrdinate(index: Int, ordinate: Int, value: scala.Double): Unit = {
      coordRef = null
      coords(index * dimension + ordinate) = value.toFloat
    }

    override def expandEnvelope(env: Envelope): Envelope = {
      var i = 0
      while (i < coords.length) {
        env.expandToInclude(coords(i).toDouble, coords(i + 1).toDouble)
        i += dimension
      }
      env
    }
  }

}

@SerialVersionUID(-3151899011275603L)
abstract class PackedCoordinateSequence protected (
  /**
   * The dimensions of the coordinates held in the packed array
   */
  var dimension: Int,
  /**
   * The number of measures of the coordinates held in the packed array.
   */
  var measures:  Int
)

/**
 * Creates an instance of this class
 *
 * @param dimension the total number of ordinates that make up a { @link Coordinate} in this sequence.
 * @param measures the number of measure-ordinates each { @link Coordinate} in this sequence has.
 */
    extends CoordinateSequence
    with Serializable {
  if (dimension - measures < 2)
    throw new IllegalArgumentException("Must have at least 2 spatial dimensions")

  /**
   * A soft reference to the Coordinate[] representation of this sequence.
   * Makes repeated coordinate array accesses more efficient.
   */
  protected var coordRef: SoftReference[Array[Coordinate]] = null

  /**
   * @see CoordinateSequence#getDimension()
   */
  override def getDimension: Int = this.dimension

  /**
   * @see CoordinateSequence#getMeasures()
   */
  override def getMeasures: Int = this.measures

  /**
   * @see CoordinateSequence#getCoordinate(int)
   */
  override def getCoordinate(i: Int): Coordinate = {
    val coords = getCachedCoords
    if (coords != null) coords(i)
    else getCoordinateInternal(i)
  }

  override def getCoordinateCopy(i: Int): Coordinate = getCoordinateInternal(i)

  override def getCoordinate(i: Int, coord: Coordinate): Unit = {
    coord.x = getOrdinate(i, 0)
    coord.y = getOrdinate(i, 1)
    if (hasZ) coord.setZ(getZ(i))
    if (hasM) coord.setM(getM(i))
  }

  /**
   * @see CoordinateSequence#toCoordinateArray()
   */
  override def toCoordinateArray: Array[Coordinate] = {
    var coords = getCachedCoords
    // testing - never cache
    if (coords != null) return coords
    coords = new Array[Coordinate](size)
    var i      = 0
    while (i < coords.length) {
      coords(i) = getCoordinateInternal(i)
      i += 1
    }
    coordRef = new SoftReference[Array[Coordinate]](coords)
    coords
  }

  private def getCachedCoords: Array[Coordinate] = if (coordRef != null) {
    val coords = coordRef.get
    if (coords != null) coords
    else { // System.out.print("-");
      coordRef = null
      null
    }
  } else null

  /**
   * @see CoordinateSequence#getX(int)
   */
  override def getX(index: Int): scala.Double = getOrdinate(index, 0)

  /**
   * @see CoordinateSequence#getY(int)
   */
  override def getY(index: Int): scala.Double = getOrdinate(index, 1)

  /**
   * @see CoordinateSequence#getOrdinate(int, int)
   */
  override def getOrdinate(index: Int, ordinateIndex: Int): scala.Double

  /**
   * Sets the first ordinate of a coordinate in this sequence.
   *
   * @param index the coordinate index
   * @param value the new ordinate value
   */
  def setX(index: Int, value: Double): Unit = {
    coordRef = null
    setOrdinate(index, 0, value)
  }

  /**
   * Sets the second ordinate of a coordinate in this sequence.
   *
   * @param index the coordinate index
   * @param value the new ordinate value
   */
  def setY(index: Int, value: Double): Unit = {
    coordRef = null
    setOrdinate(index, 1, value)
  }

  override def toString: String = CoordinateSequences.toString(this)

  @throws[ObjectStreamException]
  protected def readResolve: PackedCoordinateSequence = {
    coordRef = null
    this
  }

  /**
   * Returns a Coordinate representation of the specified coordinate, by always
   * building a new Coordinate object
   *
   * @param index the coordinate index
   * return the { @link Coordinate} at the given index
   */
  protected def getCoordinateInternal(index: Int): Coordinate

  /**
   * @see java.lang.Object#clone()
   * @see CoordinateSequence#clone()
   * @deprecated
   */
  override protected def clone: Object

  override def copy: CoordinateSequence

  /**
   * Sets the ordinate of a coordinate in this sequence.
   * <br>
   * Warning: for performance reasons the ordinate index is not checked
   * - if it is over dimensions you may not get an exception but a meaningless value.
   *
   * @param index
   * the coordinate index
   * @param ordinate
   * the ordinate index in the coordinate, 0 based, smaller than the
   * number of dimensions
   * @param value
   * the new ordinate value
   */
  override def setOrdinate(index: Int, ordinate: Int, value: Double): Unit
}
