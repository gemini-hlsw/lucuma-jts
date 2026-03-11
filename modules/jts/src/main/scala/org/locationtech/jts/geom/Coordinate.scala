// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2018 Vivid Solutions
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */ /*
 * Copyright (c) 2018 Vivid Solutions
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

import org.locationtech.jts.util.Assert
import org.locationtech.jts.util.NumberUtil

import java.io.Serializable
import java.util.Comparator
import scala.annotation.nowarn

/**
 * A lightweight class used to store coordinates on the 2-dimensional Cartesian plane. <p> It is
 * distinct from {link Point}, which is a subclass of {link Geometry}. Unlike objects of type {link
 * Point} (which contain additional information such as an envelope, a precision model, and spatial
 * reference system information), a <code>Coordinate</code> only contains ordinate values and
 * accessor methods. </p> <p> <code>Coordinate</code>s are two-dimensional points, with an
 * additional Z-ordinate. If an Z-ordinate value is not specified or not defined, constructed
 * coordinates have a Z-ordinate of <code>NaN</code> (which is also the value of
 * <code>NULL_ORDINATE</code>). The standard comparison functions ignore the Z-ordinate. Apart from
 * the basic accessor functions, JTS supports only specific operations involving the Z-ordinate.</p>
 * <p> Implementations may optionally support Z-ordinate and M-measure values as appropriate for a
 * {link CoordinateSequence}. Use of {link #getZ()} and {link #getM()} accessors, or {link
 * #getOrdinate(int)} are recommended.</p>
 *
 * @version 1.16
 */
@SerialVersionUID(6683108902428366910L)
object Coordinate {

  /**
   * The value used to indicate a null or missing ordinate value. In particular, used for the value
   * of ordinates for dimensions greater than the defined dimension of a coordinate.
   */
  val NULL_ORDINATE: Double = Double.NaN

  /** Standard ordinate index value for, where X is 0 */
  val X = 0

  /** Standard ordinate index value for, where Y is 1 */
  val Y = 1

  /**
   * Standard ordinate index value for, where Z is 2.
   *
   * <p>This constant assumes XYZM coordinate sequence definition, please check this assumption
   * using {link #getDimension()} and {link #getMeasures()} before use.
   */
  val Z = 2

  /**
   * Standard ordinate index value for, where M is 3.
   *
   * <p>This constant assumes XYZM coordinate sequence definition, please check this assumption
   * using {link #getDimension()} and {link #getMeasures()} before use.
   */
  val M = 3

  /**
   * Computes a hash code for a double value, using the algorithm from Joshua Bloch's book
   * <i>Effective Java"</i>
   *
   * @param x
   *   the value to compute for return a hashcode for x
   */
  def hashCode(x: Double): Int = {
    val f = java.lang.Double.doubleToLongBits(x)
    (f ^ (f >>> 32)).toInt
  }

  /**
   * Compares two {link Coordinate}s, allowing for either a 2-dimensional or 3-dimensional
   * comparison, and handling NaN values correctly.
   */
  object DimensionalComparator {

    /**
     * Compare two <code>double</code>s, allowing for NaN values. NaN is treated as being less than
     * any valid number.
     *
     * @param a
     *   a <code>double</code>
     * @param b
     *   a <code>double</code> return -1, 0, or 1 depending on whether a is less than, equal to or
     *   greater than b
     */
    def compare(a: Double, b: Double): Int = {
      if (a < b) return -1
      if (a > b) return 1
      if (java.lang.Double.isNaN(a)) {
        if (java.lang.Double.isNaN(b)) return 0
        return -1
      }
      if (java.lang.Double.isNaN(b)) return 1
      0
    }
  }

  class DimensionalComparator(val dimensionsToTest: Int) extends Comparator[Coordinate] {
    if (dimensionsToTest != 2 && dimensionsToTest != 3)
      throw new IllegalArgumentException("only 2 or 3 dimensions may be specified")

    /**
     * Creates a comparator for 2 dimensional coordinates.
     */
    def this() =
      this(2)

    /**
     * Compares two {link Coordinate}s along to the number of dimensions specified.
     *
     * @param o1
     *   a {@link Coordinate}
     * @param o2
     *   a {link Coordinate} return -1, 0, or 1 depending on whether o1 is less than, equal to, or
     *   greater than 02
     */
    override def compare(c1: Coordinate, c2: Coordinate): Int = {
      val compX = DimensionalComparator.compare(c1.x, c2.x)
      if (compX != 0) return compX
      val compY = DimensionalComparator.compare(c1.y, c2.y)
      if (compY != 0) return compY
      if (dimensionsToTest <= 2) return 0
      val compZ = DimensionalComparator.compare(c1.getZ, c2.getZ)
      compZ
    }
  }

}

@SerialVersionUID(6683108902428366910L)
class Coordinate(var x: Double, var y: Double, var z: Double)
    extends Comparable[Coordinate]
    with Cloneable
    with Serializable {

  /**
   * Constructs a <code>Coordinate</code> having the same (x,y,z) values as <code>other</code>.
   *
   * @param c
   *   the <code>Coordinate</code> to copy.
   */
  def this(c: Coordinate) =
    this(c.x, c.y, c.getZ)

  /**
   * Constructs a <code>Coordinate</code> at (x,y,NaN).
   *
   * @param x
   *   the x-value
   * @param y
   *   the y-value
   */
  def this(x: Double, y: Double) =
    this(x, y, Coordinate.NULL_ORDINATE)

  /**
   * Constructs a <code>Coordinate</code> at (0,0,NaN).
   */
  def this() =
    this(0.0, 0.0)

  /**
   * Sets this <code>Coordinate</code>s (x,y,z) values to that of <code>other</code>.
   *
   * @param other
   *   the <code>Coordinate</code> to copy
   */
  def setCoordinate(other: Coordinate): Unit = {
    x = other.x
    y = other.y
    z = other.getZ
  }

  /**
   * Retrieves the value of the X ordinate.
   *
   * return the value of the X ordinate
   */
  def getX: Double = x

  /**
   * Sets the X ordinate value.
   *
   * @param x
   *   the value to set as X
   */
  def setX(x: Double): Unit = this.x = x

  /**
   * Retrieves the value of the Y ordinate.
   *
   * return the value of the Y ordinate
   */
  def getY: Double = y

  /**
   * Sets the Y ordinate value.
   *
   * @param y
   *   the value to set as Y
   */
  def setY(y: Double): Unit = this.y = y

  /**
   * Retrieves the value of the Z ordinate, if present. If no Z value is present returns
   * <tt>NaN</tt>.
   *
   * return the value of the Z ordinate, or <tt>NaN</tt>
   */
  def getZ: Double = z

  /**
   * Sets the Z ordinate value.
   *
   * @param z
   *   the value to set as Z
   */
  def setZ(z: Double): Unit = this.z = z

  /**
   * Retrieves the value of the measure, if present. If no measure value is present returns
   * <tt>NaN</tt>.
   *
   * return the value of the measure, or <tt>NaN</tt>
   */
  def getM: Double = Double.NaN

  /**
   * Sets the measure value, if supported.
   *
   * @param m
   *   the value to set as M
   */
  def setM(m: Double): Unit = throw new IllegalArgumentException(
    "Invalid ordinate index: " + Coordinate.M
  )

  /**
   * Gets the ordinate value for the given index.
   *
   * The base implementation supports values for the index are {link X}, {link Y}, and {link Z}.
   *
   * @param ordinateIndex
   *   the ordinate index return the value of the ordinate throws IllegalArgumentException if the
   *   index is not valid
   */
  @nowarn
  def getOrdinate(ordinateIndex: Int): Double =
    ordinateIndex match {
      case Coordinate.X => x
      case Coordinate.Y => y
      case Coordinate.Z =>
        getZ // sure to delegate to subclass rather than offer direct field access
    }

  /**
   * Sets the ordinate for the given index to a given value.
   *
   * The base implementation supported values for the index are {link X}, {link Y}, and {link Z}.
   *
   * @param ordinateIndex
   *   the ordinate index
   * @param value
   *   the value to set throws IllegalArgumentException if the index is not valid
   */
  def setOrdinate(ordinateIndex: Int, value: Double): Unit = ordinateIndex match {
    case Coordinate.X =>
      x = value
    case Coordinate.Y =>
      y = value
    case Coordinate.Z =>
      setZ(value) // delegate to subclass rather than offer direct field access
    case _            =>
      throw new IllegalArgumentException("Invalid ordinate index: " + ordinateIndex)
  }

  /**
   * Returns whether the planar projections of the two <code>Coordinate</code>s are equal.
   *
   * @param other
   *   a <code>Coordinate</code> with which to do the 2D comparison. return <code>true</code> if the
   *   x- and y-coordinates are equal; the z-coordinates do not have to be equal.
   */
  def equals2D(other: Coordinate): Boolean = {
    if (x != other.x) return false
    if (y != other.y) return false
    true
  }

  /**
   * Tests if another Coordinate has the same values for the X and Y ordinates, within a specified
   * tolerance value. The Z ordinate is ignored.
   *
   * @param c
   *   a <code>Coordinate</code> with which to do the 2D comparison.
   * @param tolerance
   *   the tolerance value to use return true if <code>other</code> is a <code>Coordinate</code>
   *   with the same values for X and Y.
   */
  def equals2D(c: Coordinate, tolerance: Double): Boolean = {
    if (!NumberUtil.equalsWithTolerance(this.x, c.x, tolerance)) return false
    if (!NumberUtil.equalsWithTolerance(this.y, c.y, tolerance)) return false
    true
  }

  /**
   * Tests if another coordinate has the same values for the X, Y and Z ordinates.
   *
   * @param other
   *   a <code>Coordinate</code> with which to do the 3D comparison. return true if
   *   <code>other</code> is a <code>Coordinate</code> with the same values for X, Y and Z.
   */
  def equals3D(other: Coordinate): Boolean =
    (x == other.x) && (y == other.y) && ((getZ == other.getZ) || (java.lang.Double.isNaN(
      getZ
    ) && java.lang.Double.isNaN(other.getZ)))

  /**
   * Tests if another coordinate has the same value for Z, within a tolerance.
   *
   * @param c
   *   a coordinate
   * @param tolerance
   *   the tolerance value return true if the Z ordinates are within the given tolerance
   */
  def equalInZ(c: Coordinate, tolerance: Double): Boolean =
    NumberUtil.equalsWithTolerance(this.getZ, c.getZ, tolerance)

  /**
   * Returns <code>true</code> if <code>other</code> has the same values for the x and y ordinates.
   * Since Coordinates are 2.5D, this routine ignores the z value when making the comparison.
   *
   * @param other
   *   a <code>Coordinate</code> with which to do the comparison. return <code>true</code> if
   *   <code>other</code> is a <code>Coordinate</code> with the same values for the x and y
   *   ordinates.
   */
  override def equals(other: Any): Boolean = {
    if (!other.isInstanceOf[Coordinate]) return false
    equals2D(other.asInstanceOf[Coordinate])
  }

  /**
   * Compares this {link Coordinate} with the specified {link Coordinate} for order. This method
   * ignores the z value when making the comparison. Returns: <UL> <LI> -1 : this.x &lt; other.x ||
   * ((this.x == other.x) &amp;&amp; (this.y &lt; other.y)) <LI> 0 : this.x == other.x &amp;&amp;
   * this.y = other.y <LI> 1 : this.x &gt; other.x || ((this.x == other.x) &amp;&amp; (this.y &gt;
   * other.y))
   *
   * </UL> Note: This method assumes that ordinate values are valid numbers. NaN values are not
   * handled correctly.
   *
   * @param o
   *   the <code>Coordinate</code> with which this <code>Coordinate</code> is being compared return
   *   -1, zero, or 1 as this <code>Coordinate</code> is less than, equal to, or greater than the
   *   specified <code>Coordinate</code>
   */
  override def compareTo(o: Coordinate): Int = {
    val other = o.asInstanceOf[Coordinate]
    if (x < other.x) return -1
    if (x > other.x) return 1
    if (y < other.y) return -1
    if (y > other.y) return 1
    0
  }

  /**
   * Returns a <code>String</code> of the form <I>(x,y,z)</I> .
   *
   * return a <code>String</code> of the form <I>(x,y,z)</I>
   */
  override def toString: String = "(" + x + ", " + y + ", " + getZ + ")"

  override def clone: Coordinate = try {
    val coord = super.clone.asInstanceOf[Coordinate]
    coord // return the clone
  } catch {
    case _: CloneNotSupportedException =>
      Assert.shouldNeverReachHere("this shouldn't happen because this class is Cloneable")
      null
  }

  /**
   * Creates a copy of this Coordinate.
   *
   * return a copy of this coordinate.
   */
  def copy = new Coordinate(this)

  /**
   * Computes the 2-dimensional Euclidean distance to another location. The Z-ordinate is ignored.
   *
   * @param c
   *   a point return the 2-dimensional Euclidean distance between the locations
   */
  def distance(c: Coordinate): Double = {
    val dx = x - c.x
    val dy = y - c.y
    Math.sqrt(dx * dx + dy * dy)
  }

  /**
   * Computes the 3-dimensional Euclidean distance to another location.
   *
   * @param c
   *   a coordinate return the 3-dimensional Euclidean distance between the locations
   */
  def distance3D(c: Coordinate): Double = {
    val dx = x - c.x
    val dy = y - c.y
    val dz = getZ - c.getZ
    Math.sqrt(dx * dx + dy * dy + dz * dz)
  }

  /**
   * Gets a hashcode for this coordinate.
   *
   * return a hashcode for this coordinate
   */
  override def hashCode: Int = { // Algorithm from Effective Java by Joshua Bloch [Jon Aquino]
    var result = 17
    result = 37 * result + Coordinate.hashCode(x)
    result = 37 * result + Coordinate.hashCode(y)
    result
  }
}
