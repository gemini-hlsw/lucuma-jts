/*
 * Copyright (c) 2016 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */ /*
 * Copyright (c) 2016 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.math

import org.locationtech.jts.algorithm.Angle
import org.locationtech.jts.algorithm.CGAlgorithmsDD
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.util.Assert

/**
 * A 2-dimensional mathematical vector represented by double-precision X and Y components.
 *
 * @author
 *   mbdavis
 */
object Vector2D {

  /**
   * Creates a new vector with given X and Y components.
   *
   * @param x
   *   the x component
   * @param y
   *   the y component return a new vector
   */
  def create(x: Double, y: Double) = new Vector2D(x, y)

  /**
   * Creates a new vector from an existing one.
   *
   * @param v
   *   the vector to copy return a new vector
   */
  def create(v: Vector2D) = new Vector2D(v)

  /**
   * Creates a vector from a {link Coordinate}.
   *
   * @param coord
   *   the Coordinate to copy return a new vector
   */
  def create(coord: Coordinate) = new Vector2D(coord)

  /**
   * Creates a vector with the direction and magnitude of the difference between the <tt>to</tt> and
   * <tt>from</tt> {link Coordinate}s.
   *
   * @param from
   *   the origin Coordinate
   * @param to
   *   the destination Coordinate return a new vector
   */
  def create(from: Coordinate, to: Coordinate) = new Vector2D(from, to)
}

class Vector2D(protected val x: Double = 0.0, protected val y: Double = 0.0) {

  /**
   * The X component of this vector.
   */
//  private var x = .0
  /**
   * The Y component of this vector.
   */
//  private var y = .0

//  def this {
//    this()
//    this.x = x
//    this.y = y
//  }

  def this(v: Vector2D) = {
    this(v.x, v.y)
////    x = v.x
//    y = v.y
  }

  def this(from: Coordinate, to: Coordinate) = {
    this(to.x - from.x, to.y - from.y)
  }

  def this(v: Coordinate) = {
    this(v.x, v.y)
//    x = v.x
//    y = v.y
  }

  def getX: Double = x

  def getY: Double = y

  def getComponent(index: Int): Double = {
    if (index == 0) return x
    y
  }

  def add(v: Vector2D): Vector2D = Vector2D.create(x + v.x, y + v.y)

  def subtract(v: Vector2D): Vector2D = Vector2D.create(x - v.x, y - v.y)

  /**
   * Multiplies the vector by a scalar value.
   *
   * @param d
   *   the value to multiply by return a new vector with the value v * d
   */
  def multiply(d: Double): Vector2D = Vector2D.create(x * d, y * d)

  /**
   * Divides the vector by a scalar value.
   *
   * @param d
   *   the value to divide by return a new vector with the value v / d
   */
  def divide(d: Double): Vector2D = Vector2D.create(x / d, y / d)

  def negate: Vector2D = Vector2D.create(-x, -y)

  def length: Double = Math.sqrt(x * x + y * y)

  def lengthSquared: Double = x * x + y * y

  def normalize: Vector2D = {
    val vlength = length
    if (vlength > 0.0) return divide(vlength)
    Vector2D.create(0.0, 0.0)
  }

  def average(v: Vector2D): Vector2D = weightedSum(v, 0.5)

  /**
   * Computes the weighted sum of this vector with another vector, with this vector contributing a
   * fraction of <tt>frac</tt> to the total. <p> In other words, <pre> sum = frac * this + (1 -
   * frac) * v </pre>
   *
   * @param v
   *   the vector to sum
   * @param frac
   *   the fraction of the total contributed by this vector return the weighted sum of the two
   *   vectors
   */
  def weightedSum(v: Vector2D, frac: Double): Vector2D =
    Vector2D.create(frac * x + (1.0 - frac) * v.x, frac * y + (1.0 - frac) * v.y)

  /**
   * Computes the distance between this vector and another one.
   *
   * @param v
   *   a vector return the distance between the vectors
   */
  def distance(v: Vector2D): Double = {
    val delx = v.x - x
    val dely = v.y - y
    Math.sqrt(delx * delx + dely * dely)
  }

  /**
   * Computes the dot-product of two vectors
   *
   * @param v
   *   a vector return the dot product of the vectors
   */
  def dot(v: Vector2D): Double = x * v.x + y * v.y

  def angle: Double = Math.atan2(y, x)

  def angle(v: Vector2D): Double = Angle.diff(v.angle, angle)

  def angleTo(v: Vector2D): Double = {
    val a1     = angle
    val a2     = v.angle
    val angDel = a2 - a1
    // normalize, maintaining orientation
    if (angDel <= -Math.PI) return angDel + Angle.PI_TIMES_2
    if (angDel > Math.PI) return angDel - Angle.PI_TIMES_2
    angDel
  }

  def rotate(angle: Double): Vector2D = {
    val cos = Math.cos(angle)
    val sin = Math.sin(angle)
    Vector2D.create(x * cos - y * sin, x * sin + y * cos)
  }

  /**
   * Rotates a vector by a given number of quarter-circles (i.e. multiples of 90 degrees or Pi/2
   * radians). A positive number rotates counter-clockwise, a negative number rotates clockwise.
   * Under this operation the magnitude of the vector and the absolute values of the ordinates do
   * not change, only their sign and ordinate index.
   *
   * @param numQuarters
   *   the number of quarter-circles to rotate by return the rotated vector.
   */
  def rotateByQuarterCircle(numQuarters: Int): Vector2D = {
    var nQuad = numQuarters % 4
    if (numQuarters < 0 && nQuad != 0) nQuad = nQuad + 4
    nQuad match {
      case 0 =>
        return Vector2D.create(x, y)
      case 1 =>
        return Vector2D.create(-y, x)
      case 2 =>
        return Vector2D.create(-x, -y)
      case 3 =>
        return Vector2D.create(y, -x)
    }
    Assert.shouldNeverReachHere()
    null
  }

  def isParallel(v: Vector2D): Boolean = 0.0 == CGAlgorithmsDD.signOfDet2x2(x, y, v.x, v.y)

  def translate(coord: Coordinate) = new Coordinate(x + coord.x, y + coord.y)

  def toCoordinate = new Coordinate(x, y)

  /**
   * Creates a copy of this vector
   *
   * return a copy of this vector
   */
  override def clone = new Vector2D(this)

  /**
   * Gets a string representation of this vector
   *
   * return a string representing this vector
   */
  override def toString: String = "[" + x + ", " + y + "]"

  /**
   * Tests if a vector <tt>o</tt> has the same values for the x and y components.
   *
   * @param o
   *   a <tt>Vector2D</tt> with which to do the comparison. return true if <tt>other</tt> is a
   *   <tt>Vector2D</tt> with the same values for the x and y components.
   */
  override def equals(o: Any): Boolean = {
    if (!o.isInstanceOf[Vector2D]) return false
    val v = o.asInstanceOf[Vector2D]
    x == v.x && y == v.y
  }

  /**
   * Gets a hashcode for this vector.
   *
   * return a hashcode for this vector
   */
  override def hashCode: Int = { // Algorithm from Effective Java by Joshua Bloch
    var result = 17
    result = 37 * result + Coordinate.hashCode(x)
    result = 37 * result + Coordinate.hashCode(y)
    result
  }
}
