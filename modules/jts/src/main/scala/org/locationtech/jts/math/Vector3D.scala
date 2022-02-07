// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

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
 */ // Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

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
 */
package org.locationtech.jts.math

import org.locationtech.jts.geom.Coordinate

/**
 * Represents a vector in 3-dimensional Cartesian space.
 *
 * @author
 *   mdavis
 */
object Vector3D {

  /**
   * Computes the dot product of the 3D vectors AB and CD.
   *
   * @param A
   *   the start point of the first vector
   * @param B
   *   the end point of the first vector
   * @param C
   *   the start point of the second vector
   * @param D
   *   the end point of the second vector return the dot product
   */
  def dot(A: Coordinate, B: Coordinate, C: Coordinate, D: Coordinate): Double = {
    val ABx = B.x - A.x
    val ABy = B.y - A.y
    val ABz = B.getZ - A.getZ
    val CDx = D.x - C.x
    val CDy = D.y - C.y
    val CDz = D.getZ - C.getZ
    ABx * CDx + ABy * CDy + ABz * CDz
  }

  /**
   * Creates a new vector with given X, Y and Z components.
   *
   * @param x
   *   the X component
   * @param y
   *   the Y component
   * @param z
   *   the Z component return a new vector
   */
  def create(x: Double, y: Double, z: Double) = new Vector3D(x, y, z)

  /**
   * Creates a vector from a 3D {link Coordinate}. The coordinate should have the X,Y and Z
   * ordinates specified.
   *
   * @param coord
   *   the Coordinate to copy return a new vector
   */
  def create(coord: Coordinate) = new Vector3D(coord)

  /**
   * Computes the 3D dot-product of two {link Coordinate}s.
   *
   * @param v1
   *   the first vector
   * @param v2
   *   the second vector return the dot product of the vectors
   */
  def dot(v1: Coordinate, v2: Coordinate): Double = v1.x * v2.x + v1.y * v2.y + v1.getZ * v2.getZ

  /**
   * Computes the length of a vector.
   *
   * @param v
   *   a coordinate representing a 3D vector return the length of the vector
   */
  def length(v: Coordinate): Double = Math.sqrt(v.x * v.x + v.y * v.y + v.getZ * v.getZ)

  /**
   * Computes a vector having identical direction but normalized to have length 1.
   *
   * @param v
   *   a coordinate representing a 3D vector return a coordinate representing the normalized vector
   */
  def normalize(v: Coordinate): Coordinate = {
    val len = length(v)
    new Coordinate(v.x / len, v.y / len, v.getZ / len)
  }
}

class Vector3D(
  protected val x: Double = 0,
  protected val y: Double = 0,
  protected val z: Double = 0
) {
//  private var x = .0
//  private var y = .0
//  private var z = .0

  /**
   * Creates a new 3D vector from a {link Coordinate}. The coordinate should have the X,Y and Z
   * ordinates specified.
   *
   * @param coord
   *   the Coordinate to copy return a new vector
   */
  def this(v: Coordinate) =
    this(x = v.x, y = v.y, z = v.getZ)

  /**
   * Creates a vector with the direction and magnitude of the difference between the <tt>to</tt> and
   * <tt>from</tt> {link Coordinate}s.
   *
   * @param from
   *   the origin Coordinate
   * @param to
   *   the destination Coordinate return a new vector
   */
  def this(from: Coordinate, to: Coordinate) =
    this(x = to.x - from.x, y = to.y - from.y, z = to.getZ - from.getZ)

  /**
   * Creates a vector with the givne components.
   *
   * @param x
   *   the X component
   * @param y
   *   the Y component
   * @param z
   *   the Z component
   */
//  def this {
//    this()
//    this.x = x
//    this.y = y
//    this.z = z
//  }

  /**
   * Gets the X component of this vector.
   *
   * return the value of the X component
   */
  def getX: Double = x

  /**
   * Gets the Y component of this vector.
   *
   * return the value of the Y component
   */
  def getY: Double = y

  /**
   * Gets the Z component of this vector.
   *
   * return the value of the Z component
   */
  def getZ: Double = z

  /**
   * Computes a vector which is the sum of this vector and the given vector.
   *
   * @param v
   *   the vector to add return the sum of this and <code>v</code>
   */
  def add(v: Vector3D): Vector3D = Vector3D.create(x + v.x, y + v.y, z + v.z)

  /**
   * Computes a vector which is the difference of this vector and the given vector.
   *
   * @param v
   *   the vector to subtract return the difference of this and <code>v</code>
   */
  def subtract(v: Vector3D): Vector3D = Vector3D.create(x - v.x, y - v.y, z - v.z)

  /**
   * Creates a new vector which has the same direction and with length equals to the length of this
   * vector divided by the scalar value <code>d</code>.
   *
   * @param d
   *   the scalar divisor return a new vector with divided length
   */
  def divide(d: Double): Vector3D = Vector3D.create(x / d, y / d, z / d)

  /**
   * Computes the dot-product of two vectors
   *
   * @param v
   *   a vector return the dot product of the vectors
   */
  def dot(v: Vector3D): Double = x * v.x + y * v.y + z * v.z

  /**
   * Computes the length of this vector.
   *
   * return the length of the vector
   */
  def length: Double = Math.sqrt(x * x + y * y + z * z)

  /**
   * Computes a vector having identical direction but normalized to have length 1.
   *
   * return a new normalized vector
   */
  def normalize: Vector3D = {
    val vlength = length
    if (vlength > 0.0) return divide(vlength)
    Vector3D.create(0.0, 0.0, 0.0)
  }

  /**
   * Gets a string representation of this vector
   *
   * return a string representing this vector
   */
  override def toString: String = "[" + x + ", " + y + ", " + z + "]"

  /**
   * Tests if a vector <tt>o</tt> has the same values for the components.
   *
   * @param o
   *   a <tt>Vector3D</tt> with which to do the comparison. return true if <tt>other</tt> is a
   *   <tt>Vector3D</tt> with the same values for the x and y components.
   */
  override def equals(o: Any): Boolean = {
    if (!o.isInstanceOf[Vector3D]) return false
    val v = o.asInstanceOf[Vector3D]
    x == v.x && y == v.y && z == v.z
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
    result = 37 * result + Coordinate.hashCode(z)
    result
  }
}
