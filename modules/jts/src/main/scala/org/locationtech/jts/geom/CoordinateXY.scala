// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
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

import scala.annotation.nowarn

/**
 * Coordinate subclass supporting XY ordinates.
 * <p>
 * This data object is suitable for use with coordinate sequences with <tt>dimension</tt> = 2.
 * <p>
 * The {link Coordinate#z} field is visible, but intended to be ignored.
 *
 * @since 1.16
 */
@SerialVersionUID(3532307803472313082L)
object CoordinateXY {

  /** Standard ordinate index value for X */
  val X = 0

  /** Standard ordinate index value for Y */
  val Y = 1

  /** CoordinateXY does not support Z values. */
  val Z: Int = -1

  /** CoordinateXY does not support M measures. */
  val M: Int = -1
}

@SerialVersionUID(3532307803472313082L)
class CoordinateXY(xArg: Double, yArg: Double)
    extends Coordinate(xArg, yArg, Coordinate.NULL_ORDINATE) {

  /**
   * Constructs a CoordinateXY instance with the x and y ordinates of the given Coordinate.
   *
   * @param coord the Coordinate providing the ordinates
   */
  def this(coord: Coordinate) = {
    this(coord.x, coord.y)
  }

  /**
   * Constructs a CoordinateXY instance with the x and y ordinates of the given CoordinateXY.
   *
   * @param coord the CoordinateXY providing the ordinates
   */
  def this(coord: CoordinateXY) = {
    this(coord.x, coord.y)
  }

  /**
   * Creates a copy of this CoordinateXY.
   *
   * return a copy of this CoordinateXY
   */
  override def copy = new CoordinateXY(this)

  /** The z-ordinate is not supported */
  override def getZ: Double = Coordinate.NULL_ORDINATE

  override def setZ(z: Double): Unit = throw new IllegalArgumentException(
    "CoordinateXY dimension 2 does not support z-ordinate"
  )

  override def setCoordinate(other: Coordinate): Unit = {
    x = other.x
    y = other.y
    z = other.getZ
  }

  @nowarn
  override def getOrdinate(ordinateIndex: Int): Double = {
    ordinateIndex match {
      case CoordinateXY.X =>
        return x
      case CoordinateXY.Y =>
        return y
    }
    throw new IllegalArgumentException("Invalid ordinate index: " + ordinateIndex)
  }

  override def setOrdinate(ordinateIndex: Int, value: Double): Unit = ordinateIndex match {
    case CoordinateXY.X =>
      x = value
    case CoordinateXY.Y =>
      y = value
    case _              =>
      throw new IllegalArgumentException("Invalid ordinate index: " + ordinateIndex)
  }

  override def toString: String = "(" + x + ", " + y + ")"
}
