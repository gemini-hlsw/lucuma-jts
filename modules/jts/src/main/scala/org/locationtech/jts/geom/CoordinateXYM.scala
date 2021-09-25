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
 * Coordinate subclass supporting XYM ordinates. <p> This data object is suitable for use with
 * coordinate sequences with <tt>dimension</tt> = 3 and <tt>measures</tt> = 1. <p> The {link
 * Coordinate#z} field is visible, but intended to be ignored.
 *
 * @since 1.16
 */
@SerialVersionUID(2842127537691165613L)
object CoordinateXYM {

  /** Standard ordinate index value for X */
  val X = 0

  /** Standard ordinate index value for Y */
  val Y = 1

  /** CoordinateXYM does not support Z values. */
  val Z: Int = -1

  /**
   * Standard ordinate index value for M in XYM sequences.
   *
   * <p>This constant assumes XYM coordinate sequence definition. Check this assumption using {link
   * #getDimension()} and {link #getMeasures()} before use.
   */
  val M = 2
}

@SerialVersionUID(2842127537691165613L)
class CoordinateXYM(xArg: Double, yArg: Double, var m: Double)
    extends Coordinate(xArg, yArg, Coordinate.NULL_ORDINATE) {

  /**
   * Constructs a CoordinateXYM instance with the x and y ordinates of the given Coordinate.
   *
   * @param coord
   *   the coordinate providing the ordinates
   */
  def this(coord: Coordinate) = {
    this(coord.x, coord.y, coord.getM)
  }

  /**
   * Constructs a CoordinateXY instance with the x and y ordinates of the given CoordinateXYM.
   *
   * @param coord
   *   the coordinate providing the ordinates
   */
  def this(coord: CoordinateXYM) = {
    this(coord.x, coord.y, coord.m)
  }

  /**
   * Creates a copy of this CoordinateXYM.
   *
   * return a copy of this CoordinateXYM
   */
  override def copy = new CoordinateXYM(this)

  /** The m-measure. */
//  protected var m = .0

  /** The m-measure, if available. */
  override def getM: Double = this.m

  override def setM(m: Double): Unit = this.m = m

  /** The z-ordinate is not supported */
  override def getZ: Double = Coordinate.NULL_ORDINATE

  override def setZ(z: Double): Unit                                = throw new IllegalArgumentException(
    "CoordinateXY dimension 2 does not support z-ordinate"
  )

  override def setCoordinate(other: Coordinate): Unit               = {
    x = other.x
    y = other.y
    z = other.getZ
    m = other.getM
  }

  @nowarn
  override def getOrdinate(ordinateIndex: Int): Double = {
    ordinateIndex match {
      case CoordinateXYM.X =>
        return x
      case CoordinateXYM.Y =>
        return y
      case CoordinateXYM.M =>
        return m
    }
    throw new IllegalArgumentException("Invalid ordinate index: " + ordinateIndex)
  }

  override def setOrdinate(ordinateIndex: Int, value: Double): Unit = ordinateIndex match {
    case CoordinateXYM.X =>
      x = value
    case CoordinateXYM.Y =>
      y = value
    case CoordinateXYM.M =>
      m = value
    case _               =>
      throw new IllegalArgumentException("Invalid ordinate index: " + ordinateIndex)
  }

  override def toString: String                                     = "(" + x + ", " + y + " m=" + getM + ")"
}
