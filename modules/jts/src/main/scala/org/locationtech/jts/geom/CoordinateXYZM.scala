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
package org.locationtech.jts.geom

import scala.annotation.nowarn

/**
 * Coordinate subclass supporting XYZM ordinates. <p> This data object is suitable for use with
 * coordinate sequences with <tt>dimension</tt> = 4 and <tt>measures</tt> = 1.
 *
 * @since 1.16
 */
@SerialVersionUID(-8763329985881823442L)
class CoordinateXYZM(xArg: Double, yArg: Double, zArg: Double, var m: Double)
    extends Coordinate(xArg, yArg, zArg) {

  /**
   * Constructs a CoordinateXYZM instance with the ordinates of the given Coordinate.
   *
   * @param coord
   *   the coordinate providing the ordinates
   */
  def this(coord: Coordinate) = {
    this(coord.x, coord.y, coord.z, coord.getM)
  }

  /**
   * Constructs a CoordinateXYZM instance with the ordinates of the given CoordinateXYZM.
   *
   * @param coord
   *   the coordinate providing the ordinates
   */
  def this(coord: CoordinateXYZM) = {
    this(coord.x, coord.y, coord.z, coord.m)
  }

  /**
   * Creates a copy of this CoordinateXYZM.
   *
   * return a copy of this CoordinateXYZM
   */
  override def copy = new CoordinateXYZM(this)

  /** The m-measure, if available. */
  override def getM: Double = m

  override def setM(m: Double): Unit = this.m = m

  @nowarn
  override def getOrdinate(ordinateIndex: Int): Double = {
    ordinateIndex match {
      case Coordinate.X =>
        return x
      case Coordinate.Y =>
        return y
      case Coordinate.Z =>
        return getZ // sure to delegate to subclass rather than offer direct field access
      case Coordinate.M =>
        return getM
    }
    throw new IllegalArgumentException("Invalid ordinate index: " + ordinateIndex)
  }

  override def setCoordinate(other: Coordinate): Unit               = {
    x = other.x
    y = other.y
    z = other.getZ
    m = other.getM
  }

  override def setOrdinate(ordinateIndex: Int, value: Double): Unit = ordinateIndex match {
    case Coordinate.X =>
      x = value
    case Coordinate.Y =>
      y = value
    case Coordinate.Z =>
      z = value
    case Coordinate.M =>
      m = value
    case _            =>
      throw new IllegalArgumentException("Invalid ordinate index: " + ordinateIndex)
  }

  override def toString: String                                     = "(" + x + ", " + y + ", " + getZ + " m=" + getM + ")"
}
