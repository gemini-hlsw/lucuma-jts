// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2019 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v20.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.shape.fractal

import org.locationtech.jts.geom.Coordinate

/**
 * Encodes points as the index along finite planar Hilbert curves. <p> The planar Hilbert Curve is
 * a continuous space-filling curve. In the limit the Hilbert curve has infinitely many vertices
 * and fills the space of the unit square. A sequence of finite approximations to the infinite
 * Hilbert curve is defined by the level number. The finite Hilbert curve at level n contains 2^(n
 * + 1) points. Each finite Hilbert curve defines an ordering of the points in the 2-dimensional
 * range square containing the curve. <p> This implementation represents codes using 32-bit
 * integers. This allows levels 0 to 16 to be handled. <p> The Hilbert order has the property that
 * it tends to preserve locality.
 *
 * @author
 *   Martin Davis
 */
object HilbertCode {

  /**
   * The maximum curve level that can be represented.
   */
  val MAX_LEVEL = 16

  /**
   * The number of points in the curve for the given level. The number of points is 2^(2 * level).
   *
   * @param level
   *   the level of the curve return the number of points
   */
  def size(level: Int): Int = {
    checkLevel(level)
    Math.pow(2, 2 * level).toInt
  }

  /**
   * The maximum ordinate value for points in the curve for the given level. The maximum ordinate
   * is 2^level - 1.
   *
   * @param level
   *   the level of the curve return the maximum ordinate value
   */
  def maxOrdinate(level: Int): Int = {
    checkLevel(level)
    Math.pow(2, level).toInt - 1
  }

  /**
   * The level of the finite Hilbert curve which contains at least the given number of points.
   *
   * @param numPoints
   *   the number of points required return the level of the curve
   */
  def level(numPoints: Int): Int = {
    val pow2  = (Math.log(numPoints.toDouble) / Math.log(2)).toInt
    var level = pow2 / 2
    val sz    = size(level)
    if (sz < numPoints) level += 1
    level
  }

  private def checkLevel(level: Int): Unit =
    if (level > MAX_LEVEL)
      throw new IllegalArgumentException("Level must be in range 0 to " + MAX_LEVEL)

  /**
   * Encodes a point (x,y) in the range of the the Hilbert curve at a given level as the index of
   * the point along the curve. The index will lie in the range [0, 2^(level + 1)].
   *
   * @param level
   *   the level of the Hilbert curve
   * @param x
   *   the x ordinate of the point
   * @param y
   *   the y ordinate of the point return the index of the point along the Hilbert curve
   */
  def encode(level: Int, x0: Int, y0: Int): Int = {
    // Fast Hilbert curve algorithm by http://threadlocalmutex.com/
    // Ported from C++ https://github.com/rawrunprotected/hilbert_curves (public
    // domain)

    val lvl = levelClamp(level)

    val x = x0 << (16 - lvl)
    val y = y0 << (16 - lvl)

    var a: Long = (x ^ y).toLong
    var b: Long = 0xffff ^ a
    var c: Long = 0xffff ^ (x | y).toLong
    var d: Long = (x & (y ^ 0xffff)).toLong

    var bigA: Long = a | (b >> 1)
    var bigB: Long = (a >> 1) ^ a
    var bigC: Long = ((c >> 1) ^ (b & (d >> 1))) ^ c
    var bigD: Long = ((a & (c >> 1)) ^ (d >> 1)) ^ d

    a = bigA
    b = bigB
    c = bigC
    d = bigD
    bigA = (a & (a >> 2)) ^ (b & (b >> 2))
    bigB = (a & (b >> 2)) ^ (b & ((a ^ b) >> 2))
    bigC ^= (a & (c >> 2)) ^ (b & (d >> 2))
    bigD ^= (b & (c >> 2)) ^ ((a ^ b) & (d >> 2))

    a = bigA
    b = bigB
    c = bigC
    d = bigD
    bigA = (a & (a >> 4)) ^ (b & (b >> 4))
    bigB = (a & (b >> 4)) ^ (b & ((a ^ b) >> 4))
    bigC ^= (a & (c >> 4)) ^ (b & (d >> 4))
    bigD ^= (b & (c >> 4)) ^ ((a ^ b) & (d >> 4))

    a = bigA
    b = bigB
    c = bigC
    d = bigD
    bigC ^= (a & (c >> 8)) ^ (b & (d >> 8))
    bigD ^= (b & (c >> 8)) ^ ((a ^ b) & (d >> 8))

    a = bigC ^ (bigC >> 1)
    b = bigD ^ (bigD >> 1)

    var i0: Long = (x ^ y).toLong
    var i1: Long = b | (0xffff ^ (i0 | a))

    i0 = (i0 | (i0 << 8)) & 0x00ff00ff
    i0 = (i0 | (i0 << 4)) & 0x0f0f0f0f
    i0 = (i0 | (i0 << 2)) & 0x33333333
    i0 = (i0 | (i0 << 1)) & 0x55555555

    i1 = (i1 | (i1 << 8)) & 0x00ff00ff
    i1 = (i1 | (i1 << 4)) & 0x0f0f0f0f
    i1 = (i1 | (i1 << 2)) & 0x33333333
    i1 = (i1 | (i1 << 1)) & 0x55555555

    val index = ((i1 << 1) | i0) >> (32 - 2 * lvl)
    index.toInt
  }

  /**
   * Clamps a level to the range valid for the index algorithm used.
   *
   * @param level
   *   the level of a Hilbert curve return a valid level
   */
  private def levelClamp(level: Int): Int = {
    // clamp order to [1, 16]
    var lvl = if (level < 1) 1 else level
    lvl = if (lvl > MAX_LEVEL) MAX_LEVEL else lvl
    lvl
  }

  /**
   * Computes the point on a Hilbert curve of given level for a given code index. The point
   * ordinates will lie in the range [0, 2^level - 1].
   *
   * @param level
   *   the Hilbert curve level
   * @param index
   *   the index of the point on the curve return the point on the Hilbert curve
   */
  def decode(level: Int, index0: Int): Coordinate = {
    checkLevel(level)
    val lvl = levelClamp(level)

    val index = index0 << (32 - 2 * lvl)

    val i0 = deinterleave(index)
    val i1 = deinterleave(index >> 1)

    val t0 = (i0 | i1) ^ 0xffff
    val t1 = i0 & i1

    val prefixT0 = prefixScan(t0)
    val prefixT1 = prefixScan(t1)

    val a = ((i0 ^ 0xffff) & prefixT1) | (i0 & prefixT0)

    val x = (a ^ i1) >> (16 - lvl)
    val y = (a ^ i0 ^ i1) >> (16 - lvl)

    new Coordinate(x.toDouble, y.toDouble)
  }

  private def prefixScan(x0: Long): Long = {
    var x = x0
    x = (x >> 8) ^ x
    x = (x >> 4) ^ x
    x = (x >> 2) ^ x
    x = (x >> 1) ^ x
    x
  }

  private def deinterleave(x0: Int): Long = {
    var x = x0 & 0x55555555
    x = (x | (x >> 1)) & 0x33333333
    x = (x | (x >> 2)) & 0x0f0f0f0f
    x = (x | (x >> 4)) & 0x00ff00ff
    x = (x | (x >> 8)) & 0x0000ffff
    x.toLong
  }
}
