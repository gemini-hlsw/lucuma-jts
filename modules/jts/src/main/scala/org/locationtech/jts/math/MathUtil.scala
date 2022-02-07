// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
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

/**
 * Various utility functions for mathematical and numerical operations.
 *
 * @author
 *   mbdavis
 */
object MathUtil {

  /**
   * Clamps a <tt>double</tt> value to a given range.
   *
   * @param x
   *   the value to clamp
   * @param min
   *   the minimum value of the range
   * @param max
   *   the maximum value of the range return the clamped value
   */
  def clamp(x: Double, min: Double, max: Double): Double = {
    if (x < min) return min
    if (x > max) return max
    x
  }

  /**
   * Clamps an <tt>int</tt> value to a given range.
   *
   * @param x
   *   the value to clamp
   * @param min
   *   the minimum value of the range
   * @param max
   *   the maximum value of the range return the clamped value
   */
  def clamp(x: Int, min: Int, max: Int): Int = {
    if (x < min) return min
    if (x > max) return max
    x
  }

  private val LOG_10 = Math.log(10)

  /**
   * Computes the base-10 logarithm of a <tt>double</tt> value. <ul> <li>If the argument is NaN or
   * less than zero, then the result is NaN. <li>If the argument is positive infinity, then the
   * result is positive infinity. <li>If the argument is positive zero or negative zero, then the
   * result is negative infinity. </ul>
   *
   * @param x
   *   a positive number return the value log a, the base-10 logarithm of the input value
   */
  def log10(x: Double): Double = {
    val ln = Math.log(x)
    if (java.lang.Double.isInfinite(ln)) return ln
    if (java.lang.Double.isNaN(ln)) return ln
    ln / LOG_10
  }

  /**
   * Computes an index which wraps around a given maximum value. For values &gt;= 0, this is equals
   * to <tt>val % max</tt>. For values &lt; 0, this is equal to <tt>max - (-val) % max</tt>
   *
   * @param index
   *   the value to wrap
   * @param max
   *   the maximum value (or modulus) return the wrapped index
   */
  def wrap(index: Int, max: Int): Int = {
    if (index < 0) return max - ((-index) % max)
    index % max
  }

  /**
   * Computes the average of two numbers.
   *
   * @param x1
   *   a number
   * @param x2
   *   a number return the average of the inputs
   */
  def average(x1: Double, x2: Double): Double = (x1 + x2) / 2.0

  def max(v1: Double, v2: Double, v3: Double): Double = {
    var max = v1
    if (v2 > max) max = v2
    if (v3 > max) max = v3
    max
  }

  def max(v1: Double, v2: Double, v3: Double, v4: Double): Double = {
    var max = v1
    if (v2 > max) max = v2
    if (v3 > max) max = v3
    if (v4 > max) max = v4
    max
  }

  def min(v1: Double, v2: Double, v3: Double, v4: Double): Double = {
    var min = v1
    if (v2 < min) min = v2
    if (v3 < min) min = v3
    if (v4 < min) min = v4
    min
  }
}
