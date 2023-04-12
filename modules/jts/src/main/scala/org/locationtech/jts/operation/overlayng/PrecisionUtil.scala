// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.operation.overlayng

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateFilter
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.PrecisionModel
import org.locationtech.jts.io.OrdinateFormat
import org.locationtech.jts.math.MathUtil

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

/**
 * Functions for computing precision model scale factors that ensure robust geometry operations. In
 * particular, these can be used to automatically determine appropriate scale factors for operations
 * using limited-precision noding (such as {@link OverlayNG}). <p> WARNING: the
 * <code>inherentScale</code> and <code>robustScale</code> functions can be very slow, due to the
 * method used to determine number of decimal places of a number. These are not recommended for
 * production use.
 *
 * @author
 *   Martin Davis
 */
object PrecisionUtil {

  /**
   * A number of digits of precision which leaves some computational "headroom" to ensure robust
   * evaluation of certain double-precision floating point geometric operations.
   *
   * This value should be less than the maximum decimal precision of double-precision values (16).
   */
  var MAX_ROBUST_DP_DIGITS = 14

  /**
   * Determines a precision model to use for robust overlay operations. The precision scale factor
   * is chosen to maximize output precision while avoiding round-off issues. <p> NOTE: this is a
   * heuristic determination, so is not guaranteed to eliminate precision issues. <p> WARNING: this
   * is very slow.
   *
   * @param a
   *   a geometry
   * @param b
   *   a geometry
   * @return
   *   a suitable precision model for overlay
   */
  def robustPM(a: Geometry, b: Geometry): PrecisionModel = {
    val scale = PrecisionUtil.robustScale(a, b)
    new PrecisionModel(scale)
  }

  /**
   * Computes a safe scale factor for a numeric value. A safe scale factor ensures that rounded
   * number has no more than {@link MAX_PRECISION_DIGITS} digits of precision.
   *
   * @param value
   *   a numeric value
   * @return
   *   a safe scale factor for the value
   */
  def safeScale(value: Double): Double = precisionScale(value, MAX_ROBUST_DP_DIGITS)

  /**
   * Computes a safe scale factor for a geometry. A safe scale factor ensures that the rounded
   * ordinates have no more than {@link MAX_PRECISION_DIGITS} digits of precision.
   *
   * @param geom
   *   a geometry
   * @return
   *   a safe scale factor for the geometry ordinates
   */
  def safeScale(geom: Geometry): Double = safeScale(maxBoundMagnitude(geom.getEnvelopeInternal))

  /**
   * Computes a safe scale factor for two geometries. A safe scale factor ensures that the rounded
   * ordinates have no more than {@link MAX_PRECISION_DIGITS} digits of precision.
   *
   * @param a
   *   a geometry
   * @param b
   *   a geometry (which may be null)
   * @return
   *   a safe scale factor for the geometry ordinates
   */
  def safeScale(a: Geometry, b: Geometry): Double = {
    var maxBnd = maxBoundMagnitude(a.getEnvelopeInternal)
    if (b != null) {
      val maxBndB = maxBoundMagnitude(b.getEnvelopeInternal)
      maxBnd = Math.max(maxBnd, maxBndB)
    }
    val scale  = PrecisionUtil.safeScale(maxBnd)
    scale
  }

  /**
   * Determines the maximum magnitude (absolute value) of the bounds of an of an envelope. This is
   * equal to the largest ordinate value which must be accommodated by a scale factor.
   *
   * @param env
   *   an envelope
   * @return
   *   the value of the maximum bound magnitude
   */
  private def maxBoundMagnitude(env: Envelope) = MathUtil.max(Math.abs(env.getMaxX),
                                                              Math.abs(env.getMaxY),
                                                              Math.abs(env.getMinX),
                                                              Math.abs(env.getMinY)
  )

  /**
   * Computes the scale factor which will produce a given number of digits of precision (significant
   * digits) when used to round the given number. <p> For example: to provide 5 decimal digits of
   * precision for the number 123.456 the precision scale factor is 100; for 3 digits of precision
   * the scale factor is 1; for 2 digits of precision the scale factor is 0.1. <p> Rounding to the
   * scale factor can be performed with {@link PrecisionModel# round}
   *
   * @param value
   *   a number to be rounded
   * @param precisionDigits
   *   the number of digits of precision required
   * @return
   *   scale factor which provides the required number of digits of precision
   * @see
   *   PrecisionModel.round
   */
  private def precisionScale(value: Double, precisionDigits: Int) = { // the smallest power of 10 greater than the value
    val magnitude   = (Math.log(value) / Math.log(10) + 1.0).toInt
    val precDigits  = precisionDigits - magnitude
    val scaleFactor = Math.pow(10.0, precDigits)
    scaleFactor
  }

  /**
   * Computes the inherent scale of a number. The inherent scale is the scale factor for rounding
   * which preserves <b>all</b> digits of precision (significant digits) present in the numeric
   * value. In other words, it is the scale factor which does not change the numeric value when
   * rounded: <pre> num = round( num, inherentScale(num) ) </pre>
   *
   * @param value
   *   a number
   * @return
   *   the inherent scale factor of the number
   */
  def inherentScale(value: Double): Double = {
    val numDec      = numberOfDecimals(value)
    val scaleFactor = Math.pow(10.0, numDec)
    scaleFactor
  }

  /**
   * Computes the inherent scale of a geometry. The inherent scale is the scale factor for rounding
   * which preserves <b>all</b> digits of precision (significant digits) present in the geometry
   * ordinates. <p> This is the maximum inherent scale of all ordinate values in the geometry. <p>
   * WARNING: this is very slow.
   *
   * @param value
   *   a number
   * @return
   *   the inherent scale factor of the number
   */
  def inherentScale(geom: Geometry): Double = {
    val scaleFilter = new PrecisionUtil.InherentScaleFilter
    geom.applyF(scaleFilter)
    scaleFilter.getScale
  }

  /**
   * Computes the inherent scale of two geometries. The inherent scale is the scale factor for
   * rounding which preserves <b>all</b> digits of precision (significant digits) present in the
   * geometry ordinates. <p> This is the maximum inherent scale of all ordinate values in the
   * geometries. <p> WARNING: this is very slow.
   *
   * @param a
   *   a geometry
   * @param b
   *   a geometry
   * @return
   *   the inherent scale factor of the two geometries
   */
  def inherentScale(a: Geometry, b: Geometry): Double = {
    var scale = PrecisionUtil.inherentScale(a)
    if (b != null) {
      val scaleB = PrecisionUtil.inherentScale(b)
      scale = Math.max(scale, scaleB)
    }
    scale
  }

  /**
   * Determines the number of decimal places represented in a double-precision number (as determined
   * by Java). This uses the Java double-precision print routine to determine the number of decimal
   * places, This is likely not optimal for performance, but should be accurate and portable.
   *
   * @param value
   *   a numeric value
   * @return
   *   the number of decimal places in the value
   */
  private def numberOfDecimals(value: Double): Int = {

    /**
     * Ensure that scientific notation is NOT used (it would skew the number of fraction digits)
     */
    val s        = OrdinateFormat.DEFAULT.format(value)
    if (s.endsWith(".0")) return 0
    val len      = s.length
    val decIndex = s.indexOf('.')
    if (decIndex <= 0) return 0
    len - decIndex - 1
  }

  /**
   * Applies the inherent scale calculation to every ordinate in a geometry. <p> WARNING: this is
   * very slow.
   *
   * @author
   *   Martin Davis
   */
  private class InherentScaleFilter extends CoordinateFilter {
    private var scale: Double = 0

    def getScale: Double = scale

    override def filter(coord: Coordinate): Unit = {
      updateScaleMax(coord.getX)
      updateScaleMax(coord.getY)
    }

    private def updateScaleMax(value: Double): Unit = {
      val scaleVal = PrecisionUtil.inherentScale(value)
      if (scaleVal > scale) { // System.out.println("Value " + value + " has scale: " + scaleVal);
        scale = scaleVal
      }
    }
  }

  /**
   * Determines a precision model to use for robust overlay operations for one geometry. The
   * precision scale factor is chosen to maximize output precision while avoiding round-off issues.
   * <p> NOTE: this is a heuristic determination, so is not guaranteed to eliminate precision
   * issues. <p> WARNING: this is very slow.
   *
   * @param a
   *   a geometry
   * @return
   *   a suitable precision model for overlay
   */
  def robustPM(a: Geometry): PrecisionModel = {
    val scale = PrecisionUtil.robustScale(a)
    new PrecisionModel(scale)
  }

  /**
   * Determines a scale factor which maximizes the digits of precision and is safe to use for
   * overlay operations. The robust scale is the minimum of the inherent scale and the safe scale
   * factors. <p> WARNING: this is very slow.
   *
   * @param a
   *   a geometry
   * @param b
   *   a geometry
   * @return
   *   a scale factor for use in overlay operations
   */
  def robustScale(a: Geometry, b: Geometry): Double = {
    val inherentScale0 = inherentScale(a, b)
    val safeScale0     = safeScale(a, b)
    robustScale(inherentScale0, safeScale0)
  }

  /**
   * Determines a scale factor which maximizes the digits of precision and is safe to use for
   * overlay operations. The robust scale is the minimum of the inherent scale and the safe scale
   * factors.
   *
   * @param a
   *   a geometry
   * @return
   *   a scale factor for use in overlay operations
   */
  def robustScale(a: Geometry): Double = {
    val inherentScale0 = inherentScale(a)
    val safeScale0     = safeScale(a)
    robustScale(inherentScale0, safeScale0)
  }

  private def robustScale(inherentScale: Double, safeScale: Double): Double = {

    /**
     * Use safe scale if lower, since it is important to preserve some precision for robustness
     */
    if (inherentScale <= safeScale) return inherentScale
    // System.out.println("Scale = " + scale);
    safeScale
  }
}
