// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.io

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
 * Formats numeric values for ordinates in a consistent, accurate way.
 *
 * Characteristics:
 *   - Always uses a period decimal separator (locale-independent).
 *   - NaN, +Inf, -Inf use canonical symbols (`NaN`, `Inf`, `-Inf`).
 *   - Other values are produced by `java.lang.Double.toString`, which is cross-platform (JVM +
 *     Scala.js) and round-trips exactly through `Double.parseDouble`.
 *
 * This is a cross-platform re-implementation: upstream JTS uses `java.text.DecimalFormat` (and
 * configurable max fraction digits), which is not available in Scala.js' javalib. For the values
 * JTS produces (geometry ordinates), `Double.toString` is fully sufficient and the output remains
 * valid WKT.
 */
object OrdinateFormat:
  val REP_POS_INF         = "Inf"
  val REP_NEG_INF         = "-Inf"
  val REP_NAN             = "NaN"
  val MAX_FRACTION_DIGITS = 325

  var DEFAULT: OrdinateFormat = new OrdinateFormat

  def create(maximumFractionDigits: Int): OrdinateFormat =
    new OrdinateFormat(maximumFractionDigits)

class OrdinateFormat:
  // maximumFractionDigits is accepted for API compatibility but ignored:
  // Double.toString round-trips exactly with whatever precision the value has.
  def this(maximumFractionDigits: Int) = this()

  def format(ord: Double): String =
    if java.lang.Double.isNaN(ord) then OrdinateFormat.REP_NAN
    else if java.lang.Double.isInfinite(ord) then
      if ord > 0 then OrdinateFormat.REP_POS_INF else OrdinateFormat.REP_NEG_INF
    else java.lang.Double.toString(ord)
