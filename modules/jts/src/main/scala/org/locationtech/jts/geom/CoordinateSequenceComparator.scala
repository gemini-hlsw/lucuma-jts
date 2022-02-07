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
 */
package org.locationtech.jts.geom

import java.util.Comparator

/**
 * Compares two {link CoordinateSequence}s. For sequences of the same dimension, the ordering is
 * lexicographic. Otherwise, lower dimensions are sorted before higher. The dimensions compared can
 * be limited; if this is done ordinate dimensions above the limit will not be compared. <p> If
 * different behaviour is required for comparing size, dimension, or coordinate values, any or all
 * methods can be overridden.
 */
object CoordinateSequenceComparator {

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

class CoordinateSequenceComparator(dimensionLimit: Int = Int.MaxValue)

/**
 * Creates a comparator which will test all dimensions.
 */ extends Comparator[CoordinateSequence] {

  /**
   * The number of dimensions to test
   */
//  protected var dimensionLimit = 0

  /**
   * Creates a comparator which will test only the specified number of dimensions.
   *
   * @param dimensionLimit
   *   the number of dimensions to test
   */
//  def this(dimensionLimit: Int) {
//    this()
//    this.dimensionLimit = dimensionLimit
//  }

  /**
   * Compares two {link CoordinateSequence}s for relative order.
   *
   * @param o1
   *   a { @link CoordinateSequence}
   * @param o2
   *   a { @link CoordinateSequence} return -1, 0, or 1 depending on whether o1 is less than, equal
   *   to, or greater than o2
   */
  override def compare(s1: CoordinateSequence, s2: CoordinateSequence): Int = {
    val size1      = s1.size
    val size2      = s2.size
    val dim1       = s1.getDimension
    val dim2       = s2.getDimension
    var minDim     = dim1
    if (dim2 < minDim) minDim = dim2
    var dimLimited = false
    if (dimensionLimit <= minDim) {
      minDim = dimensionLimit
      dimLimited = true
    }
    // lower dimension is less than higher
    if (!dimLimited) {
      if (dim1 < dim2) return -1
      if (dim1 > dim2) return 1
    }
    // lexicographic ordering of point sequences
    var i          = 0
    while (i < size1 && i < size2) {
      val ptComp = compareCoordinate(s1, s2, i, minDim)
      if (ptComp != 0) return ptComp
      i += 1
    }
    if (i < size1) return 1
    if (i < size2) return -1
    0
  }

  /**
   * Compares the same coordinate of two {link CoordinateSequence}s along the given number of
   * dimensions.
   *
   * @param s1
   *   a { @link CoordinateSequence}
   * @param s2
   *   a { @link CoordinateSequence}
   * @param i
   *   the index of the coordinate to test
   * @param dimension
   *   the number of dimensions to test return -1, 0, or 1 depending on whether s1[i] is less than,
   *   equal to, or greater than s2[i]
   */
  protected def compareCoordinate(
    s1:        CoordinateSequence,
    s2:        CoordinateSequence,
    i:         Int,
    dimension: Int
  ): Int = {
    var d = 0
    while (d < dimension) {
      val ord1 = s1.getOrdinate(i, d)
      val ord2 = s2.getOrdinate(i, d)
      val comp = CoordinateSequenceComparator.compare(ord1, ord2)
      if (comp != 0) return comp
      d += 1
    }
    0
  }
}
