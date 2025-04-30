// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
package org.locationtech.jts.index.strtree

import org.locationtech.jts.util.Assert

/**
 * A contiguous portion of 1D-space. Used internally by SIRtree.
 *
 * @see
 *   SIRtree
 * @version 1.7
 */
class Interval(var min: Double, var max: Double) {
  Assert.isTrue(min <= max)

  def this(other: Interval) =
    this(other.min, other.max)

  def getCentre: Double = (min + max) / 2

  /**
   * return this
   */
  def expandToInclude(other: Interval): Interval = {
    max = Math.max(max, other.max)
    min = Math.min(min, other.min)
    this
  }

  def intersects(other: Interval): Boolean = !(other.min > max || other.max < min)

  override def equals(o: Any): Boolean = {
    if (!o.isInstanceOf[Interval]) return false
    val other = o.asInstanceOf[Interval]
    min == other.min && max == other.max
  }
}
