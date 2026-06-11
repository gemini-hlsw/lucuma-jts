// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2023 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v20.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.operation.relateng

import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.Location

object BasicPredicate {

  private val UNKNOWN = -1
  private val FALSE   = 0
  private val TRUE    = 1

  private def isKnown(value: Int): Boolean =
    value > UNKNOWN

  private def toBoolean(value: Int): Boolean =
    value == TRUE

  private def toValue(v: Boolean): Int =
    if (v) TRUE else FALSE

  /**
   * Tests if two geometries intersect based on an interaction at given locations.
   *
   * @param locA
   *   the location on geometry A
   * @param locB
   *   the location on geometry B return true if the geometries intersect
   */
  def isIntersection(locA: Int, locB: Int): Boolean =
    // -- i.e. some location on both geometries intersects
    locA != Location.EXTERIOR && locB != Location.EXTERIOR
}

/**
 * The base class for relate topological predicates with a boolean value. Implements tri-state
 * logic for the predicate value, to detect when the final value has been determined.
 *
 * @author
 *   Martin Davis
 */
abstract class BasicPredicate extends TopologyPredicate {

  private var valueInt: Int = BasicPredicate.UNKNOWN

  /*
  public boolean isSelfNodingRequired() {
    return false;
  }
   */

  override def isKnown: Boolean =
    BasicPredicate.isKnown(valueInt)

  override def value: Boolean =
    BasicPredicate.toBoolean(valueInt)

  /**
   * Updates the predicate value to the given state if it is currently unknown.
   *
   * @param v
   *   the predicate value to update
   */
  protected def setValue(v: Boolean): Unit = {
    // -- don't change already-known value
    if (isKnown)
      return
    valueInt = BasicPredicate.toValue(v)
  }

  protected def setValue(v: Int): Unit = {
    // -- don't change already-known value
    if (isKnown)
      return
    valueInt = v
  }

  protected def setValueIf(value: Boolean, cond: Boolean): Unit =
    if (cond)
      setValue(value)

  protected def require(cond: Boolean): Unit =
    if (!cond)
      setValue(false)

  protected def requireCovers(a: Envelope, b: Envelope): Unit =
    require(a.covers(b))
}
