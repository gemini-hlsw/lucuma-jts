// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.io

/*
 * Copyright (c) 2018 Felix Obermaier
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
 * An enumeration of possible Well-Known-Text or Well-Known-Binary ordinates. <p> Intended to be
 * used as an {@code EnumSet<Ordinate>}, optimized create methods have been provided for {@link #
 * createXY ( )}, {@link # createXYM ( )}, {@link # createXYZ ( )} and {@link # createXYZM ( )}.
 */
object Ordinate extends Enumeration {
  type Ordinate = Value

  /**
   * X-ordinate
   */
  val X = Value

  /**
   * Y-ordinate
   */
  val Y = Value

  /**
   * Z-ordinate
   */
  val Z = Value

  /**
   * Measure-ordinate
   */
  val M = Value

  private val XY: Ordinate.ValueSet   =
    ValueSet(Ordinate.X, Ordinate.Y)
  private val XYZ: Ordinate.ValueSet  = ValueSet(X, Y, Z)
  private val XYM: Ordinate.ValueSet  = ValueSet(X, Y, M)
  private val XYZM: Ordinate.ValueSet = ValueSet(X, Y, Z, M)

  /**
   * EnumSet of X and Y ordinates, a copy is returned as EnumSets are not immutable.
   *
   * @return
   *   EnumSet of X and Y ordinates.
   */
  def createXY: ValueSet =
    XY

  /**
   * EnumSet of XYZ ordinates, a copy is returned as EnumSets are not immutable.
   *
   * @return
   *   EnumSet of X and Y ordinates.
   */
  def createXYZ: ValueSet =
    XYZ

  /**
   * EnumSet of XYM ordinates, a copy is returned as EnumSets are not immutable.
   *
   * @return
   *   EnumSet of X and Y ordinates.
   */
  def createXYM: ValueSet =
    XYM

  /**
   * EnumSet of XYZM ordinates, a copy is returned as EnumSets are not immutable.
   *
   * @return
   *   EnumSet of X and Y ordinates.
   */
  def createXYZM: ValueSet =
    XYZM
}
