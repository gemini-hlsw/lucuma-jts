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

import org.locationtech.jts.geom.Dimension
import org.locationtech.jts.geom.Location

/**
 * Codes which combine a geometry dimension and a location on the geometry.
 *
 * @author
 *   mdavis
 */
object DimensionLocation {

  val EXTERIOR: Int = Location.EXTERIOR
  val POINT_INTERIOR = 103
  val LINE_INTERIOR  = 110
  val LINE_BOUNDARY  = 111
  val AREA_INTERIOR  = 120
  val AREA_BOUNDARY  = 121

  def locationArea(loc: Int): Int = loc match {
    case Location.INTERIOR => AREA_INTERIOR
    case Location.BOUNDARY => AREA_BOUNDARY
    case _                 => EXTERIOR
  }

  def locationLine(loc: Int): Int = loc match {
    case Location.INTERIOR => LINE_INTERIOR
    case Location.BOUNDARY => LINE_BOUNDARY
    case _                 => EXTERIOR
  }

  def locationPoint(loc: Int): Int = loc match {
    case Location.INTERIOR => POINT_INTERIOR
    case _                 => EXTERIOR
  }

  def location(dimLoc: Int): Int = dimLoc match {
    case POINT_INTERIOR | LINE_INTERIOR | AREA_INTERIOR => Location.INTERIOR
    case LINE_BOUNDARY | AREA_BOUNDARY                  => Location.BOUNDARY
    case _                                              => Location.EXTERIOR
  }

  def dimension(dimLoc: Int): Int = dimLoc match {
    case POINT_INTERIOR                 => Dimension.P
    case LINE_INTERIOR | LINE_BOUNDARY  => Dimension.L
    case AREA_INTERIOR | AREA_BOUNDARY  => Dimension.A
    case _                              => Dimension.FALSE
  }

  def dimension(dimLoc: Int, exteriorDim: Int): Int = {
    if (dimLoc == EXTERIOR)
      return exteriorDim
    dimension(dimLoc)
  }

}
