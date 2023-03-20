// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.precision

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateList
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.LinearRing
import org.locationtech.jts.geom.PrecisionModel
import org.locationtech.jts.geom.util.GeometryEditor

/*
 * Copyright (c) 2016 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v20.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */

class PrecisionReducerCoordinateOperation(
  var targetPM:        PrecisionModel,
  val removeCollapsed: Boolean
) extends GeometryEditor.CoordinateOperation {

  override def edit(coordinates: Array[Coordinate], geom: Geometry): Array[Coordinate] = {
    if (coordinates.length == 0) return null
    val reducedCoords = new Array[Coordinate](coordinates.length)
    // copy coordinates and reduce
    for (i <- 0 until coordinates.length) {
      val coord = new Coordinate(coordinates(i))
      targetPM.makePrecise(coord)
      reducedCoords(i) = coord
    }
    // remove repeated points, to simplify returned geometry as much as possible
    val noRepeatedCoordList = new CoordinateList(reducedCoords, false)
    val noRepeatedCoords = noRepeatedCoordList.toCoordinateArray

    /**
     * Check to see if the removal of repeated points collapsed the coordinate List to an invalid
     * length for the type of the parent geometry. It is not necessary to check for Point collapses,
     * since the coordinate list can never collapse to less than one point. If the length is
     * invalid, return the full-length coordinate array first computed, or null if collapses are
     * being removed. (This may create an invalid geometry - the client must handle this.)
     */
    var minLength       = 0
    if (geom.isInstanceOf[LineString]) minLength = 2
    if (geom.isInstanceOf[LinearRing]) minLength = 4
    var collapsedCoords = reducedCoords
    if (removeCollapsed) collapsedCoords = null
    // return null or original length coordinate array
    if (noRepeatedCoords.length < minLength) return collapsedCoords
    // ok to return shorter coordinate array
    noRepeatedCoords
  }
}
