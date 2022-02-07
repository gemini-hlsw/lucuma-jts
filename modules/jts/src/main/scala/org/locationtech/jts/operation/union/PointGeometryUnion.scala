// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
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
 */ /*
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
package org.locationtech.jts.operation.union

import java.util
import org.locationtech.jts.algorithm.PointLocator
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateArrays
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.Location
import org.locationtech.jts.geom.Point
import org.locationtech.jts.geom.Puntal
import org.locationtech.jts.geom.util.GeometryCombiner

/**
 * Computes the union of a {link Puntal} geometry with another arbitrary {link Geometry}. Does not
 * copy any component geometries.
 *
 * @author
 *   mbdavis
 */
object PointGeometryUnion {
  def union(pointGeom: Puntal, otherGeom: Geometry): Geometry = {
    val unioner = new PointGeometryUnion(pointGeom, otherGeom)
    unioner.union
  }
}

class PointGeometryUnion(val pointGeomArg: Puntal, var otherGeom: Geometry) {
  val pointGeom: Geometry = pointGeomArg.asInstanceOf[Geometry]
//  private var pointGeom = null
  private val geomFact    = otherGeom.getFactory

  def union: Geometry = {
    val locater          = new PointLocator
    // use a set to eliminate duplicates, as required for union
    val exteriorCoords   = new util.TreeSet[Coordinate]
    var i                = 0
    while (i < pointGeom.getNumGeometries) {
      val point = pointGeom.getGeometryN(i).asInstanceOf[Point]
      val coord = point.getCoordinate
      val loc   = locater.locate(coord, otherGeom)
      if (loc == Location.EXTERIOR) exteriorCoords.add(coord)
      i += 1
    }
    // if no points are in exterior, return the other geom
    if (exteriorCoords.size == 0) return otherGeom
    // make a puntal geometry of appropriate size
    var ptComp: Geometry = null
    val coords           = CoordinateArrays.toCoordinateArray(exteriorCoords)
    if (coords.length == 1) ptComp = geomFact.createPoint(coords(0))
    else ptComp = geomFact.createMultiPointFromCoords(coords)
    // add point component to the other geometry
    GeometryCombiner.combine(ptComp, otherGeom)
  }
}
