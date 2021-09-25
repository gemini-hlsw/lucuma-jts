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
package org.locationtech.jts.algorithm

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryCollection
import org.locationtech.jts.geom.Point

/**
 * Computes a point in the interior of an point geometry. <h2>Algorithm</h2> Find a point which is
 * closest to the centroid of the geometry.
 *
 * @version 1.7
 */
object InteriorPointPoint {

  /**
   * Computes an interior point for the puntal components of a Geometry.
   *
   * @param geom
   *   the geometry to compute return the computed interior point, or <code>null</code> if the
   *   geometry has no puntal components
   */
  def getInteriorPoint(geom: Geometry): Coordinate = {
    val intPt = new InteriorPointPoint(geom)
    intPt.getInteriorPoint
  }
}

class InteriorPointPoint(val g: Geometry) {
  add(g)
  private val centroid                  = g.getCentroid.getCoordinate
  private var minDistance               = Double.MaxValue
  private var interiorPoint: Coordinate = null

  /**
   * Tests the point(s) defined by a Geometry for the best inside point. If a Geometry is not of
   * dimension 0 it is not tested.
   *
   * @param geom
   *   the geometry to add
   */
  private def add(geom: Geometry): Unit    = if (geom.isInstanceOf[Point]) add(geom.getCoordinate)
  else if (geom.isInstanceOf[GeometryCollection]) {
    val gc = geom.asInstanceOf[GeometryCollection]
    var i  = 0
    while (i < gc.getNumGeometries) {
      add(gc.getGeometryN(i))
      i += 1
    }
  }

  private def add(point: Coordinate): Unit = {
    val dist = point.distance(centroid)
    if (dist < minDistance) {
      interiorPoint = new Coordinate(point)
      minDistance = dist
    }
  }

  def getInteriorPoint: Coordinate = interiorPoint
}
