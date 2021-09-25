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
import org.locationtech.jts.geom.LineString

/**
 * Computes a point in the interior of an linear geometry. <h2>Algorithm</h2> <ul> <li>Find an
 * interior vertex which is closest to the centroid of the linestring. <li>If there is no interior
 * vertex, find the endpoint which is closest to the centroid. </ul>
 *
 * @version 1.7
 */
object InteriorPointLine {

  /**
   * Computes an interior point for the linear components of a Geometry.
   *
   * @param geom
   *   the geometry to compute return the computed interior point, or <code>null</code> if the
   *   geometry has no linear components
   */
  def getInteriorPoint(geom: Geometry): Coordinate = {
    val intPt = new InteriorPointLine(geom)
    intPt.getInteriorPoint
  }
}

class InteriorPointLine(val g: Geometry) {
  addInterior(g)
  private val centroid                  = g.getCentroid.getCoordinate
  private var minDistance               = Double.MaxValue
  private var interiorPoint: Coordinate = null
  if (interiorPoint == null) addEndpoints(g)

  def getInteriorPoint: Coordinate = interiorPoint

  /**
   * Tests the interior vertices (if any) defined by a linear Geometry for the best inside point. If
   * a Geometry is not of dimension 1 it is not tested.
   *
   * @param geom
   *   the geometry to add
   */
  private def addInterior(geom: Geometry): Unit         = if (geom.isInstanceOf[LineString])
    addInterior(geom.getCoordinates)
  else if (geom.isInstanceOf[GeometryCollection]) {
    val gc = geom.asInstanceOf[GeometryCollection]
    var i  = 0
    while (i < gc.getNumGeometries) {
      addInterior(gc.getGeometryN(i))
      i += 1
    }
  }

  private def addInterior(pts: Array[Coordinate]): Unit = {
    var i = 1
    while (i < pts.length - 1) {
      add(pts(i))
      i += 1
    }
  }

  /**
   * Tests the endpoint vertices defined by a linear Geometry for the best inside point. If a
   * Geometry is not of dimension 1 it is not tested.
   *
   * @param geom
   *   the geometry to add
   */
  private def addEndpoints(geom: Geometry): Unit         = if (geom.isInstanceOf[LineString])
    addEndpoints(geom.getCoordinates)
  else if (geom.isInstanceOf[GeometryCollection]) {
    val gc = geom.asInstanceOf[GeometryCollection]
    var i  = 0
    while (i < gc.getNumGeometries) {
      addEndpoints(gc.getGeometryN(i))
      i += 1
    }
  }

  private def addEndpoints(pts: Array[Coordinate]): Unit = {
    add(pts(0))
    add(pts(pts.length - 1))
  }

  private def add(point: Coordinate): Unit               = {
    val dist = point.distance(centroid)
    if (dist < minDistance) {
      interiorPoint = new Coordinate(point)
      minDistance = dist
    }
  }
}
