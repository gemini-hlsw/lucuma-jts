// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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
package org.locationtech.jts.operation.distance

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryFilter
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.Point
import org.locationtech.jts.geom.Polygon

import java.util

/**
 * Extracts a single point from each connected element in a Geometry (e.g. a polygon, linestring or
 * point) and returns them in a list
 *
 * @version 1.7
 */
object ConnectedElementPointFilter {

  /**
   * Returns a list containing a Coordinate from each Polygon, LineString, and Point found inside
   * the specified geometry. Thus, if the specified geometry is not a GeometryCollection, an empty
   * list will be returned.
   */
  def getCoordinates(geom: Geometry): util.ArrayList[Coordinate] = {
    val pts = new util.ArrayList[Coordinate]
    geom.applyF(new ConnectedElementPointFilter(pts))
    pts
  }
}

class ConnectedElementPointFilter private[distance] (var pts: util.List[Coordinate])
    extends GeometryFilter {
  override def filter(geom: Geometry): Unit = {
    if (geom.isInstanceOf[Point] || geom.isInstanceOf[LineString] || geom.isInstanceOf[Polygon])
      pts.add(geom.getCoordinate)
    ()
  }
}
