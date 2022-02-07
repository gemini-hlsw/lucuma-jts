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
package org.locationtech.jts.operation.distance

import java.util
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryFilter
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.Point
import org.locationtech.jts.geom.Polygon

/**
 * A ConnectedElementPointFilter extracts a single point from each connected element in a Geometry
 * (e.g. a polygon, linestring or point) and returns them in a list. The elements of the list are
 * {link org.locationtech.jts.operation.distance.GeometryLocation}s. Empty geometries do not provide
 * a location item.
 *
 * @version 1.7
 */
object ConnectedElementLocationFilter {

  /**
   * Returns a list containing a point from each Polygon, LineString, and Point found inside the
   * specified geometry. Thus, if the specified geometry is not a GeometryCollection, an empty list
   * will be returned. The elements of the list are {link
   * org.locationtech.jts.operation.distance.GeometryLocation}s.
   */
  def getLocations(geom: Geometry): util.ArrayList[GeometryLocation] = {
    val locations = new util.ArrayList[GeometryLocation]
    geom.applyF(new ConnectedElementLocationFilter(locations))
    locations
  }
}

class ConnectedElementLocationFilter private[distance] (var locations: util.List[GeometryLocation])
    extends GeometryFilter {
  override def filter(geom: Geometry): Unit = { // empty geometries do not provide a location
    if (geom.isEmpty) return
    if (geom.isInstanceOf[Point] || geom.isInstanceOf[LineString] || geom.isInstanceOf[Polygon])
      locations.add(new GeometryLocation(geom, 0, geom.getCoordinate))
    ()
  }
}
