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
package org.locationtech.jts.geom.util

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryComponentFilter
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.Point

import java.util

/**
 * Extracts a representative {link Coordinate} from each connected component of a {link Geometry}.
 *
 * @version 1.9
 */
object ComponentCoordinateExtracter {

  /**
   * Extracts a representative {link Coordinate} from each connected component in a geometry. <p> If
   * more than one geometry is to be processed, it is more efficient to create a single {link
   * ComponentCoordinateExtracter} instance and pass it to each geometry.
   *
   * @param geom
   *   the Geometry from which to extract return a list of representative Coordinates
   */
  def getCoordinates(geom: Geometry): util.ArrayList[Coordinate] = {
    val coords = new util.ArrayList[Coordinate]
    geom.applyF(new ComponentCoordinateExtracter(coords))
    coords
  }
}

class ComponentCoordinateExtracter(coords: util.List[Coordinate])

/**
 * Constructs a LineExtracterFilter with a list in which to store LineStrings found.
 */
    extends GeometryComponentFilter {
  override def filter(geom: Geometry): Unit = { // add coordinates from connected components
    if (geom.isInstanceOf[LineString] || geom.isInstanceOf[Point]) coords.add(geom.getCoordinate)
    ()
  }
}
