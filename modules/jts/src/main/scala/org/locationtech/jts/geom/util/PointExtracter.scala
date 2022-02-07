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
package org.locationtech.jts.geom.util

import java.util
import java.util.Collections
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryCollection
import org.locationtech.jts.geom.GeometryFilter
import org.locationtech.jts.geom.Point

/**
 * Extracts all the 0-dimensional ({link Point}) components from a {link Geometry}.
 *
 * @version 1.7
 * @see
 *   GeometryExtracter
 */
object PointExtracter {

  /**
   * Extracts the {link Point} elements from a single {link Geometry} and adds them to the provided
   * {link List}.
   *
   * @param geom
   *   the geometry from which to extract
   * @param list
   *   the list to add the extracted elements to
   */
  def getPoints(geom: Geometry, list: util.List[Geometry]): util.List[Geometry] = {
    geom match {
      case _: Point              => list.add(geom)
      case _: GeometryCollection => geom.applyF(new PointExtracter(list))
      case _                     =>
    }
    // skip non-Polygonal elemental geometries
    list
  }

  /**
   * Extracts the {link Point} elements from a single {link Geometry} and returns them in a {link
   * List}.
   *
   * @param geom
   *   the geometry from which to extract
   */
  def getPoints(geom: Geometry): util.List[Geometry] = {
    if (geom.isInstanceOf[Point]) return Collections.singletonList(geom)
    getPoints(geom, new util.ArrayList[Geometry])
  }
}

class PointExtracter(var pts: util.List[Geometry])

/**
 * Constructs a PointExtracterFilter with a list in which to store Points found.
 */ extends GeometryFilter {
  override def filter(geom: Geometry): Unit = {
    if (geom.isInstanceOf[Point]) pts.add(geom)
    ()
  }
}
