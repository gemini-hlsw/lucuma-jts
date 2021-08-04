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
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryCollection
import org.locationtech.jts.geom.GeometryFilter
import org.locationtech.jts.geom.Polygon

/**
 * Extracts all the {link Polygon} elements from a {link Geometry}.
 *
 * @version 1.7
 * @see GeometryExtracter
 */
object PolygonExtracter {

  /**
   * Extracts the {link Polygon} elements from a single {link Geometry}
   * and adds them to the provided {link List}.
   *
   * @param geom the geometry from which to extract
   * @param list the list to add the extracted elements to
   */
  def getPolygons(geom: Geometry, list: util.List[Geometry]): util.List[Geometry] = {
    geom match {
      case _: Polygon            => list.add(geom)
      case _: GeometryCollection => geom.applyF(new PolygonExtracter(list))
      case _                     =>
    }
    // skip non-Polygonal elemental geometries
    list
  }

  /**
   * Extracts the {link Polygon} elements from a single {link Geometry}
   * and returns them in a {link List}.
   *
   * @param geom the geometry from which to extract
   */
  def getPolygons(geom: Geometry): util.List[Geometry] =
    getPolygons(geom, new util.ArrayList[Geometry])
}

class PolygonExtracter(var comps: util.List[Geometry])

/**
 * Constructs a PolygonExtracterFilter with a list in which to store Polygons found.
 */
    extends GeometryFilter {
  override def filter(geom: Geometry): Unit = {
    if (geom.isInstanceOf[Polygon]) comps.add(geom)
    ()
  }
}
