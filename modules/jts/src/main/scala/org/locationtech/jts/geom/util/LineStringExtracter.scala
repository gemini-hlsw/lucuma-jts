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

import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryCollection
import org.locationtech.jts.geom.GeometryFilter
import org.locationtech.jts.geom.LineString

import java.util
import scala.jdk.CollectionConverters._

/**
 * Extracts all the {link LineString} elements from a {link Geometry}.
 *
 * @version 1.7
 * @see
 *   GeometryExtracter
 */
object LineStringExtracter {

  /**
   * Extracts the {link LineString} elements from a single {link Geometry} and adds them to the
   * provided {link List}.
   *
   * @param geom
   *   the geometry from which to extract
   * @param lines
   *   the list to add the extracted LineStrings to return the list argument
   */
  def getLines(geom: Geometry, lines: util.List[LineString]): util.List[LineString] = {
    geom match {
      case geom: LineString         => lines.add(geom)
      case geom: GeometryCollection => geom.applyF(new LineStringExtracter(lines))
      case _                        =>
    }
    // skip non-LineString elemental geometries
    lines
  }

  /**
   * Extracts the {link LineString} elements from a single {link Geometry} and returns them in a
   * {link List}.
   *
   * @param geom
   *   the geometry from which to extract return a list containing the linear elements
   */
  def getLines(geom: Geometry): util.List[LineString] =
    getLines(geom, new util.ArrayList[LineString])

  /**
   * Extracts the {link LineString} elements from a single {link Geometry} and returns them as
   * either a {link LineString} or {link MultiLineString}.
   *
   * @param geom
   *   the geometry from which to extract return a linear geometry
   */
  def getGeometry(geom: Geometry): Geometry =
    geom.getFactory.buildGeometry(getLines(geom).asScala.collect { case g: Geometry =>
      g: Geometry
    }.asJavaCollection)
}

class LineStringExtracter(var comps: util.List[LineString])

/**
 * Constructs a filter with a list in which to store the elements found.
 */
    extends GeometryFilter {
  override def filter(geom: Geometry): Unit = {
    if (geom.isInstanceOf[LineString]) comps.add(geom.asInstanceOf[LineString])
    ()
  }
}
