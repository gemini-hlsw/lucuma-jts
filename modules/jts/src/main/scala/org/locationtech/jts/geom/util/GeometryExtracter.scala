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
import org.locationtech.jts.geom.LinearRing
import org.locationtech.jts.geom.MultiLineString
import org.locationtech.jts.geom.MultiPoint
import org.locationtech.jts.geom.MultiPolygon
import org.locationtech.jts.geom.Point
import org.locationtech.jts.geom.Polygon

import java.util

/**
 * Extracts the components of a given type from a {link Geometry}.
 *
 * @version 1.7
 */
object GeometryExtracter {
  protected def isOfClass(o: Any, clz: Class[_]): Boolean =
    clz.isAssignableFrom(o.getClass)
  //		return o.getClass() == clz;

  /**
   * Extracts the components of type <tt>clz</tt> from a {link Geometry} and adds them to the
   * provided {link List}.
   *
   * @param geom
   *   the geometry from which to extract
   * @param list
   *   the list to add the extracted elements to
   * @deprecated
   *   Use {@link GeometryExtracter#extract(Geometry, String, List)}
   */
  def extract(geom: Geometry, clz: Class[_], list: util.List[Geometry]): util.List[Geometry] =
    extract(geom, toGeometryType(clz), list)

  /**
   * @deprecated
   */
  def toGeometryType(clz: Class[_]): String = {
    if (clz == null)
      return null;
    else if (clz.isAssignableFrom(classOf[Point]))
      return Geometry.TYPENAME_POINT;
    else if (clz.isAssignableFrom(classOf[LineString]))
      return Geometry.TYPENAME_LINESTRING;
    else if (clz.isAssignableFrom(classOf[LinearRing]))
      return Geometry.TYPENAME_LINEARRING;
    else if (clz.isAssignableFrom(classOf[Polygon]))
      return Geometry.TYPENAME_POLYGON;
    else if (clz.isAssignableFrom(classOf[MultiPoint]))
      return Geometry.TYPENAME_MULTIPOINT;
    else if (clz.isAssignableFrom(classOf[MultiLineString]))
      return Geometry.TYPENAME_MULTILINESTRING;
    else if (clz.isAssignableFrom(classOf[MultiPolygon]))
      return Geometry.TYPENAME_MULTIPOLYGON;
    else if (clz.isAssignableFrom(classOf[GeometryCollection]))
      return Geometry.TYPENAME_GEOMETRYCOLLECTION;
    throw new RuntimeException("Unsupported class");
  }

  /**
   * Extracts the components of <tt>geometryType</tt> from a {@link Geometry} and adds them to the
   * provided {@link List} .
   *
   * @param geom
   *   the geometry from which to extract
   * @param geometryType
   *   Geometry type to extract (null means all types)
   * @param list
   *   the list to add the extracted elements to
   */
  def extract(
    geom:         Geometry,
    geometryType: String,
    list:         util.List[Geometry]
  ): util.List[Geometry] = {
    if (geom.getGeometryType == geometryType)
      list.add(geom)
    else if (geom.isInstanceOf[GeometryCollection])
      geom.applyF(new GeometryExtracter(geometryType, list))
    // skip non-LineString elemental geometries
    list
  }

  def extract(geom: Geometry, geometryType: String): util.List[Geometry] =
    extract(geom, geometryType, new util.ArrayList());

  /**
   * Extracts the components of type <tt>clz</tt> from a {link Geometry} and returns them in a {link
   * List}.
   *
   * @param geom
   *   the geometry from which to extract
   * @deprecated
   *   Use {@link GeometryExtracter#extract(Geometry, String)}
   */
  def extract(geom: Geometry, clz: Class[_]): util.List[Geometry] =
    extract(geom, clz, new util.ArrayList[Geometry])

  def isOfType(geom: Geometry, geometryType: String): Boolean = {
    if (geom.getGeometryType == geometryType) return true
    if (
      geometryType == Geometry.TYPENAME_LINESTRING
      && geom.getGeometryType == Geometry.TYPENAME_LINEARRING
    ) return true
    return false
  }
}

class GeometryExtracter(var geometryType: String, var comps: util.List[Geometry])

/**
 * Constructs a filter with a list in which to store the elements found.
 *
 * @param clz
 *   the class of the components to extract (null means all types)
 * @param comps
 *   the list to extract into
 */
    extends GeometryFilter {
  override def filter(geom: Geometry): Unit = {
    if (geometryType == null || GeometryExtracter.isOfType(geom, geometryType)) comps.add(geom)
    ()
  }
}
