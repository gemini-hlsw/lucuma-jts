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
   */
  def extract(geom: Geometry, clz: Class[_], list: util.List[Geometry]): util.List[Geometry] = {
    if (isOfClass(geom, clz)) list.add(geom)
    else if (geom.isInstanceOf[GeometryCollection]) geom.applyF(new GeometryExtracter(clz, list))
    // skip non-LineString elemental geometries
    list
  }

  /**
   * Extracts the components of type <tt>clz</tt> from a {link Geometry} and returns them in a {link
   * List}.
   *
   * @param geom
   *   the geometry from which to extract
   */
  def extract(geom: Geometry, clz: Class[_]): util.List[Geometry] =
    extract(geom, clz, new util.ArrayList[Geometry])
}

class GeometryExtracter(var clz: Class[_], var comps: util.List[Geometry])

/**
 * Constructs a filter with a list in which to store the elements found.
 *
 * @param clz   the class of the components to extract (null means all types)
 * @param comps the list to extract into
 */ extends GeometryFilter {
  override def filter(geom: Geometry): Unit = {
    if (clz == null || GeometryExtracter.isOfClass(geom, clz)) comps.add(geom)
    ()
  }
}
