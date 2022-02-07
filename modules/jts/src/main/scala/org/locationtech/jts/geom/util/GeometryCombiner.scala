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
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryFactory

/**
 * Combines {link Geometry}s to produce a {link GeometryCollection} of the most appropriate type.
 * Input geometries which are already collections will have their elements extracted first. No
 * validation of the result geometry is performed. (The only case where invalidity is possible is
 * where {link Polygonal} geometries are combined and result in a self-intersection).
 *
 * @author
 *   mbdavis
 * @see
 *   GeometryFactory#buildGeometry
 */
object GeometryCombiner {

  /**
   * Combines a collection of geometries.
   *
   * @param geoms
   *   the geometries to combine return the combined geometry
   */
  def combine(geoms: util.Collection[Geometry]): Geometry = {
    val combiner = new GeometryCombiner(geoms)
    combiner.combine
  }

  /**
   * Combines two geometries.
   *
   * @param g0
   *   a geometry to combine
   * @param g1
   *   a geometry to combine return the combined geometry
   */
  def combine(g0: Geometry, g1: Geometry): Geometry = {
    val combiner = new GeometryCombiner(createList(g0, g1))
    combiner.combine
  }

  /**
   * Combines three geometries.
   *
   * @param g0
   *   a geometry to combine
   * @param g1
   *   a geometry to combine
   * @param g2
   *   a geometry to combine return the combined geometry
   */
  def combine(g0: Geometry, g1: Geometry, g2: Geometry): Geometry = {
    val combiner = new GeometryCombiner(createList(g0, g1, g2))
    combiner.combine
  }

  /**
   * Creates a list from two items
   *
   * @param obj0
   * @param obj1
   *   return a List containing the two items
   */
  private def createList(obj0: Geometry, obj1: Geometry) = {
    val list = new util.ArrayList[Geometry]
    list.add(obj0)
    list.add(obj1)
    list
  }

  private def createList(obj0: Geometry, obj1: Geometry, obj2: Geometry) = {
    val list = new util.ArrayList[Geometry]
    list.add(obj0)
    list.add(obj1)
    list.add(obj2)
    list
  }

  /**
   * Extracts the GeometryFactory used by the geometries in a collection
   *
   * @param geoms
   *   return a GeometryFactory
   */
  def extractFactory(geoms: util.Collection[Geometry]): GeometryFactory = {
    if (geoms.isEmpty) return null
    geoms.iterator.next.getFactory
  }
}

class GeometryCombiner(var inputGeoms: util.Collection[Geometry]) {

  /**
   * Creates a new combiner for a collection of geometries
   *
   * @param geoms
   *   the geometries to combine
   */
  private val geomFactory = GeometryCombiner.extractFactory(inputGeoms)
  private val skipEmpty   = false

  /**
   * Computes the combination of the input geometries to produce the most appropriate {link
   * Geometry} or {link GeometryCollection}
   *
   * return a Geometry which is the combination of the inputs
   */
  def combine: Geometry = {
    val elems = new util.ArrayList[Geometry]
    val i     = inputGeoms.iterator
    while (i.hasNext) {
      val g = i.next
      extractElements(g, elems)
    }
    if (elems.size == 0) {
      if (geomFactory != null) { // return an empty GC
        return geomFactory.createGeometryCollection
      }
      return null
    }
    // return the "simplest possible" geometry
    geomFactory.buildGeometry(elems)
  }

  private def extractElements(geom: Geometry, elems: util.List[Geometry]): Unit = {
    if (geom == null) return
    var i = 0
    while (i < geom.getNumGeometries) {
      val elemGeom = geom.getGeometryN(i)
      if (!(skipEmpty && elemGeom.isEmpty)) {
        elems.add(elemGeom)
      }
      i += 1
    }
  }
}
