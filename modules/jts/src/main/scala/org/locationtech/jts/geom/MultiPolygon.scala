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
package org.locationtech.jts.geom

import java.util

/**
 * Models a collection of {link Polygon}s. <p> As per the OGC SFS specification, the Polygons in a
 * MultiPolygon may not overlap, and may only touch at single points. This allows the topological
 * point-set semantics to be well-defined.
 *
 * @version 1.7
 */
@SerialVersionUID(-551033529766975875L)
class MultiPolygon(val polygons: Array[Polygon], override val factory: GeometryFactory)

// /**
//  * @param polygons
//  *            the <code>Polygon</code>s for this <code>MultiPolygon</code>,
//  *            or <code>null</code> or an empty array to create the empty
//  *            geometry. Elements may be empty <code>Polygon</code>s, but
//  *            not <code>null</code>s. The polygons must conform to the
//  *            assertions specified in the <A
//  *            HREF="http://www.opengis.org/techno/specs.htm">OpenGIS Simple
//  *            Features Specification for SQL</A>.
//  */
    extends GeometryCollection(polygons.map(x => x: Geometry), factory)
    with Polygonal {

  /**
   * Constructs a <code>MultiPolygon</code>.
   *
   * @param polygons
   *   the <code>Polygon</code>s for this <code>MultiPolygon</code> , or <code>null</code> or an
   *   empty array to create the empty geometry. Elements may be empty <code>Polygon</code>s, but
   *   not <code>null</code> s. The polygons must conform to the assertions specified in the <A
   *   HREF="http://www.opengis.org/techno/specs.htm">OpenGIS Simple Features Specification for
   *   SQL</A> .
   * @param precisionModel
   *   the specification of the grid of allowable points for this <code>MultiPolygon</code>
   * @param SRID
   *   the ID of the Spatial Reference System used by this <code>MultiPolygon</code>
   * @deprecated
   *   Use GeometryFactory instead
   */
  def this(polygons: Array[Polygon], precisionModel: PrecisionModel, SRID: Int) =
    this(polygons, new GeometryFactory(precisionModel, SRID))

  override def getDimension = 2

  override def getBoundaryDimension = 1

  override def getGeometryType = "MultiPolygon"

  /**
   * Computes the boundary of this geometry
   *
   * return a lineal geometry (which may be empty)
   * @see
   *   Geometry#getBoundary
   */
  override def getBoundary: Geometry = {
    if (isEmpty) return getFactory.createMultiLineString
    val allRings      = new util.ArrayList[Geometry]
    var i             = 0
    while (i < geometries.length) {
      val polygon = geometries(i).asInstanceOf[Polygon]
      val rings   = polygon.getBoundary
      var j       = 0
      while (j < rings.getNumGeometries) {
        allRings.add(rings.getGeometryN(j))
        j += 1
      }
      i += 1
    }
    val allRingsArray = new Array[LineString](allRings.size)
    getFactory.createMultiLineString(allRings.toArray(allRingsArray))
  }

  override def equalsExact(other: Geometry, tolerance: Double): Boolean = {
    if (!isEquivalentClass(other)) return false
    super.equalsExact(other, tolerance)
  }

  /**
   * Creates a {link MultiPolygon} with every component reversed. The order of the components in the
   * collection are not reversed.
   *
   * return a MultiPolygon in the reverse order
   * @deprecated
   */
  override def reverse: Geometry = super.reverse

  override protected def copyInternal: MultiPolygon = {
    val polygons = new Array[Polygon](this.geometries.length)
    var i        = 0
    while (i < polygons.length) {
      polygons(i) = this.geometries(i).copy.asInstanceOf[Polygon]
      i += 1
    }
    new MultiPolygon(polygons, factory)
  }

  override protected def getSortIndex: Int = Geometry.SORTINDEX_MULTIPOLYGON
}
