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
 */ /*
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

import org.locationtech.jts.operation.BoundaryOp

/**
 * Models a collection of {link LineString}s. <p> Any collection of LineStrings is a valid
 * MultiLineString.
 *
 * @version 1.7
 */
@SerialVersionUID(8166665132445433741L)
class MultiLineString(lineStrings: Array[LineString], factory: GeometryFactory)
    extends GeometryCollection(lineStrings.map(x => x: Geometry), factory)
    with Lineal {

  /**
   * Constructs a <code>MultiLineString</code>.
   *
   * @param lineStrings
   *   the <code>LineString</code>s for this <code>MultiLineString</code> , or <code>null</code> or
   *   an empty array to create the empty geometry. Elements may be empty <code>LineString</code>s,
   *   but not <code>null</code> s.
   * @param precisionModel
   *   the specification of the grid of allowable points for this <code>MultiLineString</code>
   * @param SRID
   *   the ID of the Spatial Reference System used by this <code>MultiLineString</code>
   * @deprecated
   *   Use GeometryFactory instead
   */
  def this(lineStrings: Array[LineString], precisionModel: PrecisionModel, SRID: Int) =
    this(lineStrings, new GeometryFactory(precisionModel, SRID))

  /**
   * @param lineStrings
   *   the <code>LineString</code>s for this <code>MultiLineString</code>, or <code>null</code> or
   *   an empty array to create the empty geometry. Elements may be empty <code>LineString</code>s,
   *   but not <code>null</code>s.
   */
//  def this(lineStrings: Array[LineString], factory: GeometryFactory) {
//    this()
//    super (lineStrings, factory)
//  }

  override def getDimension = 1

  override def getBoundaryDimension: Int = {
    if (isClosed) return Dimension.FALSE
    0
  }

  override def getGeometryType = "MultiLineString"

  def isClosed: Boolean = {
    if (isEmpty) return false
    var i = 0
    while (i < geometries.length) {
      if (!(geometries(i).asInstanceOf[LineString]).isClosed) return false
      i += 1
    }
    true
  }

  /**
   * Gets the boundary of this geometry. The boundary of a lineal geometry is always a
   * zero-dimensional geometry (which may be empty).
   *
   * return the boundary geometry
   * @see
   *   Geometry#getBoundary
   */
  override def getBoundary: Geometry = new BoundaryOp(this).getBoundary

  /**
   * Creates a {link MultiLineString} in the reverse order to this object. Both the order of the
   * component LineStrings and the order of their coordinate sequences are reversed.
   *
   * return a { @link MultiLineString} in the reverse order
   * @deprecated
   */
  override def reverse: Geometry = super.reverse

  override protected def copyInternal: MultiLineString = {
    val lineStrings = new Array[LineString](this.geometries.length)
    var i           = 0
    while (i < lineStrings.length) {
      lineStrings(i) = this.geometries(i).copy.asInstanceOf[LineString]
      i += 1
    }
    new MultiLineString(lineStrings, factory)
  }

  override def equalsExact(other: Geometry, tolerance: Double): Boolean = {
    if (!isEquivalentClass(other)) return false
    super.equalsExact(other, tolerance)
  }

  override protected def getSortIndex: Int = Geometry.SORTINDEX_MULTILINESTRING
}
