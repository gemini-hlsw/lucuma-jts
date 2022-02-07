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

/**
 * Models a collection of {link Point}s. <p> Any collection of Points is a valid MultiPoint.
 *
 * @version 1.7
 */
@SerialVersionUID(-8048474874175355449L)
class MultiPoint(points: Array[Point], factory: GeometryFactory)
    extends GeometryCollection(points.map(x => x: Geometry), factory)
    with Puntal {

  /**
   * Constructs a <code>MultiPoint</code>.
   *
   * @param points
   *   the <code>Point</code>s for this <code>MultiPoint</code> , or <code>null</code> or an empty
   *   array to create the empty geometry. Elements may be empty <code>Point</code>s, but not
   *   <code>null</code>s.
   * @param precisionModel
   *   the specification of the grid of allowable points for this <code>MultiPoint</code>
   * @param SRID
   *   the ID of the Spatial Reference System used by this <code>MultiPoint</code>
   * @deprecated
   *   Use GeometryFactory instead
   */
  def this(points: Array[Point], precisionModel: PrecisionModel, SRID: Int) =
    this(points, new GeometryFactory(precisionModel, SRID))

  /**
   * @param points
   *   the <code>Point</code>s for this <code>MultiPoint</code> , or <code>null</code> or an empty
   *   array to create the empty geometry. Elements may be empty <code>Point</code>s, but not
   *   <code>null</code>s.
   */
//  def this(points: Array[Point], factory: GeometryFactory) = {
//    this()
//    super (points, factory)
//  }

  override def getDimension = 0

  override def getBoundaryDimension: Int = Dimension.FALSE

  override def getGeometryType = "MultiPoint"

  /**
   * Gets the boundary of this geometry. Zero-dimensional geometries have no boundary by definition,
   * so an empty GeometryCollection is returned.
   *
   * return an empty GeometryCollection
   * @see
   *   Geometry#getBoundary
   */
  override def getBoundary: GeometryCollection = getFactory.createGeometryCollection

  override def isValid = true

  override def equalsExact(other: Geometry, tolerance: Double): Boolean = {
    if (!isEquivalentClass(other)) return false
    super.equalsExact(other, tolerance)
  }

  /**
   * Returns the <code>Coordinate</code> at the given position.
   *
   * @param n
   *   the index of the <code>Coordinate</code> to retrieve, beginning at 0 return the
   *   <code>n</code>th <code>Coordinate</code>
   */
  protected def getCoordinate(n: Int): Coordinate = geometries(n).asInstanceOf[Point].getCoordinate

  override protected def copyInternal: MultiPoint = {
    val points = new Array[Point](this.geometries.length)
    var i      = 0
    while (i < points.length) {
      points(i) = this.geometries(i).copy.asInstanceOf[Point]
      i += 1
    }
    new MultiPoint(points, factory)
  }

  override protected def getSortIndex: Int = Geometry.SORTINDEX_MULTIPOINT
}
