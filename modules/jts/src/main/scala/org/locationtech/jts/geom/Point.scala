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

import org.locationtech.jts.util.Assert

/**
 * Represents a single point.
 *
 * A <code>Point</code> is topologically valid if and only if:
 * <ul>
 * <li>the coordinate which defines it (if any) is a valid coordinate
 * (i.e. does not have an <code>NaN</code> X or Y ordinate)
 * </ul>
 *
 * @version 1.7
 */
@SerialVersionUID(4902022702746614570L)
class Point(factory: GeometryFactory) extends Geometry(factory) with Puntal {

  /**
   * The <code>Coordinate</code> wrapped by this <code>Point</code>.
   */
  private var coordinates: CoordinateSequence = null

  /**
   * Constructs a <code>Point</code> with the given coordinate.
   *
   * @param  coordinate     the coordinate on which to base this <code>Point</code>
   *                        , or <code>null</code> to create the empty geometry.
   * @param  precisionModel the specification of the grid of allowable points
   *                        for this <code>Point</code>
   * @param  SRID           the ID of the Spatial Reference System used by this
   *                        <code>Point</code>
   * @deprecated Use GeometryFactory instead
   */
  def this(coordinate: Coordinate, precisionModel: PrecisionModel, SRID: Int) = {
    this(new GeometryFactory(precisionModel, SRID))
    init(
      getFactory.getCoordinateSequenceFactory.create(
        if (coordinate != null) Array[Coordinate](coordinate)
        else Array.empty[Coordinate]
      )
    )
  }

  /**
   * @param  coordinates contains the single coordinate on which to base this <code>Point</code>
   *                     , or <code>null</code> to create the empty geometry.
   */
  def this(coordinates: CoordinateSequence, factory: GeometryFactory) = {
    this(factory)
    init(coordinates)
  }

  private def init(coordinates: CoordinateSequence): Unit = {
    val coord: CoordinateSequence =
      if (coordinates == null) getFactory.getCoordinateSequenceFactory.create(Array[Coordinate]())
      else coordinates
    Assert.isTrue(coord.size <= 1)
    this.coordinates = coord
  }

  override def getCoordinates: Array[Coordinate] = if (isEmpty) Array.empty[Coordinate]
  else Array[Coordinate](getCoordinate)

  override def getNumPoints: Int = if (isEmpty) 0
  else 1

  override def isEmpty: Boolean = coordinates.size == 0

  override def isSimple = true

  override def getDimension = 0

  override def getBoundaryDimension: Int = Dimension.FALSE

  def getX: Double = {
    if (getCoordinate == null) throw new IllegalStateException("getX called on empty Point")
    getCoordinate.x
  }

  def getY: Double = {
    if (getCoordinate == null) throw new IllegalStateException("getY called on empty Point")
    getCoordinate.y
  }

  override def getCoordinate: Coordinate = if (coordinates.size != 0) coordinates.getCoordinate(0)
  else null

  override def getGeometryType = "Point"

  /**
   * Gets the boundary of this geometry.
   * Zero-dimensional geometries have no boundary by definition,
   * so an empty GeometryCollection is returned.
   *
   * return an empty GeometryCollection
   * @see Geometry#getBoundary
   */
  override def getBoundary: GeometryCollection = getFactory.createGeometryCollection

  override protected def computeEnvelopeInternal: Envelope = {
    if (isEmpty) return new Envelope
    val env = new Envelope
    env.expandToInclude(coordinates.getX(0), coordinates.getY(0))
    env
  }

  override def equalsExact(other: Geometry, tolerance: Double): Boolean = {
    if (!isEquivalentClass(other)) return false
    if (isEmpty && other.isEmpty) return true
    if (isEmpty != other.isEmpty) return false
    equal(other.asInstanceOf[Point].getCoordinate, this.getCoordinate, tolerance)
  }

  override def applyF(filter: CoordinateFilter): Unit = {
    if (isEmpty) return
    filter.filter(getCoordinate)
  }

  override def applyF(filter: CoordinateSequenceFilter): Unit = {
    if (isEmpty) return
    filter.filter(coordinates, 0)
    if (filter.isGeometryChanged) geometryChanged()
  }

  override def applyF(filter: GeometryFilter): Unit = filter.filter(this)

  override def applyF(filter: GeometryComponentFilter): Unit = filter.filter(this)

  /**
   * Creates and returns a full copy of this {link Point} object.
   * (including all coordinates contained by it).
   *
   * return a clone of this instance
   * @deprecated
   */
  override def clone: AnyRef = copy

  override protected def copyInternal = new Point(coordinates.copy, factory)

  /** @deprecated */
  override def reverse: Geometry = super.reverse

  override protected def reverseInternal: Point = getFactory.createPoint(coordinates.copy)

  override def normalize(): Unit = {
    // a Point is always in normalized form
  }

  override protected def compareToSameClass(other: Geometry): Int = {
    val point = other.asInstanceOf[Point]
    getCoordinate.compareTo(point.getCoordinate)
  }

  def compareToSameClass(other: Geometry, comp: CoordinateSequenceComparator): Int = {
    val point = other.asInstanceOf[Point]
    comp.compare(this.coordinates, point.coordinates)
  }

  override protected def getSortIndex: Int = Geometry.SORTINDEX_POINT

  def getCoordinateSequence: CoordinateSequence = coordinates
}
