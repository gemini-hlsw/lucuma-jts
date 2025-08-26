// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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

import org.locationtech.jts.util.Assert

/**
 * Models a collection of {link Geometry}s of arbitrary type and dimension.
 *
 * @version 1.7
 */
@SerialVersionUID(-5694727726395021467L)
class GeometryCollection(
  /**
   * Internal representation of this <code>GeometryCollection</code>.
   */
  var geometries:       Array[Geometry],
  override val factory: GeometryFactory
)

// /**
//  * @param geometries
//  *            the <code>Geometry</code>s for this <code>GeometryCollection</code>,
//  *            or <code>null</code> or an empty array to create the empty
//  *            geometry. Elements may be empty <code>Geometry</code>s,
//  *            but not <code>null</code>s.
//  */
    extends Geometry(factory) {
  if (geometries == null) geometries = Array.empty[Geometry]
  if (Geometry.hasNullElements(geometries.map(x => x: AnyRef)))
    throw new IllegalArgumentException("geometries must not contain null elements")

  /** @deprecated Use GeometryFactory instead */
  def this(geometries: Array[Geometry], precisionModel: PrecisionModel, SRID: Int) =
    this(geometries, new GeometryFactory(precisionModel, SRID))

  override def getCoordinate: Coordinate = {
    if (isEmpty) return null
    geometries(0).getCoordinate
  }

  /**
   * Collects all coordinates of all subgeometries into an Array.
   *
   * Note that while changes to the coordinate objects themselves may modify the Geometries in
   * place, the returned Array as such is only a temporary container which is not synchronized back.
   *
   * return the collected coordinates
   */
  override def getCoordinates: Array[Coordinate] = {
    val coordinates = new Array[Coordinate](getNumPoints)
    var k           = -1
    var i           = 0
    while (i < geometries.length) {
      val childCoordinates = geometries(i).getCoordinates
      var j                = 0
      while (j < childCoordinates.length) {
        k += 1
        coordinates(k) = childCoordinates(j)
        j += 1
      }
      i += 1
    }
    coordinates
  }

  override def isEmpty: Boolean = {
    var i = 0
    while (i < geometries.length) {
      if (!geometries(i).isEmpty) return false
      i += 1
    }
    true
  }

  override def getDimension: Int = {
    var dimension = Dimension.FALSE
    var i         = 0
    while (i < geometries.length) {
      dimension = Math.max(dimension, geometries(i).getDimension)
      i += 1
    }
    dimension
  }

  override def getBoundaryDimension: Int = {
    var dimension = Dimension.FALSE
    var i         = 0
    while (i < geometries.length) {
      dimension = Math.max(dimension, geometries(i).getBoundaryDimension)
      i += 1; i - 1
    }
    dimension
  }

  override def getNumGeometries: Int = geometries.length

  override def getGeometryN(n: Int): Geometry = geometries(n)

  override def getNumPoints: Int = {
    var numPoints = 0
    var i         = 0
    while (i < geometries.length) {
      numPoints += geometries(i).getNumPoints
      i += 1
    }
    numPoints
  }

  override def getGeometryType = Geometry.TYPENAME_GEOMETRYCOLLECTION

  override def getBoundary: Geometry = {
    Geometry.checkNotGeometryCollection(this)
    Assert.shouldNeverReachHere()
    null
  }

  /**
   * Returns the area of this <code>GeometryCollection</code>
   *
   * return the area of the polygon
   */
  override def getArea: Double = {
    var area = 0.0
    var i    = 0
    while (i < geometries.length) {
      area += geometries(i).getArea
      i += 1
    }
    area
  }

  override def getLength: Double = {
    var sum = 0.0
    var i   = 0
    while (i < geometries.length) {
      sum += geometries(i).getLength
      i += 1
    }
    sum
  }

  override def equalsExact(other: Geometry, tolerance: Double): Boolean = {
    if (!isEquivalentClass(other)) return false
    val otherCollection = other.asInstanceOf[GeometryCollection]
    if (geometries.length != otherCollection.geometries.length) return false
    var i               = 0
    while (i < geometries.length) {
      if (!geometries(i).equalsExact(otherCollection.geometries(i), tolerance)) return false
      i += 1
    }
    true
  }

  def applyF(filter: CoordinateFilter): Unit = {
    var i = 0
    while (i < geometries.length) {
      geometries(i).applyF(filter)
      i += 1
    }
  }

  def applyF(filter: CoordinateSequenceFilter): Unit = {
    if (geometries.isEmpty) return
    var i = 0
    while (i < geometries.length) {
      geometries(i).applyF(filter)
      if (filter.isDone) { i = geometries.length }
      else // todo: break is not supported
        i += 1
    }
    if (filter.isGeometryChanged) geometryChanged()
  }

  def applyF(filter: GeometryFilter): Unit = {
    filter.filter(this)
    var i = 0
    while (i < geometries.length) {
      geometries(i).applyF(filter)
      i += 1
    }
  }

  def applyF(filter: GeometryComponentFilter): Unit = {
    filter.filter(this)
    var i = 0
    while (i < geometries.length) {
      geometries(i).applyF(filter)
      i += 1
    }
  }

  /**
   * Creates and returns a full copy of this {link GeometryCollection} object. (including all
   * coordinates contained by it).
   *
   * return a clone of this instance
   * @deprecated
   */
  override def clone: AnyRef = copy

  override protected def copyInternal: GeometryCollection = {
    val geometries = new Array[Geometry](this.geometries.length)
    var i          = 0
    while (i < geometries.length) {
      geometries(i) = this.geometries(i).copy
      i += 1
    }
    new GeometryCollection(geometries, factory)
  }

  override def normalize(): Unit = {
    var i = 0
    while (i < geometries.length) {
      geometries(i).normalize()
      i += 1
    }
    geometries = geometries.sorted
    // java.util.Arrays.sort(geometries.map(x => x: AnyRef))
  }

  override protected def computeEnvelopeInternal: Envelope = {
    val envelope = new Envelope
    var i        = 0
    while (i < geometries.length) {
      envelope.expandToInclude(geometries(i).getEnvelopeInternal)
      i += 1
    }
    envelope
  }

  protected def compareToSameClass(o: AnyRef): Int = {
    val theseElements = new java.util.TreeSet(java.util.Arrays.asList(geometries))
    val otherElements =
      new java.util.TreeSet(java.util.Arrays.asList(o.asInstanceOf[GeometryCollection].geometries))
    // TODO this is a bug
    compare(theseElements, otherElements, null)
  }

  def compareToSameClass(o: AnyRef, comp: CoordinateSequenceComparator): Int = {
    val gc = o.asInstanceOf[GeometryCollection]
    val n1 = getNumGeometries
    val n2 = gc.getNumGeometries
    var i  = 0
    while (i < n1 && i < n2) {
      val thisGeom      = getGeometryN(i)
      val otherGeom     = gc.getGeometryN(i)
      val holeComp: Int = thisGeom.compareToSameClass(otherGeom, comp)
      if (holeComp != 0) return holeComp
      i += 1
    }
    if (i < n1) return 1
    if (i < n2) return -1
    0
  }

  override protected def getTypeCode: Int = Geometry.TYPECODE_GEOMETRYCOLLECTION

  /**
   * Creates a {link GeometryCollection} with every component reversed. The order of the components
   * in the collection are not reversed.
   *
   * return a {@link GeometryCollection} in the reverse order
   */
  override def reverse: GeometryCollection = super.reverse.asInstanceOf[GeometryCollection]

  override protected def reverseInternal: GeometryCollection = {
    val numGeometries = geometries.length
    val reversed      = new java.util.ArrayList[Geometry](numGeometries)
    var i             = 0
    while (i < numGeometries) {
      reversed.add(geometries(i).reverse)
      i += 1
    }
    getFactory.buildGeometry(reversed).asInstanceOf[GeometryCollection]
  }
}
