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
package org.locationtech.jts.geom

import org.locationtech.jts.algorithm.Length
import org.locationtech.jts.operation.BoundaryOp

/**
 * Models an OGC-style <code>LineString</code>. A LineString consists of a sequence of two or more
 * vertices, along with all points along the linearly-interpolated curves (line segments) between
 * each pair of consecutive vertices. Consecutive vertices may be equal. The line segments in the
 * line may intersect each other (in other words, the linestring may "curl back" in itself and
 * self-intersect. Linestrings with exactly two identical points are invalid. <p> A linestring must
 * have either 0 or 2 or more points. If these conditions are not met, the constructors throw an
 * {link IllegalArgumentException}
 *
 * @version 1.7
 */
@SerialVersionUID(3110669828065365560L)
class LineString(fac: GeometryFactory) extends Geometry(fac) with Lineal {

  /**
   * The points of this <code>LineString</code>.
   */
  protected var points: CoordinateSequence = null

  /** @deprecated Use GeometryFactory instead */
  def this(points: Array[Coordinate], precisionModel: PrecisionModel, SRID: Int) = {
    this(new GeometryFactory(precisionModel, SRID))
    init(getFactory.getCoordinateSequenceFactory.create(points))
  }

  /**
   * Constructs a <code>LineString</code> with the given points.
   *
   * @param points
   *   the points of the linestring, or <code>null</code> to create the empty geometry. throws
   *   IllegalArgumentException if too few points are provided
   */
  def this(points: CoordinateSequence, factory: GeometryFactory) = {
    this(factory)
    init(points)
  }

  private def init(point: CoordinateSequence): Unit = {
    if (point == null)
      points = getFactory.getCoordinateSequenceFactory.create(Array.empty[Coordinate])
    if (point.size == 1)
      throw new IllegalArgumentException(
        "Invalid number of points in LineString (found " + points.size + " - must be 0 or >= 2)"
      )
    this.points = point
  }

  override def getCoordinates: Array[Coordinate] = points.toCoordinateArray

  def getCoordinateSequence: CoordinateSequence = points

  def getCoordinateN(n: Int): Coordinate = points.getCoordinate(n)

  override def getCoordinate: Coordinate = {
    if (isEmpty) return null
    points.getCoordinate(0)
  }

  override def getDimension = 1

  override def getBoundaryDimension: Int = {
    if (isClosed) return Dimension.FALSE
    0
  }

  override def isEmpty: Boolean = points.size == 0

  override def getNumPoints: Int = points.size

  def getPointN(n: Int): Point = getFactory.createPoint(points.getCoordinate(n))

  def getStartPoint: Point = {
    if (isEmpty) return null
    getPointN(0)
  }

  def getEndPoint: Point = {
    if (isEmpty) return null
    getPointN(getNumPoints - 1)
  }

  def isClosed: Boolean = {
    if (isEmpty) return false
    getCoordinateN(0).equals2D(getCoordinateN(getNumPoints - 1))
  }

  def isRing: Boolean = isClosed && isSimple

  override def getGeometryType = Geometry.TYPENAME_LINESTRING

  /**
   * Returns the length of this <code>LineString</code>
   *
   * return the length of the linestring
   */
  override def getLength: Double = Length.ofLine(points)

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
   * Creates a {link LineString} whose coordinates are in the reverse order of this objects
   *
   * return a { @link LineString} with coordinates in the reverse order
   * @deprecated
   */
  override def reverse: LineString = super.reverse.asInstanceOf[LineString]

  override protected def reverseInternal: LineString = {
    val seq = points.copy
    CoordinateSequences.reverse(seq)
    getFactory.createLineString(seq)
  }

  /**
   * Returns true if the given point is a vertex of this <code>LineString</code>.
   *
   * @param pt
   *   the <code>Coordinate</code> to check return <code>true</code> if <code>pt</code> is one of
   *   this <code>LineString</code> 's vertices
   */
  def isCoordinate(pt: Coordinate): Boolean = {
    var i = 0
    while (i < points.size) {
      if (points.getCoordinate(i) == pt) return true
      i += 1
    }
    false
  }

  override protected def computeEnvelopeInternal: Envelope = {
    if (isEmpty) return new Envelope
    points.expandEnvelope(new Envelope)
  }

  override def equalsExact(other: Geometry, tolerance: Double): Boolean = {
    if (!isEquivalentClass(other)) return false
    val otherLineString = other.asInstanceOf[LineString]
    if (points.size != otherLineString.points.size) return false
    var i               = 0
    while (i < points.size) {
      if (!equal(points.getCoordinate(i), otherLineString.points.getCoordinate(i), tolerance))
        return false
      i += 1
    }
    true
  }

  def applyF(filter: CoordinateFilter): Unit = {
    var i = 0
    while (i < points.size) {
      filter.filter(points.getCoordinate(i))
      i += 1
    }
  }

  def applyF(filter: CoordinateSequenceFilter): Unit = {
    if (points.size == 0) return
    var i = 0
    while (i < points.size) {
      filter.filter(points, i)
      if (filter.isDone) {
        i = points.size
      } else {
        i += 1
      }
    }
    if (filter.isGeometryChanged) geometryChanged()
  }

  def applyF(filter: GeometryFilter): Unit = filter.filter(this)

  def applyF(filter: GeometryComponentFilter): Unit = filter.filter(this)

  /**
   * Creates and returns a full copy of this {link LineString} object. (including all coordinates
   * contained by it).
   *
   * return a clone of this instance
   * @deprecated
   */
  override def clone: AnyRef = copy

  override protected def copyInternal: Geometry = new LineString(points.copy, factory)

  /**
   * Normalizes a LineString. A normalized linestring has the first point which is not equal to it's
   * reflected point less than the reflected point.
   */
  override def normalize(): Unit = {
    var i = 0
    while (i < points.size / 2) {
      val j = points.size - 1 - i
      // skip equal points on both ends
      if (!(points.getCoordinate(i) == points.getCoordinate(j))) {
        if (points.getCoordinate(i).compareTo(points.getCoordinate(j)) > 0) {
          val copy = points.copy
          CoordinateSequences.reverse(copy)
          points = copy
        }
        return
      }
      i += 1
    }
  }

  override protected def isEquivalentClass(other: Geometry): Boolean =
    other.isInstanceOf[LineString]

  protected def compareToSameClass(o: Geometry): Int = {
    val line = o.asInstanceOf[LineString]
    // MD - optimized implementation
    var i    = 0
    var j    = 0
    while (i < points.size && j < line.points.size) {
      val comparison = points.getCoordinate(i).compareTo(line.points.getCoordinate(j))
      if (comparison != 0) return comparison
      i += 1
      j += 1
    }
    if (i < points.size) return 1
    if (j < line.points.size) return -1
    0
  }

  def compareToSameClass(o: Geometry, comp: CoordinateSequenceComparator): Int = {
    val line = o.asInstanceOf[LineString]
    comp.compare(this.points, line.points)
  }

  override protected def getTypeCode: Int = Geometry.TYPECODE_LINESTRING
}
