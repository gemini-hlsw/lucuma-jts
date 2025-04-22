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

import org.locationtech.jts.algorithm.Area
import org.locationtech.jts.algorithm.Orientation

/**
 * Represents a polygon with linear edges, which may include holes. The outer boundary (shell) and
 * inner boundaries (holes) of the polygon are represented by {link LinearRing}s. The boundary rings
 * of the polygon may have any orientation. Polygons are closed, simple geometries by definition.
 * <p> The polygon model conforms to the assertions specified in the <A
 * HREF="http://www.opengis.org/techno/specs.htm">OpenGIS Simple Features Specification for SQL</A>.
 * <p> A <code>Polygon</code> is topologically valid if and only if: <ul> <li>the coordinates which
 * define it are valid coordinates <li>the linear rings for the shell and holes are valid (i.e. are
 * closed and do not self-intersect) <li>holes touch the shell or another hole at at most one point
 * (which implies that the rings of the shell and holes must not cross) <li>the interior of the
 * polygon is connected, or equivalently no sequence of touching holes makes the interior of the
 * polygon disconnected (i.e. effectively split the polygon into two pieces). </ul>
 *
 * @version 1.7
 */
@SerialVersionUID(-3494792200821764533L)
class Polygon(
  val shellArg:         LinearRing,
  // /**
  //  * The interior boundaries, if any.
  //  * This instance var is never null.
  //  * If there are no holes, the array is of zero length.
  //  */
  var holes:            Array[LinearRing],
  override val factory: GeometryFactory
)

// /**
//  * Constructs a <code>Polygon</code> with the given exterior boundary and
//  * interior boundaries.
//  *
//  * @param  shell the outer boundary of the new <code>Polygon</code>,
//  *               or <code>null</code> or an empty <code>LinearRing</code> if the empty
//  *               geometry is to be created.
//  * @param  holes the inner boundaries of the new <code>Polygon</code>
//  *               , or <code>null</code> or empty <code>LinearRing</code>s if the empty
//  *               geometry is to be created.
//  */
    extends Geometry(factory)
    with Polygonal {
  var shell: LinearRing = if (shellArg == null) getFactory.createLinearRing else shellArg
  if (holes == null) holes = Array.empty[LinearRing]
  if (Geometry.hasNullElements(holes.map(x => x: Geometry)))
    throw new IllegalArgumentException("holes must not contain null elements")
  if (shell.isEmpty && Geometry.hasNonEmptyElements(holes.map(x => x: Geometry)))
    throw new IllegalArgumentException("shell is empty but holes are not")

  /**
   * The exterior boundary, or <code>null</code> if this <code>Polygon</code> is empty.
   */
//  protected var shell = null

  /**
   * Constructs a <code>Polygon</code> with the given exterior boundary.
   *
   * @param shell
   *   the outer boundary of the new <code>Polygon</code>, or <code>null</code> or an empty
   *   <code>LinearRing</code> if the empty geometry is to be created.
   * @param precisionModel
   *   the specification of the grid of allowable points for this <code>Polygon</code>
   * @param SRID
   *   the ID of the Spatial Reference System used by this <code>Polygon</code>
   * @deprecated
   *   Use GeometryFactory instead
   */
  def this(shell: LinearRing, precisionModel: PrecisionModel, SRID: Int) =
    this(shell, Array.empty[LinearRing], new GeometryFactory(precisionModel, SRID))

  /**
   * Constructs a <code>Polygon</code> with the given exterior boundary and interior boundaries.
   *
   * @param shell
   *   the outer boundary of the new <code>Polygon</code>, or <code>null</code> or an empty
   *   <code>LinearRing</code> if the empty geometry is to be created.
   * @param holes
   *   the inner boundaries of the new <code>Polygon</code> , or <code>null</code> or empty
   *   <code>LinearRing</code>s if the empty geometry is to be created.
   * @param precisionModel
   *   the specification of the grid of allowable points for this <code>Polygon</code>
   * @param SRID
   *   the ID of the Spatial Reference System used by this <code>Polygon</code>
   * @deprecated
   *   Use GeometryFactory instead
   */
  def this(
    shell:          LinearRing,
    holes:          Array[LinearRing],
    precisionModel: PrecisionModel,
    SRID:           Int
  ) =
    this(shell, holes, new GeometryFactory(precisionModel, SRID))

  override def getCoordinate: Coordinate = shell.getCoordinate

  override def getCoordinates: Array[Coordinate] = {
    if (isEmpty) return Array.empty[Coordinate]
    val coordinates      = new Array[Coordinate](getNumPoints)
    var k                = -1
    val shellCoordinates = shell.getCoordinates
    var x                = 0
    while (x < shellCoordinates.length) {
      k += 1
      coordinates(k) = shellCoordinates(x)
      x += 1
    }
    var i                = 0
    while (i < holes.length) {
      val childCoordinates = holes(i).getCoordinates
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

  override def getNumPoints: Int = {
    var numPoints = shell.getNumPoints
    var i         = 0
    while (i < holes.length) {
      numPoints += holes(i).getNumPoints
      i += 1
    }
    numPoints
  }

  override def getDimension = 2

  override def getBoundaryDimension = 1

  override def isEmpty: Boolean = shell.isEmpty

  override def isRectangle: Boolean = {
    if (getNumInteriorRing != 0) return false
    if (shell == null) return false
    if (shell.getNumPoints != 5) return false
    val seq   = shell.getCoordinateSequence
    // check vertices have correct values
    val env   = getEnvelopeInternal
    var i     = 0
    while (i < 5) {
      val x = seq.getX(i)
      if (!(x == env.getMinX || x == env.getMaxX)) return false
      val y = seq.getY(i)
      if (!(y == env.getMinY || y == env.getMaxY)) return false
      i += 1
    }
    // check vertices are in right order
    var prevX = seq.getX(0)
    var prevY = seq.getY(0)
    i = 1
    while (i <= 4) {
      val x        = seq.getX(i)
      val y        = seq.getY(i)
      val xChanged = x != prevX
      val yChanged = y != prevY
      if (xChanged == yChanged) return false
      prevX = x
      prevY = y
      i += 1
    }
    true
  }

  def getExteriorRing: LinearRing = shell

  def getNumInteriorRing: Int = holes.length

  def getInteriorRingN(n: Int): LinearRing = holes(n)

  override def getGeometryType =
    Geometry.TYPENAME_POLYGON

  /**
   * Returns the area of this <code>Polygon</code>
   *
   * return the area of the polygon
   */
  override def getArea: Double = {
    var area = 0.0
    area += Area.ofRing(shell.getCoordinateSequence)
    var i    = 0
    while (i < holes.length) {
      area -= Area.ofRing(holes(i).getCoordinateSequence)
      i += 1
    }
    area
  }

  /**
   * Returns the perimeter of this <code>Polygon</code>
   *
   * return the perimeter of the polygon
   */
  override def getLength: Double = {
    var len = 0.0
    len += shell.getLength
    var i   = 0
    while (i < holes.length) {
      len += holes(i).getLength
      i += 1
    }
    len
  }

  /**
   * Computes the boundary of this geometry
   *
   * return a lineal geometry (which may be empty)
   * @see
   *   Geometry#getBoundary
   */
  override def getBoundary: Geometry = {
    if (isEmpty) return getFactory.createMultiLineString
    val rings = new Array[LinearRing](holes.length + 1)
    rings(0) = shell
    var i     = 0
    while (i < holes.length) {
      rings(i + 1) = holes(i)
      i += 1
    }
    // create LineString or MultiLineString as appropriate
    if (rings.length <= 1) return getFactory.createLinearRing(rings(0).getCoordinateSequence)
    getFactory.createMultiLineString(rings.map(x => x: LineString))
  }

  override protected def computeEnvelopeInternal: Envelope = shell.getEnvelopeInternal

  override def equalsExact(other: Geometry, tolerance: Double): Boolean = {
    if (!isEquivalentClass(other)) return false
    val otherPolygon      = other.asInstanceOf[Polygon]
    val thisShell         = shell
    val otherPolygonShell = otherPolygon.shell
    if (!thisShell.equalsExact(otherPolygonShell, tolerance)) return false
    if (holes.length != otherPolygon.holes.length) return false
    var i                 = 0
    while (i < holes.length) {
      if (!holes(i).asInstanceOf[Geometry].equalsExact(otherPolygon.holes(i), tolerance))
        return false
      i += 1
    }
    true
  }

  def applyF(filter: CoordinateFilter): Unit = {
    shell.applyF(filter)
    var i = 0
    while (i < holes.length) {
      holes(i).applyF(filter)
      i += 1
    }
  }

  def applyF(filter: CoordinateSequenceFilter): Unit = {
    shell.applyF(filter)
    if (!filter.isDone) {
      var i = 0
      while (i < holes.length) {
        holes(i).applyF(filter)
        if (filter.isDone) {
          i = holes.length
        } else {
          i += 1
        }
      }
      if (filter.isGeometryChanged) geometryChanged()
    }
  }

  def applyF(filter: GeometryFilter): Unit = filter.filter(this)

  def applyF(filter: GeometryComponentFilter): Unit = {
    filter.filter(this)
    shell.applyF(filter)
    var i = 0
    while (i < holes.length) {
      holes(i).applyF(filter)
      i += 1
    }
  }

  /**
   * Creates and returns a full copy of this {link Polygon} object. (including all coordinates
   * contained by it).
   *
   * return a clone of this instance
   * @deprecated
   */
  override def clone: Geometry = copy

  override protected def copyInternal: Geometry = {
    val shellCopy  = shell.copy.asInstanceOf[LinearRing]
    val holeCopies = new Array[LinearRing](this.holes.length)
    var i          = 0
    while (i < holes.length) {
      holeCopies(i) = holes(i).copy.asInstanceOf[LinearRing]
      i += 1
    }
    new Polygon(shellCopy, holeCopies, factory)
  }

  override def convexHull: Geometry = getExteriorRing.convexHull

  override def normalize(): Unit = {
    shell = normalized(shell, true)
    var i = 0
    while (i < holes.length) {
      holes(i) = normalized(holes(i), false)
      i += 1
    }
    holes = holes.sorted
  }

  protected def compareToSameClass(o: AnyRef): Int = {
    val thisShell: LinearRing  = shell
    val otherShell: LinearRing = o.asInstanceOf[Polygon].shell
    thisShell.compareToSameClass(otherShell)
  }

  def compareToSameClass(o: AnyRef, comp: CoordinateSequenceComparator): Int = {
    val poly       = o.asInstanceOf[Polygon]
    val thisShell  = shell
    val otherShell = poly.shell
    val shellComp  = thisShell.compareToSameClass(otherShell, comp)
    if (shellComp != 0) return shellComp
    val nHole1     = getNumInteriorRing
    val nHole2     = poly.getNumInteriorRing
    var i          = 0
    while (i < nHole1 && i < nHole2) {
      val thisHole  = getInteriorRingN(i).asInstanceOf[LinearRing]
      val otherHole = poly.getInteriorRingN(i).asInstanceOf[LinearRing]
      val holeComp  = thisHole.compareToSameClass(otherHole, comp)
      if (holeComp != 0) return holeComp
      i += 1
    }
    if (i < nHole1) return 1
    if (i < nHole2) return -1
    return 0
  }

  override protected def getTypeCode: Int = Geometry.TYPECODE_POLYGON

  private def normalized(ring: LinearRing, clockwise: Boolean): LinearRing = {
    val res = ring.copy.asInstanceOf[LinearRing]
    normalize(res, clockwise)
    res
  }

  private def normalize(ring: LinearRing, clockwise: Boolean): Unit = {
    if (ring.isEmpty) return
    val seq                = ring.getCoordinateSequence
    val minCoordinateIndex = CoordinateSequences.minCoordinateIndex(seq, 0, seq.size - 2)
    CoordinateSequences.scroll(seq, minCoordinateIndex, true)
    if (Orientation.isCCW(seq) == clockwise) CoordinateSequences.reverse(seq)
  }

  /** @deprecated */
  override def reverse: Polygon = super.reverse.asInstanceOf[Polygon]

  override protected def reverseInternal: Polygon = {
    val shell = getExteriorRing.reverse.asInstanceOf[LinearRing]
    val holes = new Array[LinearRing](getNumInteriorRing)
    var i     = 0
    while (i < holes.length) {
      holes(i) = getInteriorRingN(i).reverse.asInstanceOf[LinearRing]
      i += 1
    }
    getFactory.createPolygon(shell, holes)
  }
}
