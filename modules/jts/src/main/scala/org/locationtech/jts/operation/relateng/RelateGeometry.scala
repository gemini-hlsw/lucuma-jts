// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2023 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v20.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.operation.relateng

import org.locationtech.jts.algorithm.BoundaryNodeRule
import org.locationtech.jts.algorithm.Orientation
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateArrays
import org.locationtech.jts.geom.Dimension
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryCollection
import org.locationtech.jts.geom.GeometryCollectionIterator
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.LinearRing
import org.locationtech.jts.geom.MultiLineString
import org.locationtech.jts.geom.MultiPoint
import org.locationtech.jts.geom.MultiPolygon
import org.locationtech.jts.geom.Point
import org.locationtech.jts.geom.Polygon
import org.locationtech.jts.geom.util.ComponentCoordinateExtracter
import org.locationtech.jts.geom.util.PointExtracter

import java.util

class RelateGeometry(input: Geometry, prepared: Boolean, bnRule: BoundaryNodeRule) {

  def this(input: Geometry) =
    this(input, false, BoundaryNodeRule.OGC_SFS_BOUNDARY_RULE)

  def this(input: Geometry, bnRule: BoundaryNodeRule) =
    this(input, false, bnRule)

  private val geom: Geometry                       = input
  private val geomEnv: Envelope                    = input.getEnvelopeInternal
  private val boundaryNodeRule: BoundaryNodeRule   = bnRule
  private var geomDim: Int                         = Dimension.FALSE
  private var uniquePoints: util.Set[Coordinate]   = null
  private var locator: RelatePointLocator          = null
  private var elementId                            = 0
  private var hasPoints                            = false
  private var hasLines                             = false
  private var hasAreas                             = false
  // -- cache geometry metadata
  private val isGeomEmpty: Boolean                 = geom.isEmpty

  geomDim = input.getDimension
  analyzeDimensions()
  private val isLineZeroLen: Boolean = isZeroLengthLine(geom)

  private def isZeroLengthLine(geom: Geometry): Boolean = {
    // avoid expensive zero-length calculation if not linear
    if (getDimension != Dimension.L)
      return false
    RelateGeometry.isZeroLength(geom)
  }

  private def analyzeDimensions(): Unit = {
    if (isGeomEmpty) {
      return
    }
    if (geom.isInstanceOf[Point] || geom.isInstanceOf[MultiPoint]) {
      hasPoints = true
      geomDim = Dimension.P
      return
    }
    if (geom.isInstanceOf[LineString] || geom.isInstanceOf[MultiLineString]) {
      hasLines = true
      geomDim = Dimension.L
      return
    }
    if (geom.isInstanceOf[Polygon] || geom.isInstanceOf[MultiPolygon]) {
      hasAreas = true
      geomDim = Dimension.A
      return
    }
    // -- analyze a (possibly mixed type) collection
    val geomi = new GeometryCollectionIterator(geom)
    while (geomi.hasNext) {
      val elem = geomi.next
      if (!elem.isEmpty) {
        if (elem.isInstanceOf[Point]) {
          hasPoints = true
          if (geomDim < Dimension.P) geomDim = Dimension.P
        }
        if (elem.isInstanceOf[LineString]) {
          hasLines = true
          if (geomDim < Dimension.L) geomDim = Dimension.L
        }
        if (elem.isInstanceOf[Polygon]) {
          hasAreas = true
          if (geomDim < Dimension.A) geomDim = Dimension.A
        }
      }
    }
  }

  def getGeometry: Geometry = geom

  def isPrepared: Boolean = prepared

  def getEnvelope: Envelope = geomEnv

  def getDimension: Int = geomDim

  def hasDimension(dim: Int): Boolean =
    dim match {
      case Dimension.P => hasPoints
      case Dimension.L => hasLines
      case Dimension.A => hasAreas
      case _           => false
    }

  /**
   * Gets the actual non-empty dimension of the geometry. Zero-length LineStrings are treated as
   * Points.
   *
   * return the real (non-empty) dimension
   */
  def getDimensionReal: Int = {
    if (isGeomEmpty) return Dimension.FALSE
    if (getDimension == 1 && isLineZeroLen)
      return Dimension.P
    if (hasAreas) return Dimension.A
    if (hasLines) return Dimension.L
    Dimension.P
  }

  def hasEdges: Boolean = hasLines || hasAreas

  private def getLocator: RelatePointLocator = {
    if (locator == null)
      locator = new RelatePointLocator(geom, prepared, boundaryNodeRule)
    locator
  }

  def isNodeInArea(nodePt: Coordinate, parentPolygonal: Geometry): Boolean = {
    val loc = getLocator.locateNodeWithDim(nodePt, parentPolygonal)
    loc == DimensionLocation.AREA_INTERIOR
  }

  def locateLineEndWithDim(p: Coordinate): Int =
    getLocator.locateLineEndWithDim(p)

  /**
   * Locates a vertex of a polygon. A vertex of a Polygon or MultiPolygon is on the {link
   * Location#BOUNDARY}. But a vertex of an overlapped polygon in a GeometryCollection may be in the
   * {link Location#INTERIOR}.
   *
   * @param pt
   *   the polygon vertex return the location of the vertex
   */
  def locateAreaVertex(pt: Coordinate): Int =
    /**
     * Can pass a null polygon, because the point is an exact vertex, which will be detected as
     * being on the boundary of its polygon
     */
    locateNode(pt, null)

  def locateNode(pt: Coordinate, parentPolygonal: Geometry): Int =
    getLocator.locateNode(pt, parentPolygonal)

  def locateWithDim(pt: Coordinate): Int = {
    val loc = getLocator.locateWithDim(pt)
    loc
  }

  /**
   * Indicates whether the geometry requires self-noding for correct evaluation of specific spatial
   * predicates. Self-noding is required for geometries which may self-cross
   *   - i.e. lines, and overlapping elements in GeometryCollections. Self-noding is not required
   *     for polygonal geometries, since they can only touch at vertices.
   *
   * return true if self-noding is required for this geometry
   */
  def isSelfNodingRequired: Boolean = {
    if (
      geom.isInstanceOf[Point]
      || geom.isInstanceOf[MultiPoint]
      || geom.isInstanceOf[Polygon]
      || geom.isInstanceOf[MultiPolygon]
    )
      return false
    // -- a GC with a single polygon does not need noding
    if (hasAreas && geom.getNumGeometries == 1)
      return false
    true
  }

  /**
   * Tests whether the geometry has polygonal topology. This is not the case if it is a
   * GeometryCollection containing more than one polygon (since they may overlap or be adjacent).
   * The significance is that polygonal topology allows more assumptions about the location of
   * boundary vertices.
   *
   * return true if the geometry has polygonal topology
   */
  def isPolygonal: Boolean =
    // TODO: also true for a GC containing one polygonal element (and possibly some lower-dimension elements)
    geom.isInstanceOf[Polygon] || geom.isInstanceOf[MultiPolygon]

  def isEmpty: Boolean = isGeomEmpty

  def hasBoundary: Boolean = getLocator.hasBoundary

  def getUniquePoints: util.Set[Coordinate] = {
    // -- will be re-used in prepared mode
    if (uniquePoints == null) {
      uniquePoints = createUniquePoints
    }
    uniquePoints
  }

  private def createUniquePoints: util.Set[Coordinate] = {
    // -- only called on P geometries
    val pts = ComponentCoordinateExtracter.getCoordinates(geom)
    val set = new util.HashSet[Coordinate]
    set.addAll(pts)
    set
  }

  def getEffectivePoints: util.List[Point] = {
    val ptListAll = PointExtracter.getPoints(geom).asInstanceOf[util.List[Point]]

    if (getDimensionReal <= Dimension.P)
      return ptListAll

    // -- only return Points not covered by another element
    val ptList = new util.ArrayList[Point]
    val it     = ptListAll.iterator
    while (it.hasNext) {
      val p = it.next()
      if (!p.isEmpty) {
        val locDim = locateWithDim(p.getCoordinate)
        if (DimensionLocation.dimension(locDim) == Dimension.P) {
          ptList.add(p)
        }
      }
    }
    ptList
  }

  /**
   * Extract RelateSegmentStrings from the geometry which intersect a given envelope. If the
   * envelope is null all edges are extracted.
   *
   * @param isA
   * @param env
   *   the envelope to extract around (may be null) return a list of RelateSegmentStrings
   */
  def extractSegmentStrings(isA: Boolean, env: Envelope): util.List[RelateSegmentString] = {
    val segStrings = new util.ArrayList[RelateSegmentString]
    extractSegmentStrings(isA, env, geom, segStrings)
    segStrings
  }

  private def extractSegmentStrings(
    isA:        Boolean,
    env:        Envelope,
    geom:       Geometry,
    segStrings: util.List[RelateSegmentString]
  ): Unit = {
    // -- record if parent is MultiPolygon
    var parentPolygonal: MultiPolygon = null
    if (geom.isInstanceOf[MultiPolygon]) {
      parentPolygonal = geom.asInstanceOf[MultiPolygon]
    }

    var i = 0
    while (i < geom.getNumGeometries) {
      val g = geom.getGeometryN(i)
      if (g.isInstanceOf[GeometryCollection]) {
        extractSegmentStrings(isA, env, g, segStrings)
      } else {
        extractSegmentStringsFromAtomic(isA, g, parentPolygonal, env, segStrings)
      }
      i += 1
    }
  }

  private def extractSegmentStringsFromAtomic(
    isA:             Boolean,
    geom:            Geometry,
    parentPolygonal: MultiPolygon,
    env:             Envelope,
    segStrings:      util.List[RelateSegmentString]
  ): Unit = {
    if (geom.isEmpty)
      return
    val doExtract = env == null || env.intersects(geom.getEnvelopeInternal)
    if (!doExtract)
      return

    elementId += 1
    if (geom.isInstanceOf[LineString]) {
      val ss = RelateSegmentString.createLine(geom.getCoordinates, isA, elementId, this)
      segStrings.add(ss)
      ()
    } else if (geom.isInstanceOf[Polygon]) {
      val poly       = geom.asInstanceOf[Polygon]
      val parentPoly = if (parentPolygonal != null) parentPolygonal else poly
      extractRingToSegmentString(isA, poly.getExteriorRing, 0, env, parentPoly, segStrings)
      var i          = 0
      while (i < poly.getNumInteriorRing) {
        extractRingToSegmentString(isA,
                                   poly.getInteriorRingN(i),
                                   i + 1,
                                   env,
                                   parentPoly,
                                   segStrings
        )
        i += 1
      }
    }
  }

  private def extractRingToSegmentString(
    isA:        Boolean,
    ring:       LinearRing,
    ringId:     Int,
    env:        Envelope,
    parentPoly: Geometry,
    segStrings: util.List[RelateSegmentString]
  ): Unit = {
    if (ring.isEmpty)
      return
    if (env != null && !env.intersects(ring.getEnvelopeInternal))
      return

    // -- orient the points if required
    val requireCW = ringId == 0
    val pts       = RelateGeometry.orient(ring.getCoordinates, requireCW)
    val ss        = RelateSegmentString.createRing(pts, isA, elementId, ringId, parentPoly, this)
    segStrings.add(ss)
    ()
  }

  override def toString: String = geom.toString

}

object RelateGeometry {

  val GEOM_A: Boolean = true
  val GEOM_B: Boolean = false

  def name(isA: Boolean): String = if (isA) "A" else "B"

  /**
   * Tests if all geometry linear elements are zero-length. For efficiency the test avoids computing
   * actual length.
   *
   * @param geom
   */
  private def isZeroLength(geom: Geometry): Boolean = {
    val geomi = new GeometryCollectionIterator(geom)
    while (geomi.hasNext) {
      val elem = geomi.next
      if (elem.isInstanceOf[LineString]) {
        if (!isZeroLength(elem.asInstanceOf[LineString]))
          return false
      }
    }
    true
  }

  private def isZeroLength(line: LineString): Boolean = {
    if (line.getNumPoints >= 2) {
      val p0 = line.getCoordinateN(0)
      var i  = 0
      while (i < line.getNumPoints) {
        val pi = line.getCoordinateN(i)
        // -- most non-zero-len lines will trigger this right away
        if (!p0.equals2D(pi))
          return false
        i += 1
      }
    }
    true
  }

  def orient(pts: Array[Coordinate], orientCW: Boolean): Array[Coordinate] = {
    val isFlipped = orientCW == Orientation.isCCW(pts)
    if (isFlipped) {
      val ptsRev = pts.clone
      CoordinateArrays.reverse(ptsRev)
      return ptsRev
    }
    pts
  }

}
