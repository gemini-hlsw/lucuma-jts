// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2024 Martin Davis.
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
import org.locationtech.jts.algorithm.PointLocation
import org.locationtech.jts.algorithm.locate.IndexedPointInAreaLocator
import org.locationtech.jts.algorithm.locate.PointOnGeometryLocator
import org.locationtech.jts.algorithm.locate.SimplePointInAreaLocator
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryCollection
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.Location
import org.locationtech.jts.geom.MultiPolygon
import org.locationtech.jts.geom.Point
import org.locationtech.jts.geom.Polygon

import java.util

/**
 * Locates a point on a geometry, including mixed-type collections. The dimension of the containing
 * geometry element is also determined. GeometryCollections are handled with union semantics; i.e.
 * the location of a point is that location of that point on the union of the elements of the
 * collection. <p> Union semantics for GeometryCollections has the following behaviours: <ol>
 * <li>For a mixed-dimension (heterogeneous) collection a point may lie on two geometry elements
 * with different dimensions. In this case the location on the largest-dimension element is
 * reported. <li>For a collection with overlapping or adjacent polygons, points on polygon element
 * boundaries may lie in the effective interior of the collection geometry. </ol> Prepared mode is
 * supported via cached spatial indexes. <p> Supports specifying the {link BoundaryNodeRule} to use
 * for line endpoints.
 *
 * @author
 *   Martin Davis
 */
class RelatePointLocator(geom: Geometry, isPrepared: Boolean, boundaryRule: BoundaryNodeRule) {

  def this(geom: Geometry) =
    this(geom, false, BoundaryNodeRule.OGC_SFS_BOUNDARY_RULE)

  private var adjEdgeLocator: AdjacentEdgeLocator        = null
  private var points: util.Set[Coordinate]               = null
  private var lines: util.List[LineString]               = null
  private var polygons: util.List[Geometry]              = null
  private var polyLocator: Array[PointOnGeometryLocator] = null
  private var lineBoundary: LinearBoundary               = null
  private var isEmpty                                    = false

  init(geom)

  private def init(geom: Geometry): Unit = {
    // -- cache empty status, since may be checked many times
    isEmpty = geom.isEmpty
    extractElements(geom)

    if (lines != null) {
      lineBoundary = new LinearBoundary(lines, boundaryRule)
    }

    if (polygons != null) {
      polyLocator = new Array[PointOnGeometryLocator](polygons.size)
    }
  }

  def hasBoundary: Boolean = lineBoundary.hasBoundary

  private def extractElements(geom: Geometry): Unit = {
    if (geom.isEmpty)
      return

    if (geom.isInstanceOf[Point]) {
      addPoint(geom.asInstanceOf[Point])
    } else if (geom.isInstanceOf[LineString]) {
      addLine(geom.asInstanceOf[LineString])
    } else if (geom.isInstanceOf[Polygon] || geom.isInstanceOf[MultiPolygon]) {
      addPolygonal(geom)
    } else if (geom.isInstanceOf[GeometryCollection]) {
      var i = 0
      while (i < geom.getNumGeometries) {
        val g = geom.getGeometryN(i)
        extractElements(g)
        i += 1
      }
    }
  }

  private def addPoint(pt: Point): Unit = {
    if (points == null) {
      points = new util.HashSet[Coordinate]
    }
    points.add(pt.getCoordinate)
    ()
  }

  private def addLine(line: LineString): Unit = {
    if (lines == null) {
      lines = new util.ArrayList[LineString]
    }
    lines.add(line)
    ()
  }

  private def addPolygonal(polygonal: Geometry): Unit = {
    if (polygons == null) {
      polygons = new util.ArrayList[Geometry]
    }
    polygons.add(polygonal)
    ()
  }

  def locate(p: Coordinate): Int = DimensionLocation.location(locateWithDim(p))

  /**
   * Locates a line endpoint, as a {link DimensionLocation}. In a mixed-dim GC, the line end point
   * may also lie in an area. In this case the area location is reported. Otherwise, the dimLoc is
   * either LINE_BOUNDARY or LINE_INTERIOR, depending on the endpoint valence and the
   * BoundaryNodeRule in place.
   *
   * @param p
   *   the line end point to locate return the dimension and location of the line end point
   */
  def locateLineEndWithDim(p: Coordinate): Int = {
    // -- if a GC with areas, check for point on area
    if (polygons != null) {
      val locPoly = locateOnPolygons(p, false, null)
      if (locPoly != Location.EXTERIOR)
        return DimensionLocation.locationArea(locPoly)
    }
    // -- not in area, so return line end location
    if (lineBoundary.isBoundary(p)) DimensionLocation.LINE_BOUNDARY
    else DimensionLocation.LINE_INTERIOR
  }

  /**
   * Locates a point which is known to be a node of the geometry (i.e. a vertex or on an edge).
   *
   * @param p
   *   the node point to locate
   * @param parentPolygonal
   *   the polygon the point is a node of return the location of the node point
   */
  def locateNode(p: Coordinate, parentPolygonal: Geometry): Int =
    DimensionLocation.location(locateNodeWithDim(p, parentPolygonal))

  /**
   * Locates a point which is known to be a node of the geometry, as a {link DimensionLocation}.
   *
   * @param p
   *   the point to locate
   * @param parentPolygonal
   *   the polygon the point is a node of return the dimension and location of the point
   */
  def locateNodeWithDim(p: Coordinate, parentPolygonal: Geometry): Int =
    locateWithDim(p, true, parentPolygonal)

  /**
   * Computes the topological location ({link Location}) of a single point in a Geometry, as well as
   * the dimension of the geometry element the point is located in (if not in the Exterior). It
   * handles both single-element and multi-element Geometries. The algorithm for multi-part
   * Geometries takes into account the SFS Boundary Determination Rule.
   *
   * @param p
   *   the point to locate return the {link Location} of the point relative to the input Geometry
   */
  def locateWithDim(p: Coordinate): Int = locateWithDim(p, false, null)

  /**
   * Computes the topological location ({link Location}) of a single point in a Geometry, as well as
   * the dimension of the geometry element the point is located in (if not in the Exterior). It
   * handles both single-element and multi-element Geometries. The algorithm for multi-part
   * Geometries takes into account the SFS Boundary Determination Rule.
   *
   * @param p
   *   the coordinate to locate
   * @param isNode
   *   whether the coordinate is a node (on an edge) of the geometry
   * @param parentPolygonal
   *   return the {link Location} of the point relative to the input Geometry
   */
  private def locateWithDim(p: Coordinate, isNode: Boolean, parentPolygonal: Geometry): Int = {
    if (isEmpty) return DimensionLocation.EXTERIOR

    /**
     * In a polygonal geometry a node must be on the boundary. (This is not the case for a mixed
     * collection, since the node may be in the interior of a polygon.)
     */
    if (isNode && (geom.isInstanceOf[Polygon] || geom.isInstanceOf[MultiPolygon]))
      return DimensionLocation.AREA_BOUNDARY

    val dimLoc = computeDimLocation(p, isNode, parentPolygonal)
    dimLoc
  }

  private def computeDimLocation(p: Coordinate, isNode: Boolean, parentPolygonal: Geometry): Int = {
    // -- check dimensions in order of precedence
    if (polygons != null) {
      val locPoly = locateOnPolygons(p, isNode, parentPolygonal)
      if (locPoly != Location.EXTERIOR)
        return DimensionLocation.locationArea(locPoly)
    }
    if (lines != null) {
      val locLine = locateOnLines(p, isNode)
      if (locLine != Location.EXTERIOR)
        return DimensionLocation.locationLine(locLine)
    }
    if (points != null) {
      val locPt = locateOnPoints(p)
      if (locPt != Location.EXTERIOR)
        return DimensionLocation.locationPoint(locPt)
    }
    DimensionLocation.EXTERIOR
  }

  private def locateOnPoints(p: Coordinate): Int =
    if (points.contains(p)) {
      Location.INTERIOR
    } else {
      Location.EXTERIOR
    }

  private def locateOnLines(p: Coordinate, isNode: Boolean): Int = {
    if (lineBoundary != null && lineBoundary.isBoundary(p)) {
      return Location.BOUNDARY
    }
    // -- must be on line, in interior
    if (isNode)
      return Location.INTERIOR

    // TODO: index the lines
    val it = lines.iterator
    while (it.hasNext) {
      val line = it.next()
      // -- have to check every line, since any/all may contain point
      val loc  = locateOnLine(p, isNode, line)
      if (loc != Location.EXTERIOR)
        return loc
      // TODO: minor optimization - some BoundaryNodeRules can short-circuit
    }
    Location.EXTERIOR
  }

  private def locateOnLine(p: Coordinate, isNode: Boolean, l: LineString): Int = {
    // bounding-box check
    if (!l.getEnvelopeInternal.intersects(p))
      return Location.EXTERIOR

    val seq = l.getCoordinateSequence
    if (PointLocation.isOnLine(p, seq)) {
      return Location.INTERIOR
    }
    Location.EXTERIOR
  }

  private def locateOnPolygons(p: Coordinate, isNode: Boolean, parentPolygonal: Geometry): Int = {
    var numBdy = 0
    // TODO: use a spatial index on the polygons
    var i      = 0
    while (i < polygons.size) {
      val loc = locateOnPolygonal(p, isNode, parentPolygonal, i)
      if (loc == Location.INTERIOR) {
        return Location.INTERIOR
      }
      if (loc == Location.BOUNDARY) {
        numBdy += 1
      }
      i += 1
    }
    if (numBdy == 1) {
      return Location.BOUNDARY
    } else if (numBdy > 1) {
      // -- check for point lying on adjacent boundaries
      if (adjEdgeLocator == null) {
        adjEdgeLocator = new AdjacentEdgeLocator(geom)
      }
      return adjEdgeLocator.locate(p)
    }
    Location.EXTERIOR
  }

  private def locateOnPolygonal(
    p:               Coordinate,
    isNode:          Boolean,
    parentPolygonal: Geometry,
    index:           Int
  ): Int = {
    val polygonal = polygons.get(index)
    if (isNode && (parentPolygonal eq polygonal)) {
      return Location.BOUNDARY
    }
    val locator   = getLocator(index)
    locator.locate(p)
  }

  private def getLocator(index: Int): PointOnGeometryLocator = {
    var locator = polyLocator(index)
    if (locator == null) {
      val polygonal = polygons.get(index)
      locator =
        if (isPrepared) new IndexedPointInAreaLocator(polygonal)
        else new SimplePointInAreaLocator(polygonal)
      polyLocator(index) = locator
    }
    locator
  }

}
