// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
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
package org.locationtech.jts.operation.distance

import java.util
import org.locationtech.jts.algorithm.Distance
import org.locationtech.jts.algorithm.PointLocator
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.LineSegment
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.Location
import org.locationtech.jts.geom.Point
import org.locationtech.jts.geom.Polygon
import org.locationtech.jts.geom.util.LinearComponentExtracter
import org.locationtech.jts.geom.util.PointExtracter
import org.locationtech.jts.geom.util.PolygonExtracter

/**
 * Find two points on two {link Geometry}s which lie within a given distance, or else are the
 * nearest points on the geometries (in which case this also provides the distance between the
 * geometries). <p> The distance computation also finds a pair of points in the input geometries
 * which have the minimum distance between them. If a point lies in the interior of a line segment,
 * the coordinate computed is a close approximation to the exact point. <p> Empty geometry
 * collection components are ignored. <p> The algorithms used are straightforward O(n^2)
 * comparisons. This worst-case performance could be improved on by using Voronoi techniques or
 * spatial indexes.
 *
 * @version 1.7
 */
object DistanceOp {

  /**
   * Compute the distance between the nearest points of two geometries.
   *
   * @param g0
   *   a { @link Geometry}
   * @param g1
   *   another { @link Geometry} return the distance between the geometries
   */
  def distance(g0: Geometry, g1: Geometry): Double = {
    val distOp = new DistanceOp(g0, g1)
    distOp.distance
  }

  /**
   * Test whether two geometries lie within a given distance of each other.
   *
   * @param g0
   *   a { @link Geometry}
   * @param g1
   *   another { @link Geometry}
   * @param distance
   *   the distance to test return true if g0.distance(g1) &lt;= distance
   */
  def isWithinDistance(g0: Geometry, g1: Geometry, distance: Double): Boolean = { // check envelope distance for a short-circuit negative result
    val envDist = g0.getEnvelopeInternal.distance(g1.getEnvelopeInternal)
    if (envDist > distance) return false
    // MD - could improve this further with a positive short-circuit based on envelope MinMaxDist
    val distOp  = new DistanceOp(g0, g1, distance)
    distOp.distance <= distance
  }

  /**
   * Compute the the nearest points of two geometries. The points are presented in the same order as
   * the input Geometries.
   *
   * @param g0
   *   a { @link Geometry}
   * @param g1
   *   another { @link Geometry} return the nearest points in the geometries
   */
  def nearestPoints(g0: Geometry, g1: Geometry): Array[Coordinate] = {
    val distOp = new DistanceOp(g0, g1)
    distOp.nearestPoints
  }

  /**
   * Compute the the closest points of two geometries. The points are presented in the same order as
   * the input Geometries.
   *
   * @param g0
   *   a { @link Geometry}
   * @param g1
   *   another { @link Geometry} return the closest points in the geometries
   * @deprecated
   *   renamed to nearestPoints
   */
  def closestPoints(g0: Geometry, g1: Geometry): Array[Coordinate] = {
    val distOp = new DistanceOp(g0, g1)
    distOp.nearestPoints
  }
}

class DistanceOp(val g0: Geometry, val g1: Geometry, val terminateDistance: Double) {

  /**
   * Constructs a DistanceOp that computes the distance and nearest points between the two specified
   * geometries.
   *
   * @param g0
   *   a Geometry
   * @param g1
   *   a Geometry
   * @param terminateDistance
   *   the distance on which to terminate the search
   */
  val baseGeometry                                         = new Array[Geometry](2)
  baseGeometry(0) = g0
  baseGeometry(1) = g1
  // input
  private val geom: Array[Geometry]                        = null
  // working
  private val ptLocator                                    = new PointLocator
  private var minDistanceLocation: Array[GeometryLocation] = null
  private var minDistance                                  = Double.MaxValue

  /**
   * Constructs a DistanceOp that computes the distance and nearest points between the two specified
   * geometries.
   *
   * @param g0
   *   a Geometry
   * @param g1
   *   a Geometry
   */
  def this(g0: Geometry, g1: Geometry) =
    this(g0, g1, 0.0)

  /**
   * Report the distance between the nearest points on the input geometries.
   *
   * return the distance between the geometries or 0 if either input geometry is empty throws
   * IllegalArgumentException if either input geometry is null
   */
  def distance: Double = {
    if (geom(0) == null || geom(1) == null)
      throw new IllegalArgumentException("null geometries are not supported")
    if (geom(0).isEmpty || geom(1).isEmpty) return 0.0
    computeMinDistance()
    minDistance
  }

  /**
   * Report the coordinates of the nearest points in the input geometries. The points are presented
   * in the same order as the input Geometries.
   *
   * return a pair of { @link Coordinate}s of the nearest points
   */
  def nearestPoints: Array[Coordinate] = {
    computeMinDistance()
    val nearestPts =
      Array[Coordinate](minDistanceLocation(0).getCoordinate, minDistanceLocation(1).getCoordinate)
    nearestPts
  }

  /**
   * return a pair of { @link Coordinate}s of the nearest points
   * @deprecated
   *   renamed to nearestPoints
   */
  def closestPoints: Array[Coordinate] = nearestPoints

  /**
   * Report the locations of the nearest points in the input geometries. The locations are presented
   * in the same order as the input Geometries.
   *
   * return a pair of { @link GeometryLocation}s for the nearest points
   */
  def nearestLocations: Array[GeometryLocation] = {
    computeMinDistance()
    minDistanceLocation
  }

  /**
   * return a pair of { @link GeometryLocation}s for the nearest points
   * @deprecated
   *   renamed to nearestLocations
   */
  def closestLocations: Array[GeometryLocation] = nearestLocations

  private def updateMinDistance(locGeom: Array[GeometryLocation], flip: Boolean): Unit = { // if not set then don't update
    if (locGeom(0) == null) return
    if (flip) {
      minDistanceLocation(0) = locGeom(1)
      minDistanceLocation(1) = locGeom(0)
    } else {
      minDistanceLocation(0) = locGeom(0)
      minDistanceLocation(1) = locGeom(1)
    }
  }

  private def computeMinDistance(): Unit = { // only compute once!
    if (minDistanceLocation != null) return
    minDistanceLocation = new Array[GeometryLocation](2)
    computeContainmentDistance()
    if (minDistance <= terminateDistance) return
    computeFacetDistance()
  }

  private def computeContainmentDistance(): Unit = {
    val locPtPoly = new Array[GeometryLocation](2)
    // test if either geometry has a vertex inside the other
    computeContainmentDistance(0, locPtPoly)
    if (minDistance <= terminateDistance) return
    computeContainmentDistance(1, locPtPoly)
  }

  private def computeContainmentDistance(
    polyGeomIndex: Int,
    locPtPoly:     Array[GeometryLocation]
  ): Unit = {
    val polyGeom       = geom(polyGeomIndex)
    // if no polygon then nothing to do
    if (polyGeom.getDimension < 2) return
    val locationsIndex = 1 - polyGeomIndex
    val polys          = PolygonExtracter.getPolygons(polyGeom)
    if (polys.size > 0) {
      val insideLocs = ConnectedElementLocationFilter.getLocations(geom(locationsIndex))
      computeContainmentDistance(insideLocs, polys, locPtPoly)
      if (minDistance <= terminateDistance) { // this assigment is determined by the order of the args in the computeInside call above
        minDistanceLocation(locationsIndex) = locPtPoly(0)
        minDistanceLocation(polyGeomIndex) = locPtPoly(1)
        return
      }
    }
  }

  private def computeContainmentDistance(
    locs:      util.List[_],
    polys:     util.List[_],
    locPtPoly: Array[GeometryLocation]
  ): Unit = {
    var i = 0
    while (i < locs.size) {
      val loc = locs.get(i).asInstanceOf[GeometryLocation]
      var j   = 0
      while (j < polys.size) {
        computeContainmentDistance(loc, polys.get(j).asInstanceOf[Polygon], locPtPoly)
        if (minDistance <= terminateDistance) return
        j += 1
      }
      i += 1
    }
  }

  private def computeContainmentDistance(
    ptLoc:     GeometryLocation,
    poly:      Polygon,
    locPtPoly: Array[GeometryLocation]
  ): Unit = {
    val pt = ptLoc.getCoordinate
    // if pt is not in exterior, distance to geom is 0
    if (Location.EXTERIOR != ptLocator.locate(pt, poly)) {
      minDistance = 0.0
      locPtPoly(0) = ptLoc
      locPtPoly(1) = new GeometryLocation(poly, pt)
      return
    }
  }

  /**
   * Computes distance between facets (lines and points) of input geometries.
   */
  private def computeFacetDistance(): Unit = {
    val locGeom = new Array[GeometryLocation](2)

    /**
     * Geometries are not wholely inside, so compute distance from lines and points of one to lines
     * and points of the other
     */
    val lines0 = LinearComponentExtracter.getLines(geom(0))
    val lines1 = LinearComponentExtracter.getLines(geom(1))
    val pts0   = PointExtracter.getPoints(geom(0))
    val pts1   = PointExtracter.getPoints(geom(1))
    // exit whenever minDistance goes LE than terminateDistance
    computeMinDistanceLines(lines0, lines1, locGeom)
    updateMinDistance(locGeom, false)
    if (minDistance <= terminateDistance) return
    locGeom(0) = null
    locGeom(1) = null
    computeMinDistanceLinesPoints(lines0, pts1, locGeom)
    updateMinDistance(locGeom, false)
    if (minDistance <= terminateDistance) return
    locGeom(0) = null
    locGeom(1) = null
    computeMinDistanceLinesPoints(lines1, pts0, locGeom)
    updateMinDistance(locGeom, true)
    if (minDistance <= terminateDistance) return
    locGeom(0) = null
    locGeom(1) = null
    computeMinDistancePoints(pts0, pts1, locGeom)
    updateMinDistance(locGeom, false)
  }

  private def computeMinDistanceLines(
    lines0:  util.List[Geometry],
    lines1:  util.List[Geometry],
    locGeom: Array[GeometryLocation]
  ): Unit = {
    var i = 0
    while (i < lines0.size) {
      val line0 = lines0.get(i).asInstanceOf[LineString]
      var j     = 0
      while (j < lines1.size) {
        val line1 = lines1.get(j).asInstanceOf[LineString]
        computeMinDistance(line0, line1, locGeom)
        if (minDistance <= terminateDistance) return
        {
          j += 1; j - 1
        }
      }
      {
        i += 1; i - 1
      }
    }
  }

  private def computeMinDistancePoints(
    points0: util.List[Geometry],
    points1: util.List[Geometry],
    locGeom: Array[GeometryLocation]
  ): Unit = {
    var i = 0
    while (i < points0.size) {
      val pt0 = points0.get(i).asInstanceOf[Point]
      var j   = 0
      while (j < points1.size) {
        val pt1  = points1.get(j).asInstanceOf[Point]
        val dist = pt0.getCoordinate.distance(pt1.getCoordinate)
        if (dist < minDistance) {
          minDistance = dist
          locGeom(0) = new GeometryLocation(pt0, 0, pt0.getCoordinate)
          locGeom(1) = new GeometryLocation(pt1, 0, pt1.getCoordinate)
        }
        if (minDistance <= terminateDistance) return
        {
          j += 1; j - 1
        }
      }
      {
        i += 1; i - 1
      }
    }
  }

  private def computeMinDistanceLinesPoints(
    lines:   util.List[Geometry],
    points:  util.List[Geometry],
    locGeom: Array[GeometryLocation]
  ): Unit = {
    var i = 0
    while (i < lines.size) {
      val line = lines.get(i).asInstanceOf[LineString]
      var j    = 0
      while (j < points.size) {
        val pt = points.get(j).asInstanceOf[Point]
        computeMinDistance(line, pt, locGeom)
        if (minDistance <= terminateDistance) return
        {
          j += 1; j - 1
        }
      }
      {
        i += 1; i - 1
      }
    }
  }

  private def computeMinDistance(
    line0:   LineString,
    line1:   LineString,
    locGeom: Array[GeometryLocation]
  ): Unit = {
    if (line0.getEnvelopeInternal.distance(line1.getEnvelopeInternal) > minDistance) return
    val coord0 = line0.getCoordinates
    val coord1 = line1.getCoordinates
    // brute force approach!
    var i      = 0
    while (i < coord0.length - 1) { // short-circuit if line segment is far from line
      val segEnv0 = new Envelope(coord0(i), coord0(i + 1))
      if (segEnv0.distance(line1.getEnvelopeInternal) <= minDistance) {
        var j = 0
        while (j < coord1.length - 1) { // short-circuit if line segments are far apart
          val segEnv1 = new Envelope(coord1(j), coord1(j + 1))
          if (segEnv0.distance(segEnv1) <= minDistance) {
            val dist = Distance.segmentToSegment(coord0(i), coord0(i + 1), coord1(j), coord1(j + 1))
            if (dist < minDistance) {
              minDistance = dist
              val seg0      = new LineSegment(coord0(i), coord0(i + 1))
              val seg1      = new LineSegment(coord1(j), coord1(j + 1))
              val closestPt = seg0.closestPoints(seg1)
              locGeom(0) = new GeometryLocation(line0, i, closestPt(0))
              locGeom(1) = new GeometryLocation(line1, j, closestPt(1))
            }
            if (minDistance <= terminateDistance) return
          }
          j += 1
        }
      }
      i += 1
    }
  }

  private def computeMinDistance(
    line:    LineString,
    pt:      Point,
    locGeom: Array[GeometryLocation]
  ): Unit = {
    if (line.getEnvelopeInternal.distance(pt.getEnvelopeInternal) > minDistance) return
    val coord0 = line.getCoordinates
    val coord  = pt.getCoordinate
    var i      = 0
    while (i < coord0.length - 1) {
      val dist = Distance.pointToSegment(coord, coord0(i), coord0(i + 1))
      if (dist < minDistance) {
        minDistance = dist
        val seg             = new LineSegment(coord0(i), coord0(i + 1))
        val segClosestPoint = seg.closestPoint(coord)
        locGeom(0) = new GeometryLocation(line, i, segClosestPoint)
        locGeom(1) = new GeometryLocation(pt, 0, coord)
      }
      if (minDistance <= terminateDistance) return
      i += 1
    }
  }
}
