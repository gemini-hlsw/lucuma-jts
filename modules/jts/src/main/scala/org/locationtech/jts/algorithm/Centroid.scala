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
package org.locationtech.jts.algorithm

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryCollection
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.Point
import org.locationtech.jts.geom.Polygon

/**
 * Computes the centroid of a {link Geometry} of any dimension. If the geometry is nominally of
 * higher dimension, but has lower <i>effective</i> dimension (i.e. contains only components having
 * zero length or area), the centroid will be computed as for the equivalent lower-dimension
 * geometry. If the input geometry is empty, a <code>null</code> Coordinate is returned.
 *
 * <h2>Algorithm</h2> <ul> <li><b>Dimension 2</b> - the centroid is computed as the weighted sum of
 * the centroids of a decomposition of the area into (possibly overlapping) triangles. Holes and
 * multipolygons are handled correctly. See
 * <code>http://www.faqs.org/faqs/graphics/algorithms-faq/</code> for further details of the basic
 * approach.
 *
 * <li><b>Dimension 1</b> - Computes the average of the midpoints of all line segments weighted by
 * the segment length. Zero-length lines are treated as points.
 *
 * <li><b>Dimension 0</b> - Compute the average coordinate for all points. Repeated points are all
 * included in the average. </ul>
 *
 * @version 1.7
 */
object Centroid {

  /**
   * Computes the centroid point of a geometry.
   *
   * @param geom
   *   the geometry to use return the centroid point, or null if the geometry is empty
   */
  def getCentroid(geom: Geometry): Coordinate = {
    val cent = new Centroid(geom)
    cent.getCentroid
  }

  /**
   * Computes three times the centroid of the triangle p1-p2-p3. The factor of 3 is left in to
   * permit division to be avoided until later.
   */
  private def centroid3(p1: Coordinate, p2: Coordinate, p3: Coordinate, c: Coordinate): Unit = {
    c.x = p1.x + p2.x + p3.x
    c.y = p1.y + p2.y + p3.y
  }

  /**
   * Returns twice the signed area of the triangle p1-p2-p3. The area is positive if the triangle is
   * oriented CCW, and negative if CW.
   */
  private def area2(p1: Coordinate, p2: Coordinate, p3: Coordinate): Double =
    (p2.x - p1.x) * (p3.y - p1.y) - (p3.x - p1.x) * (p2.y - p1.y)
}

class Centroid(val geom: Geometry) {

  private var areaBasePt: Coordinate = null           // the point all triangles are based at
  private val triangleCent3          = new Coordinate // temporary variable to hold centroid of triangle
  private var areasum2: Double       = 0
  /* Partial area sum */
  private val cg3                    = new Coordinate // partial centroid sum
  // data for linear centroid computation, if needed
  private val lineCentSum            = new Coordinate
  private var totalLength            = 0.0
  private var ptCount                = 0
  private val ptCentSum              = new Coordinate

  /**
   * Creates a new instance for computing the centroid of a geometry
   */
  add(geom)

  /**
   * Adds a Geometry to the centroid total.
   *
   * @param geom
   *   the geometry to add
   */
  private def add(geom: Geometry): Unit = {
    if (geom.isEmpty) return
    geom match {
      case _: Point               => addPoint(geom.getCoordinate)
      case _: LineString          => addLineSegments(geom.getCoordinates)
      case poly: Polygon          =>
        add(poly)
      case gc: GeometryCollection =>
        var i = 0
        while (i < gc.getNumGeometries) {
          add(gc.getGeometryN(i))
          i += 1
        }
      case _                      =>
    }
  }

  /**
   * Gets the computed centroid.
   *
   * return the computed centroid, or null if the input is empty
   */
  def getCentroid: Coordinate = {

    /**
     * The centroid is computed from the highest dimension components present in the input. I.e.
     * areas dominate lineal geometry, which dominates points. Degenerate geometry are computed
     * using their effective dimension (e.g. areas may degenerate to lines or points)
     */
    val cent = new Coordinate
    if (Math.abs(areasum2) > 0.0) {

      /**
       * Input contains areal geometry
       */
      cent.x = cg3.x / 3 / areasum2
      cent.y = cg3.y / 3 / areasum2
    } else if (totalLength > 0.0) {

      /**
       * Input contains lineal geometry
       */
      cent.x = lineCentSum.x / totalLength
      cent.y = lineCentSum.y / totalLength
    } else if (ptCount > 0) {

      /**
       * Input contains puntal geometry only
       */
      cent.x = ptCentSum.x / ptCount
      cent.y = ptCentSum.y / ptCount
    } else return null
    cent
  }

  private def setAreaBasePoint(basePt: Coordinate): Unit = this.areaBasePt = basePt

  private def add(poly: Polygon): Unit = {
    addShell(poly.getExteriorRing.getCoordinates)
    var i = 0
    while (i < poly.getNumInteriorRing) {
      addHole(poly.getInteriorRingN(i).getCoordinates)
      i += 1
    }
  }

  private def addShell(pts: Array[Coordinate]): Unit = {
    if (pts.length > 0) setAreaBasePoint(pts(0))
    val isPositiveArea = !Orientation.isCCW(pts)
    var i              = 0
    while (i < pts.length - 1) {
      addTriangle(areaBasePt, pts(i), pts(i + 1), isPositiveArea)
      i += 1
    }
    addLineSegments(pts)
  }

  private def addHole(pts: Array[Coordinate]): Unit = {
    val isPositiveArea = Orientation.isCCW(pts)
    var i              = 0
    while (i < pts.length - 1) {
      addTriangle(areaBasePt, pts(i), pts(i + 1), isPositiveArea)
      i += 1
    }
    addLineSegments(pts)
  }

  private def addTriangle(
    p0:             Coordinate,
    p1:             Coordinate,
    p2:             Coordinate,
    isPositiveArea: Boolean
  ): Unit = {
    val sign: Double  = if (isPositiveArea) 1.0 else -1.0
    Centroid.centroid3(p0, p1, p2, triangleCent3)
    val area2: Double = Centroid.area2(p0, p1, p2)
    cg3.x += sign * area2 * triangleCent3.x
    cg3.y += sign * area2 * triangleCent3.y
    areasum2 += sign * area2
  }

  /**
   * Adds the line segments defined by an array of coordinates to the linear centroid accumulators.
   *
   * @param pts
   *   an array of {@link Coordinate} s
   */
  private def addLineSegments(pts: Array[Coordinate]): Unit = {
    var lineLen = 0.0
    // var i       = 0
    for (i <- 0 until (pts.length - 1)) {
      val segmentLen = pts(i).distance(pts(i + 1))
      if (segmentLen != 0.0) {
        lineLen += segmentLen
        val midx = (pts(i).x + pts(i + 1).x) / 2
        lineCentSum.x += segmentLen * midx
        val midy = (pts(i).y + pts(i + 1).y) / 2
        lineCentSum.y += segmentLen * midy
        // i += 1;
        // i - 1
      }
    }
    totalLength += lineLen
    if (lineLen == 0.0 && pts.length > 0) addPoint(pts(0))
  }

  /**
   * Adds a point to the point centroid accumulator.
   *
   * @param pt
   *   a {@link Coordinate}
   */
  private def addPoint(pt: Coordinate): Unit = {
    ptCount += 1
    ptCentSum.x += pt.x
    ptCentSum.y += pt.y
  }
}
