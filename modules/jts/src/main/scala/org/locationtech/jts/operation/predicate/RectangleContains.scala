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
package org.locationtech.jts.operation.predicate

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.Point
import org.locationtech.jts.geom.Polygon

/**
 * Optimized implementation of the <tt>contains</tt> spatial predicate for cases where the first
 * {link Geometry} is a rectangle. This class works for all input geometries, including {link
 * GeometryCollection}s. <p> As a further optimization, this class can be used to test many
 * geometries against a single rectangle in a slightly more efficient way.
 *
 * @version 1.7
 */
object RectangleContains {

  /**
   * Tests whether a rectangle contains a given geometry.
   *
   * @param rectangle
   *   a rectangular Polygon
   * @param b
   *   a Geometry of any type return true if the geometries intersect
   */
  def contains(rectangle: Polygon, b: Geometry): Boolean = {
    val rc = new RectangleContains(rectangle)
    rc.contains(b)
  }
}

class RectangleContains(val rectangle: Polygon) {

  /**
   * Create a new contains computer for two geometries.
   *
   * @param rectangle
   *   a rectangular geometry
   */
  private val rectEnv = rectangle.getEnvelopeInternal

  def contains(geom: Geometry): Boolean = { // the test geometry must be wholly contained in the rectangle envelope
    if (!rectEnv.contains(geom.getEnvelopeInternal)) return false

    /**
     * Check that geom is not contained entirely in the rectangle boundary. According to the
     * somewhat odd spec of the SFS, if this is the case the geometry is NOT contained.
     */
    if (isContainedInBoundary(geom)) return false
    true
  }

  private def isContainedInBoundary(geom: Geometry): Boolean = { // polygons can never be wholely contained in the boundary
    if (geom.isInstanceOf[Polygon]) return false
    if (geom.isInstanceOf[Point]) return isPointContainedInBoundary(geom.asInstanceOf[Point])
    if (geom.isInstanceOf[LineString])
      return isLineStringContainedInBoundary(geom.asInstanceOf[LineString])
    var i = 0
    while (i < geom.getNumGeometries) {
      val comp = geom.getGeometryN(i)
      if (!isContainedInBoundary(comp)) return false
      i += 1
    }
    true
  }

  private def isPointContainedInBoundary(point: Point): Boolean = isPointContainedInBoundary(
    point.getCoordinate
  )

  /**
   * Tests if a point is contained in the boundary of the target rectangle.
   *
   * @param pt
   *   the point to test return true if the point is contained in the boundary
   */
  private def isPointContainedInBoundary(pt: Coordinate): Boolean =
    /**
     * contains = false iff the point is properly contained in the rectangle.
     *
     * This code assumes that the point lies in the rectangle envelope
     */
    (pt.x == rectEnv.getMinX) || (pt.x == rectEnv.getMaxX) || (pt.y == rectEnv.getMinY) || (pt.y == rectEnv.getMaxY)

  /**
   * Tests if a linestring is completely contained in the boundary of the target rectangle.
   *
   * @param line
   *   the linestring to test return true if the linestring is contained in the boundary
   */
  private def isLineStringContainedInBoundary(line: LineString): Boolean = {
    val seq = line.getCoordinateSequence
    val p0  = new Coordinate
    val p1  = new Coordinate
    var i   = 0
    while (i < seq.size - 1) {
      seq.getCoordinate(i, p0)
      seq.getCoordinate(i + 1, p1)
      if (!isLineSegmentContainedInBoundary(p0, p1)) return false
      i += 1
    }
    true
  }

  /**
   * Tests if a line segment is contained in the boundary of the target rectangle.
   *
   * @param p0
   *   an endpoint of the segment
   * @param p1
   *   an endpoint of the segment return true if the line segment is contained in the boundary
   */
  private def isLineSegmentContainedInBoundary(p0: Coordinate, p1: Coordinate): Boolean = {
    if (p0 == p1) return isPointContainedInBoundary(p0)
    // we already know that the segment is contained in the rectangle envelope
    if (p0.x == p1.x)
      if ((p0.x == rectEnv.getMinX) || (p0.x == rectEnv.getMaxX)) return true
      else if (p0.y == p1.y) if ((p0.y == rectEnv.getMinY) || (p0.y == rectEnv.getMaxY)) return true

    /**
     * Either both x and y values are different or one of x and y are the same, but the other
     * ordinate is not the same as a boundary ordinate
     *
     * In either case, the segment is not wholely in the boundary
     */
    false
  }
}
