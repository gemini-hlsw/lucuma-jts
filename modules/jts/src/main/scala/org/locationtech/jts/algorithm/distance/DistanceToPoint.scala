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
package org.locationtech.jts.algorithm.distance

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryCollection
import org.locationtech.jts.geom.LineSegment
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.Polygon

/**
 * Computes the Euclidean distance (L2 metric) from a {link Coordinate} to a {link Geometry}.
 * Also computes two points on the geometry which are separated by the distance found.
 */
object DistanceToPoint {
  def computeDistance(geom: Geometry, pt: Coordinate, ptDist: PointPairDistance): Unit =
    geom match {
      case string: LineString     => computeDistance(string, pt, ptDist)
      case polygon: Polygon       => computeDistance(polygon, pt, ptDist)
      case gc: GeometryCollection =>
        var i = 0
        while (i < gc.getNumGeometries) {
          val g = gc.getGeometryN(i)
          computeDistance(g, pt, ptDist)
          i += 1
        }
      case _                      => // assume geom is Point
        ptDist.setMinimum(geom.getCoordinate, pt)
    }

  def computeDistance(line: LineString, pt: Coordinate, ptDist: PointPairDistance): Unit = {
    val tempSegment = new LineSegment
    val coords      = line.getCoordinates
    var i           = 0
    while (i < coords.length - 1) {
      tempSegment.setCoordinates(coords(i), coords(i + 1))
      // this is somewhat inefficient - could do better
      val closestPt = tempSegment.closestPoint(pt)
      ptDist.setMinimum(closestPt, pt)
      i += 1
    }
  }

  def computeDistance(segment: LineSegment, pt: Coordinate, ptDist: PointPairDistance): Unit = {
    val closestPt = segment.closestPoint(pt)
    ptDist.setMinimum(closestPt, pt)
  }

  def computeDistance(poly: Polygon, pt: Coordinate, ptDist: PointPairDistance): Unit = {
    computeDistance(poly.getExteriorRing, pt, ptDist)
    var i = 0
    while (i < poly.getNumInteriorRing) {
      computeDistance(poly.getInteriorRingN(i), pt, ptDist)
      i += 1
    }
  }
}

class DistanceToPoint() {}
