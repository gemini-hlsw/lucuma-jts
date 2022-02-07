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
import org.locationtech.jts.geom.GeometryCollectionIterator
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.LinearRing
import org.locationtech.jts.geom.Location
import org.locationtech.jts.geom.MultiLineString
import org.locationtech.jts.geom.MultiPolygon
import org.locationtech.jts.geom.Point
import org.locationtech.jts.geom.Polygon

/**
 * Computes the topological ({link Location}) of a single point to a {link Geometry}. A {link
 * BoundaryNodeRule} may be specified to control the evaluation of whether the point lies on the
 * boundary or not The default rule is to use the the <i>SFS Boundary Determination Rule</i> <p>
 * Notes: <ul> <li>{link LinearRing}s do not enclose any area - points inside the ring are still in
 * the EXTERIOR of the ring. </ul> Instances of this class are not reentrant.
 *
 * @version 1.7
 */
class PointLocator(boundaryRule: BoundaryNodeRule = BoundaryNodeRule.OGC_SFS_BOUNDARY_RULE) {
  // default is to use OGC SFS rule
  private var isIn          = false // true if the point lies in or on any Geometry element
  private var numBoundaries = 0     // the number of sub-elements whose boundaries the point lies in

  /**
   * Convenience method to test a point for intersection with a Geometry
   *
   * @param p
   *   the coordinate to test
   * @param geom
   *   the Geometry to test return <code>true</code> if the point is in the interior or boundary of
   *   the Geometry
   */
  def intersects(p: Coordinate, geom: Geometry) = locate(p, geom) != Location.EXTERIOR

  /**
   * Computes the topological relationship ({link Location}) of a single point to a Geometry. It
   * handles both single-element and multi-element Geometries. The algorithm for multi-part
   * Geometries takes into account the SFS Boundary Determination Rule.
   *
   * return the { @link Location} of the point relative to the input Geometry
   */
  def locate(p: Coordinate, geom: Geometry): Int = {
    if (geom.isEmpty) return Location.EXTERIOR
    if (geom.isInstanceOf[LineString]) return locateOnLineString(p, geom.asInstanceOf[LineString])
    else if (geom.isInstanceOf[Polygon]) return locateInPolygon(p, geom.asInstanceOf[Polygon])
    isIn = false
    numBoundaries = 0
    computeLocation(p, geom)
    if (boundaryRule.isInBoundary(numBoundaries)) return Location.BOUNDARY
    if (numBoundaries > 0 || isIn) return Location.INTERIOR
    Location.EXTERIOR
  }

  private def computeLocation(p: Coordinate, geom: Geometry): Unit = {
    if (geom.isInstanceOf[Point]) updateLocationInfo(locateOnPoint(p, geom.asInstanceOf[Point]))
    if (geom.isInstanceOf[LineString])
      updateLocationInfo(locateOnLineString(p, geom.asInstanceOf[LineString]))
    else if (geom.isInstanceOf[Polygon])
      updateLocationInfo(locateInPolygon(p, geom.asInstanceOf[Polygon]))
    else if (geom.isInstanceOf[MultiLineString]) {
      val ml = geom.asInstanceOf[MultiLineString]
      var i  = 0
      while (i < ml.getNumGeometries) {
        val l = ml.getGeometryN(i).asInstanceOf[LineString]
        updateLocationInfo(locateOnLineString(p, l))
        i += 1
      }
    } else if (geom.isInstanceOf[MultiPolygon]) {
      val mpoly = geom.asInstanceOf[MultiPolygon]
      var i     = 0
      while (i < mpoly.getNumGeometries) {
        val poly = mpoly.getGeometryN(i).asInstanceOf[Polygon]
        updateLocationInfo(locateInPolygon(p, poly))
        i += 1
      }
    } else if (geom.isInstanceOf[GeometryCollection]) {
      val geomi = new GeometryCollectionIterator(geom.asInstanceOf[GeometryCollection])
      while (geomi.hasNext) {
        val g2 = geomi.next.asInstanceOf[Geometry]
        if (g2 ne geom) computeLocation(p, g2)
      }
    }
  }

  private def updateLocationInfo(loc: Int): Unit = {
    if (loc == Location.INTERIOR) isIn = true
    if (loc == Location.BOUNDARY) {
      numBoundaries += 1
    }
  }

  private def locateOnPoint(p: Coordinate, pt: Point): Int = { // no point in doing envelope test, since equality test is just as fast
    val ptCoord = pt.getCoordinate
    if (ptCoord.equals2D(p)) return Location.INTERIOR
    Location.EXTERIOR
  }

  private def locateOnLineString(p: Coordinate, l: LineString): Int = { // bounding-box check
    if (!l.getEnvelopeInternal.intersects(p)) return Location.EXTERIOR
    val seq = l.getCoordinateSequence
    if (!l.isClosed)
      if (p == seq.getCoordinate(0) || p == seq.getCoordinate(seq.size - 1))
        return Location.BOUNDARY
    if (PointLocation.isOnLine(p, seq)) return Location.INTERIOR
    Location.EXTERIOR
  }

  private def locateInPolygonRing(p: Coordinate, ring: LinearRing): Int = {
    if (!ring.getEnvelopeInternal.intersects(p)) return Location.EXTERIOR
    PointLocation.locateInRing(p, ring.getCoordinates)
  }

  private def locateInPolygon(p: Coordinate, poly: Polygon): Int = {
    if (poly.isEmpty) return Location.EXTERIOR
    val shell    = poly.getExteriorRing
    val shellLoc = locateInPolygonRing(p, shell)
    if (shellLoc == Location.EXTERIOR) return Location.EXTERIOR
    if (shellLoc == Location.BOUNDARY) return Location.BOUNDARY
    // now test if the point lies in or on the holes
    var i        = 0
    while (i < poly.getNumInteriorRing) {
      val hole    = poly.getInteriorRingN(i)
      val holeLoc = locateInPolygonRing(p, hole)
      if (holeLoc == Location.INTERIOR) return Location.EXTERIOR
      if (holeLoc == Location.BOUNDARY) return Location.BOUNDARY
      i += 1
    }
    Location.INTERIOR
  }
}
