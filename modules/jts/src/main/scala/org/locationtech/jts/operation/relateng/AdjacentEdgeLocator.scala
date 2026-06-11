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

import org.locationtech.jts.algorithm.PointLocation
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Dimension
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryCollection
import org.locationtech.jts.geom.LinearRing
import org.locationtech.jts.geom.Location
import org.locationtech.jts.geom.Polygon

import java.util

/**
 * Determines the location for a point which is known to lie on at least one edge of a set of
 * polygons. This provides the union-semantics for determining point location in a
 * GeometryCollection, which may have polygons with adjacent edges which are effectively in the
 * interior of the geometry. Note that it is also possible to have adjacent edges which lie on the
 * boundary of the geometry (e.g. a polygon contained within another polygon with adjacent edges).
 *
 * @author
 *   mdavis
 */
class AdjacentEdgeLocator(geom: Geometry) {

  private var ringList: util.List[Array[Coordinate]] = null

  init(geom)

  def locate(p: Coordinate): Int = {
    val sections = new NodeSections(p)
    val it       = ringList.iterator
    while (it.hasNext) {
      val ring = it.next()
      addSections(p, ring, sections)
    }
    val node     = sections.createNode()
    // node.finish(false, false);
    if (node.hasExteriorEdge(true)) Location.BOUNDARY else Location.INTERIOR
  }

  private def addSections(p: Coordinate, ring: Array[Coordinate], sections: NodeSections): Unit = {
    var i = 0
    while (i < ring.length - 1) {
      val p0    = ring(i)
      val pnext = ring(i + 1)

      if (p.equals2D(pnext)) {
        // -- segment final point is assigned to next segment
      } else if (p.equals2D(p0)) {
        val iprev = if (i > 0) i - 1 else ring.length - 2
        val pprev = ring(iprev)
        sections.addNodeSection(createSection(p, pprev, pnext))
      } else if (PointLocation.isOnSegment(p, p0, pnext)) {
        sections.addNodeSection(createSection(p, p0, pnext))
      }
      i += 1
    }
  }

  private def createSection(p: Coordinate, prev: Coordinate, next: Coordinate): NodeSection = {
    if (prev.distance(p) == 0 || next.distance(p) == 0) {
      System.out.println("Found zero-length section segment")
    }
    val ns = new NodeSection(true, Dimension.A, 1, 0, null, false, prev, p, next)
    ns
  }

  private def init(geom: Geometry): Unit = {
    if (geom.isEmpty)
      return
    ringList = new util.ArrayList[Array[Coordinate]]
    addRings(geom, ringList)
  }

  private def addRings(geom: Geometry, ringList2: util.List[Array[Coordinate]]): Unit =
    if (geom.isInstanceOf[Polygon]) {
      val poly  = geom.asInstanceOf[Polygon]
      val shell = poly.getExteriorRing
      addRing(shell, true)
      var i     = 0
      while (i < poly.getNumInteriorRing) {
        val hole = poly.getInteriorRingN(i)
        addRing(hole, false)
        i += 1
      }
    } else if (geom.isInstanceOf[GeometryCollection]) {
      // -- recurse through collections
      var i = 0
      while (i < geom.getNumGeometries) {
        addRings(geom.getGeometryN(i), ringList)
        i += 1
      }
    }

  private def addRing(ring: LinearRing, requireCW: Boolean): Unit = {
    // TODO: remove repeated points?
    val pts = RelateGeometry.orient(ring.getCoordinates, requireCW)
    ringList.add(pts)
    ()
  }

}
