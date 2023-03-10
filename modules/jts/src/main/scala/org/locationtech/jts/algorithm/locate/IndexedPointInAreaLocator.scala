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
package org.locationtech.jts.algorithm.locate

import org.locationtech.jts.algorithm.RayCrossingCounter
import org.locationtech.jts.algorithm.locate.IndexedPointInAreaLocator.IntervalIndexedGeometry
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.LineSegment
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.LinearRing
import org.locationtech.jts.geom.Polygonal
import org.locationtech.jts.geom.util.LinearComponentExtracter
import org.locationtech.jts.index.ArrayListVisitor
import org.locationtech.jts.index.ItemVisitor
import org.locationtech.jts.index.intervaltree.SortedPackedIntervalRTree

import java.util

/**
 * Determines the {link Location} of {link Coordinate}s relative to an areal geometry, using
 * indexing for efficiency. This algorithm is suitable for use in cases where many points will be
 * tested against a given area. <p> The Location is computed precisely, in that points located on
 * the geometry boundary or segments will return {link Location.BOUNDARY}. <p> {link Polygonal} and
 * {link LinearRing} geometries are supported. <p> The index is lazy-loaded, which allows creating
 * instances even if they are not used. <p> Thread-safe and immutable.
 *
 * @author
 *   Martin Davis
 */
object IndexedPointInAreaLocator {

  private class SegmentVisitor(var counter: RayCrossingCounter) extends ItemVisitor {
    override def visitItem(item: Any): Unit = {
      val seg = item.asInstanceOf[LineSegment]
      counter.countSegment(seg.getCoordinate(0), seg.getCoordinate(1))
    }
  }

  private class IntervalIndexedGeometry(val geom: Geometry) {
    private var isEmpty = false
    if (geom.isEmpty) isEmpty = true
    else init(geom)
    private val index   = new SortedPackedIntervalRTree

    private def init(geom: Geometry): Unit = {
      val lines = LinearComponentExtracter.getLines(geom)
      val i     = lines.iterator
      while (i.hasNext) {
        val line = i.next.asInstanceOf[LineString]
        val pts  = line.getCoordinates
        addLine(pts)
      }
    }

    private def addLine(pts: Array[Coordinate]): Unit = {
      var i = 1
      while (i < pts.length) {
        val seg = new LineSegment(pts(i - 1), pts(i))
        val min = Math.min(seg.p0.y, seg.p1.y)
        val max = Math.max(seg.p0.y, seg.p1.y)
        index.insert(min, max, seg)
        i += 1
      }
    }

    def query(min: Double, max: Double): util.List[_] = {
      if (isEmpty) return new util.ArrayList()
      val visitor = new ArrayListVisitor
      index.query(min, max, visitor)
      visitor.getItems
    }

    def query(min: Double, max: Double, visitor: ItemVisitor): Unit = {
      if (isEmpty) return
      index.query(min, max, visitor)
    }
  }

}

class IndexedPointInAreaLocator(var geom: Geometry)

/**
 * Creates a new locator for a given {link Geometry}. {link Polygonal} and {link LinearRing}
 * geometries are supported.
 *
 * @param g
 *   the Geometry to locate in
 */
    extends PointOnGeometryLocator {
  if (!(geom.isInstanceOf[Polygonal] || geom.isInstanceOf[LinearRing]))
    throw new IllegalArgumentException("Argument must be Polygonal or LinearRing")
  private var index: IntervalIndexedGeometry = null

  /**
   * Determines the {link Location} of a point in an areal {link Geometry}.
   *
   * @param p
   *   the point to test return the location of the point in the geometry
   */
  override def locate(p: Coordinate): Int = {
    // avoid calling synchronized method improves performance
    if (index == null) createIndex()
    val rcc     = new RayCrossingCounter(p)
    val visitor = new IndexedPointInAreaLocator.SegmentVisitor(rcc)
    index.query(p.y, p.y, visitor)
    /*
         // MD - slightly slower alternative
        List segs = index.query(p.y, p.y);
        countSegs(rcc, segs);
     */
    rcc.getLocation
  }

  /**
   * Creates the indexed geometry, creating it if necessary.
   */
  def createIndex(): Unit = synchronized {
    if (index == null) {
      index = new IntervalIndexedGeometry(geom);
      // no need to hold onto geom
      geom = null;
    }
  }
}
