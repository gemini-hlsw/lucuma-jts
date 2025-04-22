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
package org.locationtech.jts.operation.valid

import org.locationtech.jts.algorithm.PointLocation
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.LinearRing
import org.locationtech.jts.geomgraph.GeometryGraph
import org.locationtech.jts.index.SpatialIndex
import org.locationtech.jts.index.strtree.STRtree

import java.util

/**
 * Tests whether any of a set of {link LinearRing}s are nested inside another ring in the set, using
 * a spatial index to speed up the comparisons.
 *
 * @version 1.7
 */
class IndexedNestedRingTester(
  var graph: GeometryGraph // used to find non-node vertices
) {
  private val rings                    = new util.ArrayList[LinearRing]
  private val totalEnv                 = new Envelope
  private var index: SpatialIndex[Any] = null
  private var nestedPt: Coordinate     = null

  def getNestedPoint: Coordinate = nestedPt

  def add(ring: LinearRing): Unit = {
    rings.add(ring)
    totalEnv.expandToInclude(ring.getEnvelopeInternal)
  }

  def isNonNested: Boolean = {
    buildIndex()
    var i = 0
    while (i < rings.size) {
      val innerRing    = rings.get(i).asInstanceOf[LinearRing]
      val innerRingPts = innerRing.getCoordinates
      val results      = index.query(innerRing.getEnvelopeInternal)
      // System.out.println(results.size());
      var j            = 0
      while (j < results.size) {
        val searchRing    = results.get(j).asInstanceOf[LinearRing]
        val searchRingPts = searchRing.getCoordinates
        if (innerRing != searchRing) {
          if (innerRing.getEnvelopeInternal.intersects(searchRing.getEnvelopeInternal)) {
            val innerRingPt = IsValidOp.findPtNotNode(innerRingPts, searchRing, graph)

            /**
             * If no non-node pts can be found, this means that the searchRing touches ALL of the
             * innerRing vertices. This indicates an invalid polygon, since either the two holes
             * create a disconnected interior, or they touch in an infinite number of points (i.e.
             * along a line segment). Both of these cases are caught by other tests, so it is safe
             * to simply skip this situation here.
             */
            if (innerRingPt != null) {
              val isInside = PointLocation.isInRing(innerRingPt, searchRingPts)
              if (isInside) {
                nestedPt = innerRingPt
                return false
              }
            }
          }
        }
        j += 1
      }
      i += 1
    }
    true
  }

  /**
   * An implementation of an optimization introduced in GEOS
   * https://github.com/libgeos/geos/pull/255/commits/1bf16cdf5a4827b483a1f712e0597ccb243f58cb
   *
   * Not used for now, since improvement is small and very data-dependent.
   *
   * return
   */
  /*
    private boolean isNonNestedWithIndex()
    {
      buildIndex();

      for (int i = 0; i < rings.size(); i++) {
        LinearRing outerRing = (LinearRing) rings.get(i);
        Coordinate[] outerRingPts = outerRing.getCoordinates();

        IndexedPointInAreaLocator ptLocator = new IndexedPointInAreaLocator(outerRing);
        List results = index.query(outerRing.getEnvelopeInternal());
  //System.out.println(results.size());
        for (int j = 0; j < results.size(); j++) {
          LinearRing searchRing = (LinearRing) results.get(j);
          if (outerRing == searchRing)
            continue;

          if (! outerRing.getEnvelopeInternal().intersects(searchRing.getEnvelopeInternal()))
            continue;

          Coordinate[] searchRingPts = searchRing.getCoordinates();
          Coordinate innerRingPt = IsValidOp.findPtNotNode(searchRingPts, outerRing, graph);

          if (innerRingPt == null)
            continue;

          boolean isInside = Location.EXTERIOR != ptLocator.locate(innerRingPt);
          //boolean isInside = PointLocation.isInRing(innerRingPt, outerRingPts);

          if (isInside) {
            nestedPt = innerRingPt;
            return false;
          }
        }
      }
      return true;
    }
   */
  private def buildIndex(): Unit = {
    index = new STRtree
    var i = 0
    while (i < rings.size) {
      val ring = rings.get(i)
      val env  = ring.getEnvelopeInternal
      index.insert(env, ring)
      i += 1
    }
  }
}
