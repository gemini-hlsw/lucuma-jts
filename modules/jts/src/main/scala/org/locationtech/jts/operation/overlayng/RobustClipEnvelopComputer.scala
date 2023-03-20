// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.operation.overlayng

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateSequence
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryCollection
import org.locationtech.jts.geom.LinearRing
import org.locationtech.jts.geom.Polygon

/*
 * Copyright (c) 2019 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v20.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */

/**
 * Computes a robust clipping envelope for a pair of polygonal geometries. The envelope is computed
 * to be large enough to include the full length of all geometry line segments which intersect a
 * given target envelope. This ensures that line segments which might intersect are not perturbed
 * when clipped using {@link RingClipper}.
 *
 * @author
 *   Martin Davis
 */
object RobustClipEnvelopeComputer {
  def getEnvelope(a: Geometry, b: Geometry, targetEnv: Envelope): Envelope = {
    val cec = new RobustClipEnvelopeComputer(targetEnv)
    cec.add(a)
    cec.add(b)
    cec.getEnvelope
  }

  private def intersectsSegment(env: Envelope, p1: Coordinate, p2: Coordinate) =
    /**
     * This is a crude test of whether segment intersects envelope. It could be refined by checking
     * exact intersection. This could be based on the algorithm in the HotPixel.intersectsScaled
     * method.
     */
    env.intersects(p1, p2)
}

class RobustClipEnvelopeComputer(var targetEnv: Envelope) {
  private var clipEnv = targetEnv.copy

  def getEnvelope: Envelope = clipEnv

  def add(g: Geometry): Unit = {
    if (g == null || g.isEmpty) return
    if (g.isInstanceOf[Polygon]) addPolygon(g.asInstanceOf[Polygon])
    else if (g.isInstanceOf[GeometryCollection]) addCollection(g.asInstanceOf[GeometryCollection])
  }

  private def addCollection(gc: GeometryCollection): Unit =
    for (i <- 0 until gc.getNumGeometries) {
      val g = gc.getGeometryN(i)
      add(g)
    }

  private def addPolygon(poly: Polygon): Unit = {
    val shell = poly.getExteriorRing
    addPolygonRing(shell)
    for (i <- 0 until poly.getNumInteriorRing) {
      val hole = poly.getInteriorRingN(i)
      addPolygonRing(hole)
    }
  }

  /**
   * Adds a polygon ring to the graph. Empty rings are ignored.
   */
  private def addPolygonRing(ring: LinearRing): Unit = { // don't add empty lines
    if (ring.isEmpty) return
    val seq = ring.getCoordinateSequence
    for (i <- 1 until seq.size)
      addSegment(seq.getCoordinate(i - 1), seq.getCoordinate(i))
  }

  private def addSegment(p1: Coordinate, p2: Coordinate): Unit =
    if (RobustClipEnvelopeComputer.intersectsSegment(targetEnv, p1, p2)) {
      clipEnv.expandToInclude(p1)
      clipEnv.expandToInclude(p2)
    }
}
