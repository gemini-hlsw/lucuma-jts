// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
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
package org.locationtech.jts.operation.buffer.validate

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateFilter
import org.locationtech.jts.geom.CoordinateSequence
import org.locationtech.jts.geom.CoordinateSequenceFilter
import org.locationtech.jts.geom.Geometry

/**
 * Finds the approximate maximum distance from a buffer curve to the originating geometry. This is
 * similar to the Discrete Oriented Hausdorff distance from the buffer curve to the input. <p> The
 * approximate maximum distance is determined by testing all vertices in the buffer curve, as well
 * as midpoints of the curve segments. Due to the way buffer curves are constructed, this should be
 * a very close approximation.
 *
 * @author
 *   mbdavis
 */
object BufferCurveMaximumDistanceFinder {

  class MaxPointDistanceFilter(var geom: Geometry) extends CoordinateFilter {
    private val maxPtDist = new PointPairDistance
    private val minPtDist = new PointPairDistance

    override def filter(pt: Coordinate): Unit = {
      minPtDist.initialize()
      DistanceToPointFinder.computeDistance(geom, pt, minPtDist)
      maxPtDist.setMaximum(minPtDist)
    }

    def getMaxPointDistance: PointPairDistance = maxPtDist
  }

  class MaxMidpointDistanceFilter(var geom: Geometry) extends CoordinateSequenceFilter {
    private val maxPtDist = new PointPairDistance
    private val minPtDist = new PointPairDistance

    override def filter(seq: CoordinateSequence, index: Int): Unit = {
      if (index == 0) return
      val p0    = seq.getCoordinate(index - 1)
      val p1    = seq.getCoordinate(index)
      val midPt = new Coordinate((p0.x + p1.x) / 2, (p0.y + p1.y) / 2)
      minPtDist.initialize()
      DistanceToPointFinder.computeDistance(geom, midPt, minPtDist)
      maxPtDist.setMaximum(minPtDist)
    }

    override def isGeometryChanged = false

    override def isDone = false

    def getMaxPointDistance: PointPairDistance = maxPtDist
  }

}

class BufferCurveMaximumDistanceFinder(var inputGeom: Geometry) {
  private val maxPtDist = new PointPairDistance

  def findDistance(bufferCurve: Geometry): Double = {
    computeMaxVertexDistance(bufferCurve)
    computeMaxMidpointDistance(bufferCurve)
    maxPtDist.getDistance
  }

  def getDistancePoints: PointPairDistance = maxPtDist

  private def computeMaxVertexDistance(curve: Geometry): Unit = {
    val distFilter = new BufferCurveMaximumDistanceFinder.MaxPointDistanceFilter(inputGeom)
    curve.applyF(distFilter)
    maxPtDist.setMaximum(distFilter.getMaxPointDistance)
  }

  private def computeMaxMidpointDistance(curve: Geometry): Unit = {
    val distFilter = new BufferCurveMaximumDistanceFinder.MaxMidpointDistanceFilter(inputGeom)
    curve.applyF(distFilter)
    maxPtDist.setMaximum(distFilter.getMaxPointDistance)
  }
}
