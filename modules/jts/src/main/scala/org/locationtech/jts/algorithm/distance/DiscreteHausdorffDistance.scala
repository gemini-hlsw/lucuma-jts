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
package org.locationtech.jts.algorithm.distance

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateFilter
import org.locationtech.jts.geom.CoordinateSequence
import org.locationtech.jts.geom.CoordinateSequenceFilter
import org.locationtech.jts.geom.Geometry

/**
 * An algorithm for computing a distance metric which is an approximation to the Hausdorff Distance
 * based on a discretization of the input {link Geometry}. The algorithm computes the Hausdorff
 * distance restricted to discrete points for one of the geometries. The points can be either the
 * vertices of the geometries (the default), or the geometries with line segments densified by a
 * given fraction. Also determines two points of the Geometries which are separated by the computed
 * distance. <p> This algorithm is an approximation to the standard Hausdorff distance.
 * Specifically, <pre> for all geometries a, b: DHD(a, b) &lt;= HD(a, b) </pre> The approximation
 * can be made as close as needed by densifying the input geometries. In the limit, this value will
 * approach the true Hausdorff distance: <pre> DHD(A, B, densifyFactor) -&gt; HD(A, B) as
 * densifyFactor -&gt; 0.0 </pre> The default approximation is exact or close enough for a large
 * subset of useful cases. Examples of these are: <ul> <li>computing distance between Linestrings
 * that are roughly parallel to each other, and roughly equal in length. This occurs in matching
 * linear networks. <li>Testing similarity of geometries. </ul> An example where the default
 * approximation is not close is: <pre> A = LINESTRING (0 0, 100 0, 10 100, 10 100) B = LINESTRING
 * (0 100, 0 10, 80 10)
 *
 * DHD(A, B) = 22.360679774997898 HD(A, B) ~= 47.8 </pre>
 */
object DiscreteHausdorffDistance {
  def distance(g0: Geometry, g1: Geometry): Double = {
    val dist = new DiscreteHausdorffDistance(g0, g1)
    dist.distance
  }

  def distance(g0: Geometry, g1: Geometry, densifyFrac: Double): Double = {
    val dist = new DiscreteHausdorffDistance(g0, g1)
    dist.setDensifyFraction(densifyFrac)
    dist.distance
  }

  class MaxPointDistanceFilter(var geom: Geometry) extends CoordinateFilter {
    private val maxPtDist = new PointPairDistance
    private val minPtDist = new PointPairDistance
//    private val euclideanDist = new DistanceToPoint

    override def filter(pt: Coordinate): Unit = {
      minPtDist.initialize()
      DistanceToPoint.computeDistance(geom, pt, minPtDist)
      maxPtDist.setMaximum(minPtDist)
    }

    def getMaxPointDistance: PointPairDistance = maxPtDist
  }

  class MaxDensifiedByFractionDistanceFilter(var geom: Geometry, val fraction: Double)
      extends CoordinateSequenceFilter {
    private val maxPtDist  = new PointPairDistance
    private val minPtDist  = new PointPairDistance
    private var numSubSegs = 0
    numSubSegs = Math.rint(1.0 / fraction).toInt

    override def filter(seq: CoordinateSequence, index: Int): Unit = {

      /**
       * This logic also handles skipping Point geometries
       */
      if (index == 0) return
      val p0   = seq.getCoordinate(index - 1)
      val p1   = seq.getCoordinate(index)
      val delx = (p1.x - p0.x) / numSubSegs
      val dely = (p1.y - p0.y) / numSubSegs
      var i    = 0
      while (i < numSubSegs) {
        val x  = p0.x + i * delx
        val y  = p0.y + i * dely
        val pt = new Coordinate(x, y)
        minPtDist.initialize()
        DistanceToPoint.computeDistance(geom, pt, minPtDist)
        maxPtDist.setMaximum(minPtDist)
        i += 1
      }
    }

    override def isGeometryChanged = false

    override def isDone = false

    def getMaxPointDistance: PointPairDistance = maxPtDist
  }

}

class DiscreteHausdorffDistance(var g0: Geometry, var g1: Geometry) {
  private val ptDist = new PointPairDistance

  /**
   * Value of 0.0 indicates that no densification should take place
   */
  private var densifyFrac = 0.0

  /**
   * Sets the fraction by which to densify each segment. Each segment will be (virtually) split into
   * a number of equal-length subsegments, whose fraction of the total length is closest to the
   * given fraction.
   *
   * @param densifyFrac
   */
  def setDensifyFraction(densifyFrac: Double): Unit = {
    if (densifyFrac > 1.0 || densifyFrac <= 0.0)
      throw new IllegalArgumentException("Fraction is not in range (0.0 - 1.0]")
    this.densifyFrac = densifyFrac
  }

  def distance: Double = {
    compute(g0, g1)
    ptDist.getDistance
  }

  def orientedDistance: Double = {
    computeOrientedDistance(g0, g1, ptDist)
    ptDist.getDistance
  }

  def getCoordinates: Array[Coordinate] = ptDist.getCoordinates

  private def compute(g0: Geometry, g1: Geometry): Unit = {
    computeOrientedDistance(g0, g1, ptDist)
    computeOrientedDistance(g1, g0, ptDist)
  }

  private def computeOrientedDistance(
    discreteGeom: Geometry,
    geom:         Geometry,
    ptDist:       PointPairDistance
  ): Unit = {
    val distFilter = new DiscreteHausdorffDistance.MaxPointDistanceFilter(geom)
    discreteGeom.applyF(distFilter)
    ptDist.setMaximum(distFilter.getMaxPointDistance)
    if (densifyFrac > 0) {
      val fracFilter =
        new DiscreteHausdorffDistance.MaxDensifiedByFractionDistanceFilter(geom, densifyFrac)
      discreteGeom.applyF(fracFilter)
      ptDist.setMaximum(fracFilter.getMaxPointDistance)
    }
  }
}
