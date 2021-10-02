/*
 * Copyright (c) 2016 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */ /*
 * Copyright (c) 2016 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.operation.distance

import org.locationtech.jts.algorithm.Distance
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateSequence
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.LineSegment

/**
 * Represents a sequence of facets (points or line segments) of a {link Geometry} specified by a
 * subsequence of a {link CoordinateSequence}.
 *
 * @author
 *   Martin Davis
 */
class FacetSequence(val geom: Geometry, val pts: CoordinateSequence, val start: Int, val end: Int) {
//  private var geom = null
//  private var pts = null
//  private var start = 0
//  private var end = 0

  /**
   * Creates a new sequence of facets based on a {link CoordinateSequence} contained in the given
   * {link Geometry}.
   *
   * @param geom
   *   the geometry containing the facets
   * @param pts
   *   the sequence containing the facet points
   * @param start
   *   the index of the start point
   * @param end
   *   the index of the end point + 1
   */
//  def this {
//    this()
//    this.geom = geom
//    this.pts = pts
//    this.start = start
//    this.end = end
//  }

  /**
   * Creates a new sequence of facets based on a {link CoordinateSequence}.
   *
   * @param pts
   *   the sequence containing the facet points
   * @param start
   *   the index of the start point
   * @param end
   *   the index of the end point + 1
   */
  def this(pts: CoordinateSequence, start: Int, end: Int) = {
    this(null, pts, start, end)
//    this.pts = pts
//    this.start = start
//    this.end = end
  }

  /**
   * Creates a new sequence for a single point from a {link CoordinateSequence}.
   *
   * @param pts
   *   the sequence containing the facet point
   * @param start
   *   the index of the point
   */
  def this(pts: CoordinateSequence, start: Int) = {
    this(null, pts, start, start + 1)
//    this.pts = pts
//    this.start = start
//    this.end = start + 1
  }

  def getEnvelope: Envelope = {
    val env = new Envelope
    var i   = start
    while (i < end) {
      env.expandToInclude(pts.getX(i), pts.getY(i))
      i += 1
    }
    env
  }

  def size: Int = end - start

  def getCoordinate(index: Int): Coordinate = pts.getCoordinate(start + index)

  def isPoint: Boolean = end - start == 1

  /**
   * Computes the distance between this and another <tt>FacetSequence</tt>.
   *
   * @param facetSeq
   *   the sequence to compute the distance to return the minimum distance between the sequences
   */
  def distance(facetSeq: FacetSequence): Double = {
    val visPoint     = isPoint
    val isPointOther = facetSeq.isPoint
    var distance     = .0
    if (visPoint && isPointOther) {
      val pt    = pts.getCoordinate(start)
      val seqPt = facetSeq.pts.getCoordinate(facetSeq.start)
      distance = pt.distance(seqPt)
    } else if (isPoint) {
      val pt = pts.getCoordinate(start)
      distance = computeDistancePointLine(pt, facetSeq, null)
    } else if (isPointOther) {
      val seqPt = facetSeq.pts.getCoordinate(facetSeq.start)
      distance = computeDistancePointLine(seqPt, this, null)
    } else distance = computeDistanceLineLine(facetSeq, null)
    distance
  }

  /**
   * Computes the locations of the nearest points between this sequence and another sequence. The
   * locations are presented in the same order as the input sequences.
   *
   * return a pair of { @link GeometryLocation}s for the nearest points
   */
  def nearestLocations(facetSeq: FacetSequence): Array[GeometryLocation] = {
    val visPoint     = isPoint
    val isPointOther = facetSeq.isPoint
    val locs         = new Array[GeometryLocation](2)
    if (visPoint && isPointOther) {
      val pt    = pts.getCoordinate(start)
      val seqPt = facetSeq.pts.getCoordinate(facetSeq.start)
      locs(0) = new GeometryLocation(geom, start, new Coordinate(pt))
      locs(1) = new GeometryLocation(facetSeq.geom, facetSeq.start, new Coordinate(seqPt))
    } else if (visPoint) {
      val pt = pts.getCoordinate(start)
      computeDistancePointLine(pt, facetSeq, locs)
    } else if (isPointOther) {
      val seqPt = facetSeq.pts.getCoordinate(facetSeq.start)
      computeDistancePointLine(seqPt, this, locs)
      // unflip the locations
      val tmp   = locs(0)
      locs(0) = locs(1)
      locs(1) = tmp
    } else computeDistanceLineLine(facetSeq, locs)
    locs
  }

  private def computeDistanceLineLine(
    facetSeq: FacetSequence,
    locs:     Array[GeometryLocation]
  ): Double = { // both linear - compute minimum segment-segment distance
    var minDistance = Double.MaxValue
    var i           = start
    while (i < end - 1) {
      val p0 = pts.getCoordinate(i)
      val p1 = pts.getCoordinate(i + 1)
      var j  = facetSeq.start
      while (j < facetSeq.end - 1) {
        val q0   = facetSeq.pts.getCoordinate(j)
        val q1   = facetSeq.pts.getCoordinate(j + 1)
        val dist = Distance.segmentToSegment(p0, p1, q0, q1)
        if (dist < minDistance) {
          minDistance = dist
          if (locs != null) updateNearestLocationsLineLine(i, p0, p1, facetSeq, j, q0, q1, locs)
          if (minDistance <= 0.0) return minDistance
        }
        j += 1
      }
      i += 1
    }
    minDistance
  }

  private def updateNearestLocationsLineLine(
    i:        Int,
    p0:       Coordinate,
    p1:       Coordinate,
    facetSeq: FacetSequence,
    j:        Int,
    q0:       Coordinate,
    q1:       Coordinate,
    locs:     Array[GeometryLocation]
  ): Unit = {
    val seg0      = new LineSegment(p0, p1)
    val seg1      = new LineSegment(q0, q1)
    val closestPt = seg0.closestPoints(seg1)
    locs(0) = new GeometryLocation(geom, i, new Coordinate(closestPt(0)))
    locs(1) = new GeometryLocation(facetSeq.geom, j, new Coordinate(closestPt(1)))
  }

  private def computeDistancePointLine(
    pt:       Coordinate,
    facetSeq: FacetSequence,
    locs:     Array[GeometryLocation]
  ): Double = {
    var minDistance = Double.MaxValue
    var i           = facetSeq.start
    while (i < facetSeq.end - 1) {
      val q0   = facetSeq.pts.getCoordinate(i)
      val q1   = facetSeq.pts.getCoordinate(i + 1)
      val dist = Distance.pointToSegment(pt, q0, q1)
      if (dist < minDistance) {
        minDistance = dist
        if (locs != null) updateNearestLocationsPointLine(pt, facetSeq, i, q0, q1, locs)
        if (minDistance <= 0.0) return minDistance
      }
      i += 1
    }
    minDistance
  }

  private def updateNearestLocationsPointLine(
    pt:       Coordinate,
    facetSeq: FacetSequence,
    i:        Int,
    q0:       Coordinate,
    q1:       Coordinate,
    locs:     Array[GeometryLocation]
  ): Unit = {
    locs(0) = new GeometryLocation(geom, start, new Coordinate(pt))
    val seg             = new LineSegment(q0, q1)
    val segClosestPoint = seg.closestPoint(pt)
    locs(1) = new GeometryLocation(facetSeq.geom, i, new Coordinate(segClosestPoint))
  }

  override def toString: String = {
    val buf = new StringBuffer
    buf.append("LINESTRING ( ")
    val p   = new Coordinate
    var i   = start
    while (i < end) {
      if (i > start) buf.append(", ")
      pts.getCoordinate(i, p)
      buf.append(s"${p.x} ${p.y}")
      i += 1
    }
    buf.append(" )")
    buf.toString
  }
}
