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

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateArrays
import org.locationtech.jts.geom.Dimension
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.noding.BasicSegmentString

/**
 * Models a linear edge of a {link RelateGeometry}.
 *
 * @author
 *   mdavis
 */
object RelateSegmentString {

  def createLine(
    pts:       Array[Coordinate],
    isA:       Boolean,
    elementId: Int,
    parent:    RelateGeometry
  ): RelateSegmentString =
    createSegmentString(pts, isA, Dimension.L, elementId, -1, null, parent)

  def createRing(
    pts:       Array[Coordinate],
    isA:       Boolean,
    elementId: Int,
    ringId:    Int,
    poly:      Geometry,
    parent:    RelateGeometry
  ): RelateSegmentString =
    createSegmentString(pts, isA, Dimension.A, elementId, ringId, poly, parent)

  private def createSegmentString(
    pts:       Array[Coordinate],
    isA:       Boolean,
    dim:       Int,
    elementId: Int,
    ringId:    Int,
    poly:      Geometry,
    parent:    RelateGeometry
  ): RelateSegmentString = {
    val pts2 = removeRepeatedPoints(pts)
    new RelateSegmentString(pts2, isA, dim, elementId, ringId, poly, parent)
  }

  private def removeRepeatedPoints(pts: Array[Coordinate]): Array[Coordinate] = {
    if (CoordinateArrays.hasRepeatedPoints(pts)) {
      return CoordinateArrays.removeRepeatedPoints(pts)
    }
    pts
  }
}

class RelateSegmentString private (
  pts:                         Array[Coordinate],
  private val isAFlag:         Boolean,
  private val dimension:       Int,
  private val id:              Int,
  private val ringId:          Int,
  private val parentPolygonal: Geometry,
  private val inputGeom:       RelateGeometry
) extends BasicSegmentString(pts, null) {

  def isA: Boolean = isAFlag

  def getGeometry: RelateGeometry = inputGeom

  def getPolygonal: Geometry = parentPolygonal

  def createNodeSection(segIndex: Int, intPt: Coordinate): NodeSection = {
    val isNodeAtVertex =
      intPt.equals2D(getCoordinate(segIndex)) || intPt.equals2D(getCoordinate(segIndex + 1))
    val prev           = prevVertex(segIndex, intPt)
    val next           = nextVertex(segIndex, intPt)
    val a              =
      new NodeSection(isA,
                      dimension,
                      id,
                      ringId,
                      parentPolygonal,
                      isNodeAtVertex,
                      prev,
                      intPt,
                      next
      )
    a
  }

  /**
   * @param segIndex
   * @param pt
   *   return the previous vertex, or null if none exists
   */
  private def prevVertex(segIndex: Int, pt: Coordinate): Coordinate = {
    val segStart = getCoordinate(segIndex)
    if (!segStart.equals2D(pt))
      return segStart
    // -- pt is at segment start, so get previous vertex
    if (segIndex > 0)
      return getCoordinate(segIndex - 1)
    if (isClosed)
      return prevInRing(segIndex)
    null
  }

  /**
   * @param segIndex
   * @param pt
   *   return the next vertex, or null if none exists
   */
  private def nextVertex(segIndex: Int, pt: Coordinate): Coordinate = {
    val segEnd = getCoordinate(segIndex + 1)
    if (!segEnd.equals2D(pt))
      return segEnd
    // -- pt is at seg end, so get next vertex
    if (segIndex < size - 2)
      return getCoordinate(segIndex + 2)
    if (isClosed)
      return nextInRing(segIndex + 1)
    // -- segstring is not closed, so there is no next segment
    null
  }

  private def prevInRing(index: Int): Coordinate = {
    var prevIndex = index - 1
    if (prevIndex < 0) {
      prevIndex = size - 2
    }
    getCoordinate(prevIndex)
  }

  private def nextInRing(index: Int): Coordinate = {
    var nextIndex = index + 1
    if (nextIndex > size - 1) {
      nextIndex = 1
    }
    getCoordinate(nextIndex)
  }

  /**
   * Tests if a segment intersection point has that segment as its canonical containing segment.
   * Segments are half-closed, and contain their start point but not the endpoint, except for the
   * final segment in a non-closed segment string, which contains its endpoint as well. This test
   * ensures that vertices are assigned to a unique segment in a segment string. In particular, this
   * avoids double-counting intersections which lie exactly at segment endpoints.
   *
   * @param segIndex
   *   the segment the point may lie on
   * @param pt
   *   the point return true if the segment contains the point
   */
  def isContainingSegment(segIndex: Int, pt: Coordinate): Boolean = {
    // -- intersection is at segment start vertex - process it
    if (pt.equals2D(getCoordinate(segIndex)))
      return true
    if (pt.equals2D(getCoordinate(segIndex + 1))) {
      val isFinalSegment = segIndex == size - 2
      if (isClosed || !isFinalSegment)
        return false
      // -- for final segment, process intersections with final endpoint
      return true
    }
    // -- intersection is interior - process it
    true
  }

}
