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
 */
package org.locationtech.jts.geomgraph

import org.locationtech.jts.geom.Coordinate

import java.io.PrintStream

/**
 * Represents a point on an edge which intersects with another edge. <p> The intersection may either
 * be a single point, or a line segment (in which case this point is the start of the line segment)
 * The intersection point must be precise.
 *
 * @version 1.7
 */
class EdgeIntersection(
  val coordArg:     Coordinate,
  var segmentIndex: Int    // the index of the containing line segment in the parent edge
  ,
  var dist:         Double // the edge distance of this point along the containing line segment
) extends Comparable[EdgeIntersection] {
  var coord                     = new Coordinate(coordArg)
  def getCoordinate: Coordinate = coord

  def getSegmentIndex: Int = segmentIndex

  def getDistance: Double = dist

  override def compareTo(other: EdgeIntersection): Int =
    compare(other.segmentIndex, other.dist)

  /**
   * return -1 this EdgeIntersection is located before the argument location return 0 this
   * EdgeIntersection is at the argument location return 1 this EdgeIntersection is located after
   * the argument location
   */
  def compare(segmentIndex: Int, dist: Double): Int = {
    if (this.segmentIndex < segmentIndex) return -1
    if (this.segmentIndex > segmentIndex) return 1
    if (this.dist < dist) return -1
    if (this.dist > dist) return 1
    0
  }

  def isEndPoint(maxSegmentIndex: Int): Boolean = {
    if (segmentIndex == 0 && dist == 0.0) return true
    if (segmentIndex == maxSegmentIndex) return true
    false
  }

  def print(out: PrintStream): Unit = {
    out.print(coord)
    out.print(" seg # = " + segmentIndex)
    out.println(" dist = " + dist)
  }

  override def toString: String = s"$coord seg # = $segmentIndex dist = $dist"
}
