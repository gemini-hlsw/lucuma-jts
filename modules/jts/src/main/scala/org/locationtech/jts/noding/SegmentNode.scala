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
package org.locationtech.jts.noding

import org.locationtech.jts.geom.Coordinate

import java.io.PrintStream

/**
 * Represents an intersection point between two {link SegmentString}s.
 *
 * @version 1.7
 */
class SegmentNode(
  val segString:     NodedSegmentString,
  val coordArg:      Coordinate,
  val segmentIndex:  Int // the index of the containing line segment in the parent edge
  ,
  val segmentOctant: Int
) extends Comparable[SegmentNode] {
  final var coord: Coordinate   = new Coordinate(coordArg)
  final private val visInterior = !coord.equals2D(segString.getCoordinate(segmentIndex))

  /**
   * Gets the {link Coordinate} giving the location of this node.
   *
   * return the coordinate of the node
   */
  def getCoordinate: Coordinate = coord

  def isInterior: Boolean = visInterior

  def isEndPoint(maxSegmentIndex: Int): Boolean = {
    if (segmentIndex == 0 && !visInterior) return true
    if (segmentIndex == maxSegmentIndex) return true
    false
  }

  /**
   * return -1 this SegmentNode is located before the argument location; 0 this SegmentNode is at
   * the argument location; 1 this SegmentNode is located after the argument location
   */
  override def compareTo(other: SegmentNode): Int = {
    if (segmentIndex < other.segmentIndex) return -1
    if (segmentIndex > other.segmentIndex) return 1
    if (coord.equals2D(other.coord)) return 0
    // an exterior node is the segment start point, so always sorts first
    // this guards against a robustness problem where the octants are not reliable
    if (!visInterior) return -1
    if (!other.visInterior) return 1
    SegmentPointComparator.compare(segmentOctant, coord, other.coord)
    // return segment.compareNodePosition(this, other);
  }

  def print(out: PrintStream): Unit = {
    out.print(coord)
    out.print(" seg # = " + segmentIndex)
  }

  override def toString: String = s"$segmentIndex:${coord.toString}"
}
