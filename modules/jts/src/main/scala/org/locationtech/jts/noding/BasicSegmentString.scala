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
import org.locationtech.jts.geom.impl.CoordinateArraySequence
//import org.locationtech.jts.io.WKTWriter

/**
 * Represents a list of contiguous line segments, and supports noding the segments. The line
 * segments are represented by an array of {link Coordinate}s. Intended to optimize the noding of
 * contiguous segments by reducing the number of allocated objects. SegmentStrings can carry a
 * context object, which is useful for preserving topological or parentage information. All noded
 * substrings are initialized with the same context object.
 *
 * @version 1.7
 */
class BasicSegmentString(var pts: Array[Coordinate], var data: Any)

/**
 * Creates a new segment string from a list of vertices.
 *
 * @param pts  the vertices of the segment string
 * @param data the user-defined data of this segment string (may be null)
 */ extends SegmentString {

  /**
   * Gets the user-defined data for this segment string.
   *
   * return the user-defined data
   */
  override def getData: Any = data

  /**
   * Sets the user-defined data for this segment string.
   *
   * @param data
   *   an Object containing user-defined data
   */
  override def setData(data: Any): Unit = this.data = data

  override def size: Int = pts.length

  override def getCoordinate(i: Int): Coordinate = pts(i)

  override def getCoordinates: Array[Coordinate] = pts

  override def isClosed: Boolean = pts(0) == pts(pts.length - 1)

  /**
   * Gets the octant of the segment starting at vertex <code>index</code>.
   *
   * @param index
   *   the index of the vertex starting the segment. Must not be the last index in the vertex list
   *   return the octant of the segment at the vertex
   */
  def getSegmentOctant(index: Int): Int = {
    if (index == pts.length - 1) return -1
    Octant.octant(getCoordinate(index), getCoordinate(index + 1))
  }

  override def toString: String         = (new CoordinateArraySequence(pts)).toString
}
