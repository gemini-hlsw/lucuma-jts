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
package org.locationtech.jts.noding

import java.util
import org.locationtech.jts.algorithm.LineIntersector
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.impl.CoordinateArraySequence
//import org.locationtech.jts.io.WKTWriter
import scala.jdk.CollectionConverters._

/**
 * Represents a list of contiguous line segments, and supports noding the segments. The line
 * segments are represented by an array of {link Coordinate}s. Intended to optimize the noding of
 * contiguous segments by reducing the number of allocated objects. SegmentStrings can carry a
 * context object, which is useful for preserving topological or parentage information. All noded
 * substrings are initialized with the same context object.
 *
 * @version 1.7
 */
object NodedSegmentString {

  /**
   * Gets the {link SegmentString}s which result from splitting this string at node points.
   *
   * @param segStrings
   *   a Collection of NodedSegmentStrings return a Collection of NodedSegmentStrings representing
   *   the substrings
   */
  def getNodedSubstrings(segStrings: util.Collection[SegmentString]): util.List[SegmentString] = {
    val resultEdgelist = new util.ArrayList[NodedSegmentString]
    getNodedSubstrings(segStrings, resultEdgelist)
    resultEdgelist.asScala.map(x => x: SegmentString).toList.asJava
  }

  /**
   * Adds the noded {link SegmentString}s which result from splitting this string at node points.
   *
   * @param segStrings
   *   a Collection of NodedSegmentStrings
   * @param resultEdgelist
   *   a List which will collect the NodedSegmentStrings representing the substrings
   */
  def getNodedSubstrings(
    segStrings:     util.Collection[SegmentString],
    resultEdgelist: util.Collection[NodedSegmentString]
  ): Unit = {
    val i = segStrings.iterator
    while (i.hasNext) {
      val ss = i.next.asInstanceOf[NodedSegmentString]
      ss.getNodeList.addSplitEdges(resultEdgelist)
    }
  }
}

class NodedSegmentString(var pts: Array[Coordinate], var data: Any)

/**
 * Creates a new segment string from a list of vertices.
 *
 * @param pts  the vertices of the segment string
 * @param data the user-defined data of this segment string (may be null)
 */ extends NodableSegmentString {
  private val nodeList = new SegmentNodeList(this)

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

  def getNodeList: SegmentNodeList = nodeList

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
    safeOctant(getCoordinate(index), getCoordinate(index + 1))
    //    return Octant.octant(getCoordinate(index), getCoordinate(index + 1));
  }

  private def safeOctant(p0: Coordinate, p1: Coordinate): Int = {
    if (p0.equals2D(p1)) return 0
    Octant.octant(p0, p1)
  }

  /**
   * Adds EdgeIntersections for one or both intersections found for a segment of an edge to the edge
   * intersection list.
   */
  def addIntersections(li: LineIntersector, segmentIndex: Int, geomIndex: Int): Unit = {
    var i = 0
    while (i < li.getIntersectionNum) {
      addIntersection(li, segmentIndex, geomIndex, i)
      i += 1
    }
  }

  /**
   * Add an SegmentNode for intersection intIndex. An intersection that falls exactly on a vertex of
   * the SegmentString is normalized to use the higher of the two possible segmentIndexes
   */
  def addIntersection(
    li:           LineIntersector,
    segmentIndex: Int,
    geomIndex:    Int,
    intIndex:     Int
  ): Unit = {
    val intPt = new Coordinate(li.getIntersection(intIndex))
    addIntersection(intPt, segmentIndex)
  }

  /**
   * Adds an intersection node for a given point and segment to this segment string.
   *
   * @param intPt
   *   the location of the intersection
   * @param segmentIndex
   *   the index of the segment containing the intersection
   */
  override def addIntersection(intPt: Coordinate, segmentIndex: Int): Unit = {
    addIntersectionNode(intPt, segmentIndex)
    ()
  }

  /**
   * Adds an intersection node for a given point and segment to this segment string. If an
   * intersection already exists for this exact location, the existing node will be returned.
   *
   * @param intPt
   *   the location of the intersection
   * @param segmentIndex
   *   the index of the segment containing the intersection return the intersection node for the
   *   point
   */
  def addIntersectionNode(intPt: Coordinate, segmentIndex: Int): SegmentNode = {
    var normalizedSegmentIndex = segmentIndex
    // Debug.println("edge intpt: " + intPt + " dist: " + dist);
    // normalize the intersection point location
    val nextSegIndex           = normalizedSegmentIndex + 1
    if (nextSegIndex < pts.length) {
      val nextPt = pts(nextSegIndex)
      // Debug.println("next pt: " + nextPt);
      // Normalize segment index if intPt falls on vertex
      // The check for point equality is 2D only - Z values are ignored
      if (intPt.equals2D(nextPt)) { // Debug.println("normalized distance");
        normalizedSegmentIndex = nextSegIndex
      }
    }

    /**
     * Add the intersection point to edge intersection list.
     */
    val ei = nodeList.add(intPt, normalizedSegmentIndex)
    ei
  }

//  override def toString: String = WKTWriter.toLineString(new CoordinateArraySequence(pts))
  override def toString: String = (new CoordinateArraySequence(pts)).toString
}
