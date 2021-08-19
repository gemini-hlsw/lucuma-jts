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
package org.locationtech.jts.geomgraph

import java.io.PrintStream
import java.util

import org.locationtech.jts.geom.Coordinate

import scala.collection.mutable
import scala.jdk.CollectionConverters._

/**
 * A list of edge intersections along an {link Edge}. Implements splitting an edge with
 * intersections into multiple resultant edges.
 *
 * @version 1.7
 */
class EdgeIntersectionList(
  var edge: Edge // the parent edge
) {
  // a Map <EdgeIntersection, EdgeIntersection>
  private val nodeMap = mutable.TreeMap.empty[EdgeIntersection, EdgeIntersection]

  /**
   * Adds an intersection into the list, if it isn't already there. The input segmentIndex and dist
   * are expected to be normalized.
   *
   * return the EdgeIntersection found or added
   */
  def add(intPt: Coordinate, segmentIndex: Int, dist: Double): EdgeIntersection = {
    val eiNew = new EdgeIntersection(intPt, segmentIndex, dist)
    val ei    = nodeMap.get(eiNew).orNull
    if (ei != null) return ei
    nodeMap.put(eiNew, eiNew)
    eiNew
  }

  /**
   * Returns an iterator of {link EdgeIntersection}s
   *
   * return an Iterator of EdgeIntersections
   */
  def iterator: util.Iterator[EdgeIntersection] = nodeMap.values.iterator.asJava

  /**
   * Tests if the given point is an edge intersection
   *
   * @param pt
   *   the point to test return true if the point is an intersection
   */
  def isIntersection(pt: Coordinate): Boolean = {
    val it = iterator
    while (it.hasNext) {
      val ei = it.next
      if (ei.coord.equals(pt)) return true
    }
    false
  }

  /**
   * Adds entries for the first and last points of the edge to the list
   */
  def addEndpoints(): EdgeIntersection = {
    val maxSegIndex = edge.pts.length - 1
    add(edge.pts(0), 0, 0.0)
    add(edge.pts(maxSegIndex), maxSegIndex, 0.0)
  }

  /**
   * Creates new edges for all the edges that the intersections in this list split the parent edge
   * into. Adds the edges to the input list (this is so a single list can be used to accumulate all
   * split edges for a Geometry).
   *
   * @param edgeList
   *   a list of EdgeIntersections
   */
  def addSplitEdges(edgeList: util.List[Edge]): Unit = { // ensure that the list has entries for the first and last point of the edge
    addEndpoints()
    val it     = iterator
    // there should always be at least two entries in the list
    var eiPrev = it.next
    while (it.hasNext) {
      val ei      = it.next
      val newEdge = createSplitEdge(eiPrev, ei)
      edgeList.add(newEdge)
      eiPrev = ei
    }
  }

  /**
   * Create a new "split edge" with the section of points between (and including) the two
   * intersections. The label for the new edge is the same as the label for the parent edge.
   */
  private[geomgraph] def createSplitEdge(ei0: EdgeIntersection, ei1: EdgeIntersection) = { //Debug.print("\ncreateSplitEdge"); Debug.print(ei0); Debug.print(ei1);
    var npts           = ei1.segmentIndex - ei0.segmentIndex + 2
    val lastSegStartPt = edge.pts(ei1.segmentIndex)
    // if the last intersection point is not equal to the its segment start pt,
    // add it to the points list as well.
    // (This check is needed because the distance metric is not totally reliable!)
    // The check for point equality is 2D only - Z values are ignored
    val useIntPt1      = ei1.dist > 0.0 || !ei1.coord.equals2D(lastSegStartPt)
    if (!useIntPt1) npts -= 1
    val pts            = new Array[Coordinate](npts)
    var ipt            = 0
    pts {
      ipt += 1; ipt - 1
    } = new Coordinate(ei0.coord)
    var i              = ei0.segmentIndex + 1
    while (i <= ei1.segmentIndex) {
      pts {
        ipt += 1; ipt - 1
      } = edge.pts(i)
      i += 1
    }
    if (useIntPt1) pts(ipt) = ei1.coord
    new Edge(pts, new Label(edge.label))
  }

  def print(out: PrintStream): Unit = {
    out.println("Intersections:")
    val it = iterator
    while (it.hasNext) {
      val ei = it.next
      ei.print(out)
    }
  }
}
