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
package org.locationtech.jts.geomgraph

import org.locationtech.jts.algorithm.BoundaryNodeRule
import org.locationtech.jts.algorithm.locate.SimplePointInAreaLocator
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Location
import org.locationtech.jts.geom.Position
import org.locationtech.jts.geom.TopologyException
import org.locationtech.jts.util.Assert

import java.io.PrintStream
import java.util
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
 * A EdgeEndStar is an ordered list of EdgeEnds around a node. They are maintained in CCW order
 * (starting with the positive x-axis) around the node for efficient lookup and topology building.
 *
 * @version 1.7
 */
abstract class EdgeEndStar() {

  /**
   * A map which maintains the edges in sorted order around the node
   */
  protected var edgeMap = mutable.TreeMap.empty[EdgeEnd, EdgeEnd]

  /**
   * A list of all outgoing edges in the result, in CCW order
   */
  protected var edgeList: java.util.List[EdgeEnd] = null

  /**
   * The location of the point for this star in Geometry i Areas
   */
  private val ptInAreaLocation = Array(Location.NONE, Location.NONE)

  /**
   * Insert a EdgeEnd into this EdgeEndStar
   */
  def insert(e: EdgeEnd): Unit

  /**
   * Insert an EdgeEnd into the map, and clear the edgeList cache, since the list of edges has now
   * changed
   */
  protected def insertEdgeEnd(e: EdgeEnd, obj: EdgeEnd): Unit = {
    edgeMap.put(e, obj)
    edgeList = null // edge list has changed - clear the cache
  }

  /**
   * return the coordinate for the node this star is based at
   */
  def getCoordinate: Coordinate = {
    val it = iterator
    if (!it.hasNext) return null
    val e  = it.next
    e.getCoordinate
  }

  def getDegree: Int = edgeMap.size

  /**
   * Iterator access to the ordered list of edges is optimized by copying the map collection to a
   * list. (This assumes that once an iterator is requested, it is likely that insertion into the
   * map is complete).
   */
  def iterator: util.Iterator[EdgeEnd] = getEdges.iterator

  def getEdges: util.List[EdgeEnd] = {
    if (edgeList == null) edgeList = new util.ArrayList(edgeMap.values.toList.asJava)
    edgeList
  }

  def getNextCW(ee: EdgeEnd): EdgeEnd = {
    getEdges
    val i       = edgeList.indexOf(ee)
    var iNextCW = i - 1
    if (i == 0) iNextCW = edgeList.size - 1
    edgeList.get(iNextCW)
  }

  def computeLabelling(geomGraph: Array[GeometryGraph]): Unit = {
    computeEdgeEndLabels(geomGraph(0).getBoundaryNodeRule)
    // Propagate side labels  around the edges in the star
    // for each parent Geometry
    // Debug.print(this);
    propagateSideLabels(0)
    // Debug.printIfWatch(this);
    propagateSideLabels(1)

    /**
     * If there are edges that still have null labels for a geometry this must be because there are
     * no area edges for that geometry incident on this node. In this case, to label the edge for
     * that geometry we must test whether the edge is in the interior of the geometry. To do this it
     * suffices to determine whether the node for the edge is in the interior of an area. If so, the
     * edge has location INTERIOR for the geometry. In all other cases (e.g. the node is on a line,
     * on a point, or not on the geometry at all) the edge has the location EXTERIOR for the
     * geometry. <p> Note that the edge cannot be on the BOUNDARY of the geometry, since then there
     * would have been a parallel edge from the Geometry at this node also labelled BOUNDARY and
     * this edge would have been labelled in the previous step. <p> This code causes a problem when
     * dimensional collapses are present, since it may try and determine the location of a node
     * where a dimensional collapse has occurred. The point should be considered to be on the
     * EXTERIOR of the polygon, but locate() will return INTERIOR, since it is passed the original
     * Geometry, not the collapsed version.
     *
     * If there are incident edges which are Line edges labelled BOUNDARY, then they must be edges
     * resulting from dimensional collapses. In this case the other edges can be labelled EXTERIOR
     * for this Geometry.
     *
     * MD 8/11/01 - NOT TRUE! The collapsed edges may in fact be in the interior of the Geometry,
     * which means the other edges should be labelled INTERIOR for this Geometry. Not sure how solve
     * this... Possibly labelling needs to be split into several phases: area label propagation,
     * symLabel merging, then finally null label resolution.
     */
    val hasDimensionalCollapseEdge = Array(false, false)
    var it                         = iterator
    while (it.hasNext) {
      val e     = it.next
      val label = e.getLabel
      var geomi = 0
      while (geomi < 2) {
        if (label.isLine(geomi) && label.getLocation(geomi) == Location.BOUNDARY)
          hasDimensionalCollapseEdge(geomi) = true
        geomi += 1
      }
    }
    it = iterator
    while (it.hasNext) {
      val e     = it.next
      val label = e.getLabel
      // Debug.println(e);
      var geomi = 0
      while (geomi < 2) {
        if (label.isAnyNull(geomi)) {
          var loc = Location.NONE
          if (hasDimensionalCollapseEdge(geomi)) loc = Location.EXTERIOR
          else {
            val p = e.getCoordinate
            loc = getLocation(geomi, p, geomGraph)
          }
          label.setAllLocationsIfNull(geomi, loc)
        }
        {
          geomi += 1; geomi - 1
        }
      }
    }
  }

  private def computeEdgeEndLabels(boundaryNodeRule: BoundaryNodeRule): Unit = { // Compute edge label for each EdgeEnd
    val it = iterator
    while (it.hasNext) {
      val ee = it.next
      ee.computeLabel(boundaryNodeRule)
    }
  }

  private def getLocation(geomIndex: Int, p: Coordinate, geom: Array[GeometryGraph]) = { // compute location only on demand
    if (ptInAreaLocation(geomIndex) == Location.NONE)
      ptInAreaLocation(geomIndex) = SimplePointInAreaLocator.locate(p, geom(geomIndex).getGeometry)
    ptInAreaLocation(geomIndex)
  }

  def isAreaLabelsConsistent(geomGraph: GeometryGraph): Boolean = {
    computeEdgeEndLabels(geomGraph.getBoundaryNodeRule)
    checkAreaLabelsConsistent(0)
  }

  private def checkAreaLabelsConsistent(geomIndex: Int): Boolean = { // Since edges are stored in CCW order around the node,
    // As we move around the ring we move from the right to the left side of the edge
    val edges         = getEdges
    // if no edges, trivially consistent
    if (edges.size <= 0) return true
    // initialize startLoc to location of last L side (if any)
    val lastEdgeIndex = edges.size - 1
    val startLabel    = edges.get(lastEdgeIndex).asInstanceOf[EdgeEnd].getLabel
    val startLoc      = startLabel.getLocation(geomIndex, Position.LEFT)
    Assert.isTrue(startLoc != Location.NONE, "Found unlabelled area edge")
    var currLoc       = startLoc
    val it            = iterator
    while (it.hasNext) {
      val e        = it.next.asInstanceOf[EdgeEnd]
      val label    = e.getLabel
      // we assume that we are only checking a area
      Assert.isTrue(label.isArea(geomIndex), "Found non-area edge")
      val leftLoc  = label.getLocation(geomIndex, Position.LEFT)
      val rightLoc = label.getLocation(geomIndex, Position.RIGHT)
      // System.out.println(leftLoc + " " + rightLoc);
      // check that edge is really a boundary between inside and outside!
      if (leftLoc == rightLoc) return false
      // check side location conflict
      // Assert.isTrue(rightLoc == currLoc, "side location conflict " + locStr);
      if (rightLoc != currLoc) return false
      currLoc = leftLoc
    }
    true
  }

  private[geomgraph] def propagateSideLabels(geomIndex: Int): Unit = {
    var startLoc = Location.NONE
    // initialize loc to location of last L side (if any)
    // System.out.println("finding start location");
    var it       = iterator
    while (it.hasNext) {
      val e     = it.next
      val label = e.getLabel
      if (label.isArea(geomIndex) && label.getLocation(geomIndex, Position.LEFT) != Location.NONE)
        startLoc = label.getLocation(geomIndex, Position.LEFT)
    }
    // no labelled sides found, so no labels to propagate
    if (startLoc == Location.NONE) return
    var currLoc  = startLoc
    it = iterator
    while (it.hasNext) {
      val e     = it.next
      val label = e.getLabel
      // set null ON values to be in current location
      if (label.getLocation(geomIndex, Position.ON) == Location.NONE)
        label.setLocation(geomIndex, Position.ON, currLoc)
      // set side labels (if any)
      if (label.isArea(geomIndex)) {
        val leftLoc  = label.getLocation(geomIndex, Position.LEFT)
        val rightLoc = label.getLocation(geomIndex, Position.RIGHT)
        // if there is a right location, that is the next location to propagate
        if (rightLoc != Location.NONE) { // Debug.print(rightLoc != currLoc, this);
          if (rightLoc != currLoc)
            throw new TopologyException("side location conflict", e.getCoordinate)
          if (leftLoc == Location.NONE)
            Assert.shouldNeverReachHere("found single null side (at " + e.getCoordinate + ")")
          currLoc = leftLoc
        } else {

          /**
           * RHS is null - LHS must be null too. This must be an edge from the other geometry, which
           * has no location labelling for this geometry. This edge must lie wholly inside or
           * outside the other geometry (which is determined by the current location). Assign both
           * sides to be the current location.
           */
          Assert.isTrue(label.getLocation(geomIndex, Position.LEFT) == Location.NONE,
                        "found single null side"
          )
          label.setLocation(geomIndex, Position.RIGHT, currLoc)
          label.setLocation(geomIndex, Position.LEFT, currLoc)
        }
      }
    }
  }

  def findIndex(eSearch: EdgeEnd): Int = {
    iterator // force edgelist to be computed
    var i = 0
    while (i < edgeList.size) {
      val e = edgeList.get(i)
      if (e eq eSearch) return i
      i += 1
    }
    -1
  }

  def print(out: PrintStream): Unit = {
    System.out.println("EdgeEndStar:   " + getCoordinate)
    val it = iterator
    while (it.hasNext) {
      val e = it.next
      e.print(out)
    }
  }

  override def toString: String = {
    val buf = new StringBuffer
    buf.append("EdgeEndStar:   " + getCoordinate)
    buf.append("\n")
    val it  = iterator
    while (it.hasNext) {
      val e = it.next
      buf.append(e)
      buf.append("\n")
    }
    buf.toString
  }
}
