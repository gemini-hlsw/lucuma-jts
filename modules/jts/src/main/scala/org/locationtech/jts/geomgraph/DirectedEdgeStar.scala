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

import java.io.PrintStream
import java.util
import org.locationtech.jts.geom.Location
import org.locationtech.jts.geom.TopologyException
import org.locationtech.jts.util.Assert

/**
 * A DirectedEdgeStar is an ordered list of <b>outgoing</b> DirectedEdges around a node. It supports
 * labelling the edges as well as linking the edges to form both MaximalEdgeRings and
 * MinimalEdgeRings.
 *
 * @version 1.7
 */
class DirectedEdgeStar() extends EdgeEndStar {

  /**
   * A list of all outgoing edges in the result, in CCW order
   */
  private var resultAreaEdgeList: util.List[DirectedEdge] = null
  private var label: Label                                = null

  /**
   * Insert a directed edge in the list
   */
  override def insert(ee: EdgeEnd): Unit = {
    val de = ee.asInstanceOf[DirectedEdge]
    insertEdgeEnd(de, de)
  }

  def getLabel: Label = label

  def getOutgoingDegree: Int = {
    var degree = 0
    val it     = iterator
    while (it.hasNext) {
      val de = it.next.asInstanceOf[DirectedEdge]
      if (de.isInResult) {
        degree += 1; degree - 1
      }
    }
    degree
  }

  def getOutgoingDegree(er: EdgeRing): Int = {
    var degree = 0
    val it     = iterator
    while (it.hasNext) {
      val de = it.next.asInstanceOf[DirectedEdge]
      if (de.getEdgeRing eq er) {
        degree += 1; degree - 1
      }
    }
    degree
  }

  def getRightmostEdge: DirectedEdge = {
    val edges  = getEdges
    val size   = edges.size
    if (size < 1) return null
    val de0    = edges.get(0).asInstanceOf[DirectedEdge]
    if (size == 1) return de0
    val deLast = edges.get(size - 1).asInstanceOf[DirectedEdge]
    val quad0  = de0.getQuadrant
    val quad1  = deLast.getQuadrant
    if (Quadrant.isNorthern(quad0) && Quadrant.isNorthern(quad1)) return de0
    else if (!Quadrant.isNorthern(quad0) && !Quadrant.isNorthern(quad1)) return deLast
    else { // edges are in different hemispheres - make sure we return one that is non-horizontal
      //Assert.isTrue(de0.getDy() != 0, "should never return horizontal edge!");
//      val nonHorizontalEdge = null
      if (de0.getDy != 0) return de0
      else if (deLast.getDy != 0) return deLast
    }
    Assert.shouldNeverReachHere("found two horizontal edges incident on node")
    null
  }

  /**
   * Compute the labelling for all dirEdges in this star, as well as the overall labelling
   */
  override def computeLabelling(geom: Array[GeometryGraph]): Unit = { //Debug.print(this);
    super.computeLabelling(geom)
    // determine the overall labelling for this DirectedEdgeStar
    // (i.e. for the node it is based at)
    label = new Label(Location.NONE)
    val it = iterator
    while (it.hasNext) {
      val ee     = it.next.asInstanceOf[EdgeEnd]
      val e      = ee.getEdge
      val eLabel = e.getLabel
      var i      = 0
      while (i < 2) {
        val eLoc = eLabel.getLocation(i)
        if (eLoc == Location.INTERIOR || eLoc == Location.BOUNDARY)
          label.setLocation(i, Location.INTERIOR)
        i += 1
      }
    }
  }

  /**
   * For each dirEdge in the star, merge the label from the sym dirEdge into the label
   */
  def mergeSymLabels(): Unit = {
    val it = iterator
    while (it.hasNext) {
      val de    = it.next.asInstanceOf[DirectedEdge]
      val label = de.getLabel
      label.merge(de.getSym.getLabel)
    }
  }

  /**
   * Update incomplete dirEdge labels from the labelling for the node
   */
  def updateLabelling(nodeLabel: Label): Unit             = {
    val it = iterator
    while (it.hasNext) {
      val de    = it.next.asInstanceOf[DirectedEdge]
      val label = de.getLabel
      label.setAllLocationsIfNull(0, nodeLabel.getLocation(0))
      label.setAllLocationsIfNull(1, nodeLabel.getLocation(1))
    }
  }

  private def getResultAreaEdges: util.List[DirectedEdge] = { //print(System.out);
    if (resultAreaEdgeList != null) return resultAreaEdgeList
    resultAreaEdgeList = new util.ArrayList[DirectedEdge]
    val it = iterator
    while (it.hasNext) {
      val de = it.next.asInstanceOf[DirectedEdge]
      if (de.isInResult || de.getSym.isInResult) resultAreaEdgeList.add(de)
    }
    resultAreaEdgeList
  }

  final private val SCANNING_FOR_INCOMING                 = 1
  final private val LINKING_TO_OUTGOING                   = 2

  /**
   * Traverse the star of DirectedEdges, linking the included edges together. To link two dirEdges,
   * the <code>next</code> pointer for an incoming dirEdge is set to the next outgoing edge. <p>
   * DirEdges are only linked if: <ul> <li>they belong to an area (i.e. they have sides) <li>they
   * are marked as being in the result </ul> <p> Edges are linked in CCW order (the order they are
   * stored). This means that rings have their face on the Right (in other words, the topological
   * location of the face is given by the RHS label of the DirectedEdge) <p> PRECONDITION: No pair
   * of dirEdges are both marked as being in the result
   */
  def linkResultDirectedEdges(): Unit = { // make sure edges are copied to resultAreaEdges list
    getResultAreaEdges
    // find first area edge (if any) to start linking at
    var firstOut: DirectedEdge = null
    var incoming: DirectedEdge = null
    var state                  = SCANNING_FOR_INCOMING
    // link edges in CCW order
    var i                      = 0
    while (i < resultAreaEdgeList.size) {
      val nextOut = resultAreaEdgeList.get(i).asInstanceOf[DirectedEdge]
      val nextIn  = nextOut.getSym
      // skip de's that we're not interested in
      if (nextOut.getLabel.isArea) {
        // record first outgoing edge, in order to link the last incoming edge
        if (firstOut == null && nextOut.isInResult) firstOut = nextOut
        // assert: sym.isInResult() == false, since pairs of dirEdges should have been removed already
        state match {
          case SCANNING_FOR_INCOMING =>
            if (nextIn.isInResult) {
              incoming = nextIn
              state = LINKING_TO_OUTGOING
            }
          case LINKING_TO_OUTGOING   =>
            if (nextOut.isInResult) {
              incoming.setNext(nextOut)
              state = SCANNING_FOR_INCOMING
            }
        }
      }
      i += 1
    }
    if (state == LINKING_TO_OUTGOING) { //Debug.print(firstOut == null, this);
      if (firstOut == null) throw new TopologyException("no outgoing dirEdge found", getCoordinate)
      //Assert.isTrue(firstOut != null, "no outgoing dirEdge found (at " + getCoordinate() );
      Assert.isTrue(firstOut.isInResult, "unable to link last incoming dirEdge")
      incoming.setNext(firstOut)
    }
  }

  def linkMinimalDirectedEdges(er: EdgeRing): Unit = {
    var firstOut: DirectedEdge = null
    var incoming: DirectedEdge = null
    var state                  = SCANNING_FOR_INCOMING
    // link edges in CW order
    var i                      = resultAreaEdgeList.size - 1
    while (i >= 0) {
      val nextOut = resultAreaEdgeList.get(i)
      val nextIn  = nextOut.getSym
      if (firstOut == null && (nextOut.getEdgeRing == er)) firstOut = nextOut
      state match {
        case SCANNING_FOR_INCOMING =>
          if (nextIn.getEdgeRing == er) {
            incoming = nextIn
            state = LINKING_TO_OUTGOING
          }
        case LINKING_TO_OUTGOING   =>
          if (nextOut.getEdgeRing == er) {
            incoming.setNextMin(nextOut)
            state = SCANNING_FOR_INCOMING
          }
      }
      i -= 1
    }
    if (state == LINKING_TO_OUTGOING) {
      Assert.isTrue(firstOut != null, "found null for first outgoing dirEdge")
      Assert.isTrue(firstOut.getEdgeRing eq er, "unable to link last incoming dirEdge")
      incoming.setNextMin(firstOut)
    }
  }

  def linkAllDirectedEdges(): Unit = {
    getEdges
    var prevOut: DirectedEdge = null
    var firstIn: DirectedEdge = null
    var i                     = edgeList.size - 1
    while (i >= 0) {
      val nextOut = edgeList.get(i).asInstanceOf[DirectedEdge]
      val nextIn  = nextOut.getSym
      if (firstIn == null) firstIn = nextIn
      if (prevOut != null) nextIn.setNext(prevOut)
      // record outgoing edge, in order to link the last incoming edge
      prevOut = nextOut
      i -= 1; i + 1
    }
    firstIn.setNext(prevOut)
  }

  /**
   * Traverse the star of edges, maintaining the current location in the result area at this node
   * (if any). If any L edges are found in the interior of the result, mark them as covered.
   */
  def findCoveredLineEdges(): Unit = { //Debug.print("findCoveredLineEdges");
    // Since edges are stored in CCW order around the node,
    // as we move around the ring we move from the right to the left side of the edge
    /**
     * Find first DirectedEdge of result area (if any). The interior of the result is on the RHS of
     * the edge, so the start location will be:
     *   - INTERIOR if the edge is outgoing
     *   - EXTERIOR if the edge is incoming
     */
    var startLoc = Location.NONE
    var it       = iterator
    import scala.util.control.Breaks._

    breakable {
      while (it.hasNext) {
        val nextOut = it.next.asInstanceOf[DirectedEdge]
        val nextIn  = nextOut.getSym
        if (!nextOut.isLineEdge) {
          if (nextOut.isInResult) {
            startLoc = Location.INTERIOR
            break() //todo: break is not supported
          }
          if (nextIn.isInResult) {
            startLoc = Location.EXTERIOR
            break() //todo: break is not supported
          }
        }
      }
    }
    // no A edges found, so can't determine if L edges are covered or not
    if (startLoc == Location.NONE) return
    /**
     * move around ring, keeping track of the current location (Interior or Exterior) for the result
     * area. If L edges are found, mark them as covered if they are in the interior
     */
    var currLoc = startLoc
    it = iterator
    while (it.hasNext) {
      val nextOut = it.next.asInstanceOf[DirectedEdge]
      val nextIn  = nextOut.getSym
      if (nextOut.isLineEdge) {
        nextOut.getEdge.setCovered(currLoc == Location.INTERIOR)
        //Debug.println(nextOut);
      } else { // edge is an Area edge
        if (nextOut.isInResult) currLoc = Location.EXTERIOR
        if (nextIn.isInResult) currLoc = Location.INTERIOR
      }
    }
  }

  def computeDepths(de: DirectedEdge): Unit = {
    val edgeIndex       = findIndex(de)
    val startDepth      = de.getDepth(Position.LEFT)
    val targetLastDepth = de.getDepth(Position.RIGHT)
    // compute the depths from this edge up to the end of the edge array
    val nextDepth: Int  = computeDepths(edgeIndex + 1, edgeList.size, startDepth)
    // compute the depths for the initial part of the array
    val lastDepth       = computeDepths(0, edgeIndex, nextDepth)
    //Debug.print(lastDepth != targetLastDepth, this);
    //Debug.print(lastDepth != targetLastDepth, "mismatch: " + lastDepth + " / " + targetLastDepth);
    if (lastDepth != targetLastDepth)
      throw new TopologyException("depth mismatch at " + de.getCoordinate)
    //Assert.isTrue(lastDepth == targetLastDepth, "depth mismatch at " + de.getCoordinate());
  }

  /**
   * Compute the DirectedEdge depths for a subsequence of the edge array.
   *
   * return the last depth assigned (from the R side of the last edge visited)
   */
  private def computeDepths(startIndex: Int, endIndex: Int, startDepth: Int): Int = {
    var currDepth = startDepth
    var i         = startIndex
    while (i < endIndex) {
      val nextDe = edgeList.get(i).asInstanceOf[DirectedEdge]
      nextDe.setEdgeDepths(Position.RIGHT, currDepth)
      currDepth = nextDe.getDepth(Position.LEFT)
      i += 1; i - 1
    }
    currDepth
  }

  override def print(out: PrintStream): Unit                                      = {
    System.out.println("DirectedEdgeStar: " + getCoordinate)
    val it = iterator
    while (it.hasNext) {
      val de = it.next.asInstanceOf[DirectedEdge]
      out.print("out ")
      de.print(out)
      out.println()
      out.print("in ")
      de.getSym.print(out)
      out.println()
    }
  }
}
