// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.operation.overlayng

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateList
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.TopologyException

import java.util

object MaximalEdgeRing {
  private val STATE_FIND_INCOMING = 1
  private val STATE_LINK_OUTGOING = 2

  /**
   * Traverses the star of edges originating at a node and links consecutive result edges together
   * into <b>maximal</b> edge rings. To link two edges the <code>resultNextMax</code> pointer for an
   * <b>incoming</b> result edge is set to the next <b>outgoing</b> result edge. <p> Edges are
   * linked when: <ul> <li>they belong to an area (i.e. they have sides) <li>they are marked as
   * being in the result </ul> <p> Edges are linked in CCW order (which is the order they are linked
   * in the underlying graph). This means that rings have their face on the Right (in other words,
   * the topological location of the face is given by the RHS label of the DirectedEdge). This
   * produces rings with CW orientation. <p> PRECONDITIONS:
   *   - This edge is in the result
   *   - This edge is not yet linked
   *   - The edge and its sym are NOT both marked as being in the result
   */
  def linkResultAreaMaxRingAtNode(nodeEdge: OverlayEdge): Unit = {
    assert(nodeEdge.isInResultArea, "Attempt to link non-result edge")
    // assertion is only valid if building a polygonal geometry (ie not a coverage)
    // Assert.isTrue(! nodeEdge.symOE().isInResultArea(), "Found both half-edges in result");
    /**
     * Since the node edge is an out-edge, make it the last edge to be linked by starting at the
     * next edge. The node edge cannot be an in-edge as well, but the next one may be the first
     * in-edge.
     */
    val endOut                    = nodeEdge.oNextOE
    var currOut                   = endOut
    // Debug.println("\n------  Linking node MAX edges");
    // Debug.println("BEFORE: " + toString(nodeEdge));
    var state                     = STATE_FIND_INCOMING
    var currResultIn: OverlayEdge = null
    var doBreak                   = false
    while ({

      /**
       * If an edge is linked this node has already been processed so can skip further processing
       */
      if (currResultIn != null && currResultIn.isResultMaxLinked) return
      state match {
        case STATE_FIND_INCOMING =>
          val currIn = currOut.symOE
          if (!currIn.isInResultArea) { doBreak = true }
          else {
            currResultIn = currIn
            state = STATE_LINK_OUTGOING
          }
        // Debug.println("Found result in-edge:  " + currResultIn);

        case STATE_LINK_OUTGOING =>
          if (!currOut.isInResultArea) { doBreak = true }
          else {
            // link the in edge to the out edge
            currResultIn.setNextResultMax(currOut)
            state = STATE_FIND_INCOMING
          }
        // Debug.println("Linked Max edge:  " + currResultIn + " -> " + currOut);

      }
      currOut = currOut.oNextOE
      doBreak || currOut != endOut
    }) ()
    // Debug.println("AFTER: " + toString(nodeEdge));
    if (state == STATE_LINK_OUTGOING) { // Debug.print(firstOut == null, this);
      throw new TopologyException("no outgoing edge found", nodeEdge.getCoordinate)
    }
  }

  /**
   * Links the edges of a {@link MaximalEdgeRing} around this node into minimal edge rings ({@link
   * OverlayEdgeRing}s). Minimal ring edges are linked in the opposite orientation (CW) to the
   * maximal ring. This changes self-touching rings into a two or more separate rings, as per the
   * OGC SFS polygon topology semantics. This relinking must be done to each max ring separately,
   * rather than all the node result edges, since there may be more than one max ring incident at
   * the node.
   *
   * @param nodeEdge
   *   an edge originating at this node
   * @param maxRing
   *   the maximal ring to link
   */
  private def linkMinRingEdgesAtNode(nodeEdge: OverlayEdge, maxRing: MaximalEdgeRing): Unit = { // Assert.isTrue(nodeEdge.isInResult(), "Attempt to link non-result edge");
    /**
     * The node edge is an out-edge, so it is the first edge linked with the next CCW in-edge
     */
    val endOut         = nodeEdge
    var currMaxRingOut = endOut
    var currOut        = endOut.oNextOE
    // Debug.println("\n------  Linking node MIN ring edges");
    while ({
      if (isAlreadyLinked(currOut.symOE, maxRing)) return
      if (currMaxRingOut == null) currMaxRingOut = selectMaxOutEdge(currOut, maxRing)
      else currMaxRingOut = linkMaxInEdge(currOut, currMaxRingOut, maxRing)
      currOut = currOut.oNextOE
      currOut != endOut
    }) ()
    if (currMaxRingOut != null)
      throw new TopologyException("Unmatched edge found during min-ring linking",
                                  nodeEdge.getCoordinate
      )
  }

  /**
   * Tests if an edge of the maximal edge ring is already linked into a minimal {@link
   * OverlayEdgeRing}. If so, this node has already been processed earlier in the maximal edgering
   * linking scan.
   *
   * @param edge
   *   an edge of a maximal edgering
   * @param maxRing
   *   the maximal edgering
   * @return
   *   true if the edge has already been linked into a minimal edgering.
   */
  private def isAlreadyLinked(edge: OverlayEdge, maxRing: MaximalEdgeRing) = {
    val isLinked = (edge.getEdgeRingMax eq maxRing) && edge.isResultLinked
    isLinked
  }

  private def selectMaxOutEdge(currOut: OverlayEdge, maxEdgeRing: MaximalEdgeRing): OverlayEdge = { // select if currOut edge is part of this max ring
    if (currOut.getEdgeRingMax eq maxEdgeRing) return currOut
    // otherwise skip this edge
    null
  }

  private def linkMaxInEdge(
    currOut:        OverlayEdge,
    currMaxRingOut: OverlayEdge,
    maxEdgeRing:    MaximalEdgeRing
  ): OverlayEdge = {
    val currIn = currOut.symOE
    // currIn is not in this max-edgering, so keep looking
    if (currIn.getEdgeRingMax ne maxEdgeRing) return currMaxRingOut
    // Debug.println("Found result in-edge:  " + currIn);
    currIn.setNextResult(currMaxRingOut)
    // Debug.println("Linked Min Edge:  " + currIn + " -> " + currMaxRingOut);
    // return null to indicate to scan for the next max-ring out-edge
    null
  }
}

class MaximalEdgeRing(var startEdge: OverlayEdge) {
  attachEdges(startEdge)

  private def attachEdges(startEdge: OverlayEdge): Unit = {
    var edge = startEdge
    while ({
      if (edge == null) throw new TopologyException("Ring edge is null")
      if (edge.getEdgeRingMax eq this)
        throw new TopologyException("Ring edge visited twice at " + edge.getCoordinate,
                                    edge.getCoordinate
        )
      if (edge.nextResultMax == null) throw new TopologyException("Ring edge missing at", edge.dest)
      edge.setEdgeRingMax(this)
      edge = edge.nextResultMax
      edge != startEdge
    }) ()
  }

  def buildMinimalRings(geometryFactory: GeometryFactory): util.List[OverlayEdgeRing] = {
    linkMinimalRings()
    val minEdgeRings = new util.ArrayList[OverlayEdgeRing]
    var e            = startEdge
    while ({
      if (e.getEdgeRing == null) {
        val minEr = new OverlayEdgeRing(e, geometryFactory)
        minEdgeRings.add(minEr)
      }
      e = e.nextResultMax
      e != startEdge
    }) ()
    minEdgeRings
  }

  private def linkMinimalRings(): Unit = {
    var e = startEdge
    while ({
      MaximalEdgeRing.linkMinRingEdgesAtNode(e, this)
      e = e.nextResultMax
      e != startEdge
    }) ()
  }

  override def toString: String = {
    val pts = getCoordinates
    pts.toString
  }

  private def getCoordinates = {
    val coords  = new CoordinateList
    var edge    = startEdge
    var doBreak = false
    while ({
      coords.add(edge.orig)
      if (edge == null) doBreak = true
      if (edge.nextResultMax == null) doBreak = true
      if (!doBreak) {
        edge = edge.nextResultMax
        edge != startEdge
      } else doBreak
    }) ()
    // add last coordinate
    coords.add(edge.dest)
    coords.toCoordinateArray
  }
}
