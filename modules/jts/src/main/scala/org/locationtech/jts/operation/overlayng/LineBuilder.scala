// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.operation.overlayng

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateList
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.Location

import java.util
import scala.annotation.nowarn
import scala.jdk.CollectionConverters._

/*
 * Copyright (c) 2019 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v20.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */

/**
 * Finds and builds overlay result lines from the overlay graph. Output linework has the following
 * semantics: <ol> <li>Linework is fully noded</li> <li>Nodes in the input are preserved in the
 * output</li> <li>Output may contain more nodes than in the input (in particular, sequences of
 * coincident line segments are noded at each vertex</li> </ol>
 *
 * Various strategies are possible for how to merge graph edges into lines. <ul> <li>This
 * implementation uses the simplest approach of maintaining all nodes arising from noding (which
 * includes all nodes in the input, and possibly others). This matches the current JTS overlay
 * output semantics.</li> <li>Another option is to fully merge output lines from node to node. For
 * rings a node point is chosen arbitrarily. It would also be possible to output LinearRings, if the
 * input is a LinearRing and is unchanged. This will require additional info from the input
 * linework.</li> </ul>
 *
 * @author
 *   Martin Davis
 */
object LineBuilder {

  /**
   * Determines the effective location for a line, for the purpose of overlay operation evaluation.
   * Line edges and Collapses are reported as INTERIOR so they may be included in the result if
   * warranted by the effect of the operation on the two edges. (For instance, the intersection of a
   * line edge and a collapsed boundary is included in the result).
   *
   * @param lbl
   *   label of line
   * @param geomIndex
   *   index of input geometry
   * @return
   *   the effective location of the line
   */
  private def effectiveLocation(lbl: OverlayLabel, geomIndex: Int): Int = {
    if (lbl.isCollapse(geomIndex)) return Location.INTERIOR
    if (lbl.isLine(geomIndex)) return Location.INTERIOR
    lbl.getLineLocation(geomIndex)
  }

  /**
   * Finds the next edge around a node which forms part of a result line.
   *
   * @param node
   *   a line edge originating at the node to be scanned
   * @return
   *   the next line edge, or null if there is none
   */
  private def nextLineEdgeUnvisited(node: OverlayEdge): OverlayEdge = {
    var e = node
    while ({
      e = e.oNextOE
      if (!e.isVisited) {
        if (e.isInResultLine) return e
        e != node
      } else true
    }) ()
    null
  }

  /**
   * Computes the degree of the line edges incident on a node
   *
   * @param node
   *   node to compute degree for
   * @return
   *   degree of the node line edges
   */
  private def degreeOfLines(node: OverlayEdge) = {
    var degree = 0
    var e      = node
    while ({
      if (e.isInResultLine) degree += 1
      e = e.oNextOE
      e != node
    }) ()
    degree
  }
}

/**
 * Creates a builder for linear elements which may be present in the overlay result.
 *
 * @param inputGeom
 *   the input geometries
 * @param graph
 *   the topology graph
 * @param hasResultArea
 *   true if an area has been generated for the result
 * @param opCode
 *   the overlay operation code
 * @param geomFact
 *   the output geometry factory
 */
class LineBuilder(
  val inputGeom:       InputGeometry,
  var graph:           OverlayGraph,
  var hasResultArea:   Boolean,
  var opCode:          Int,
  var geometryFactory: GeometryFactory
) {

  private val inputAreaIndex: Int = inputGeom.getAreaIndex

  /**
   * Indicates whether intersections are allowed to produce heterogeneous results including proper
   * boundary touches. This does not control inclusion of touches along collapses. True provides the
   * original JTS semantics.
   */
  private var isAllowMixedResult: Boolean = !OverlayNG.STRICT_MODE_DEFAULT

  /**
   * Allow lines created by area topology collapses to appear in the result. True provides the
   * original JTS semantics.
   */
  private var isAllowCollapseLines: Boolean = !OverlayNG.STRICT_MODE_DEFAULT
  private val lines: util.List[LineString]  = new util.ArrayList[LineString]

  def setStrictMode(isStrictResultMode: Boolean): Unit = {
    isAllowCollapseLines = !isStrictResultMode
    isAllowMixedResult = !isStrictResultMode
  }

  def getLines: util.List[LineString] = {
    markResultLines()
    addResultLines()
    return lines
  }

  private def markResultLines(): Unit = {
    val edges: util.Collection[OverlayEdge] = graph.getEdges
    for (edge <- edges.asScala)
      /**
       * If the edge linework is already marked as in the result, it is not included as a line. This
       * occurs when an edge either is in a result area or has already been included as a line.
       */
      if (!edge.isInResultEither) {
        if (isResultLine(edge.getLabel)) {
          edge.markInResultLine()
          // Debug.println(edge);
        }
      }
  }

  /**
   * Checks if the topology indicated by an edge label determines that this edge should be part of a
   * result line. <p> Note that the logic here relies on the semantic that for intersection lines
   * are only returned if there is no result area components.
   *
   * @param lbl
   *   the label for an edge
   * @return
   *   true if the edge should be included in the result
   */
  private def isResultLine(lbl: OverlayLabel): Boolean = {

    /**
     * Omit edge which is a boundary of a single geometry (i.e. not a collapse or line edge as
     * well). These are only included if part of a result area. This is a short-circuit for the most
     * common area edge case
     */
    if (lbl.isBoundarySingleton) {
      return false
    }

    /**
     * Omit edge which is a collapse along a boundary. I.e a result line edge must be from a input
     * line OR two coincident area boundaries.
     *
     * This logic is only used if not including collapse lines in result.
     */
    if (!isAllowCollapseLines && lbl.isBoundaryCollapse) {
      return false
    }

    /**
     * Omit edge which is a collapse interior to its parent area. (E.g. a narrow gore, or spike off
     * a hole)
     */
    if (lbl.isInteriorCollapse) {
      return false
    }

    /**
     * For ops other than Intersection, omit a line edge if it is interior to the other area.
     *
     * For Intersection, a line edge interior to an area is included.
     */
    if (opCode != OverlayNG.INTERSECTION) {

      /**
       * Omit collapsed edge in other area interior.
       */
      if (lbl.isCollapseAndNotPartInterior) {
        return false
      }

      /**
       * If there is a result area, omit line edge inside it. It is sufficient to check against the
       * input area rather than the result area, because if line edges are present then there is
       * only one input area, and the result area must be the same as the input area.
       */
      if (hasResultArea && lbl.isLineInArea(inputAreaIndex)) {
        return false
      }
    }

    /**
     * Include line edge formed by touching area boundaries, if enabled.
     */
    if (isAllowMixedResult && opCode == OverlayNG.INTERSECTION && lbl.isBoundaryTouch) {
      return true
    }

    /**
     * Finally, determine included line edge according to overlay op boolean logic.
     */
    val aLoc: Int           = LineBuilder.effectiveLocation(lbl, 0)
    val bLoc: Int           = LineBuilder.effectiveLocation(lbl, 1)
    val isInResult: Boolean = OverlayNG.isResultOfOp(opCode, aLoc, bLoc)
    return isInResult
  }

  private def addResultLines(): Unit = {
    val edges: util.Collection[OverlayEdge] = graph.getEdges
    for (edge <- edges.asScala)
      if (edge.isInResultLine) {
        if (!edge.isVisited) {
          lines.add(toLine(edge))
          edge.markVisitedBoth()
        }
      }
  }

  private def toLine(edge: OverlayEdge): LineString = {
    val isForward: Boolean        = edge.isForward
    val pts: CoordinateList       = new CoordinateList
    pts.add(edge.orig, false)
    edge.addCoordinates(pts)
    val ptsOut: Array[Coordinate] = pts.toCoordinateArray(isForward)
    val line: LineString          = geometryFactory.createLineString(ptsOut)
    return line
  }

  /**
   * NOT USED currently. Instead the raw noded edges are output. This matches the original overlay
   * semantics. It is also faster.
   */
  /// FUTURE: enable merging via an option switch on OverlayNG
  @nowarn
  private def addResultLinesMerged(): Unit = {
    addResultLinesForNodes()
    addResultLinesRings()
  }

  private def addResultLinesForNodes(): Unit = {
    val edges: util.Collection[OverlayEdge] = graph.getEdges
    for (edge <- edges.asScala)
      if (edge.isInResultLine) {
        if (!edge.isVisited) {

          /**
           * Choose line start point as a node. Nodes in the line graph are degree-1 or degree >= 3
           * edges.
           *
           * This will find all lines originating at nodes
           */
          if (LineBuilder.degreeOfLines(edge) != 2) {
            lines.add(buildLine(edge))
          }
        }
      }
  }

  /**
   * Adds lines which form rings (i.e. have only degree-2 vertices).
   */
  private def addResultLinesRings()
    : Unit = { // TODO: an ordering could be imposed on the endpoints to make this more repeatable
    // TODO: preserve input LinearRings if possible?  Would require marking them as such
    val edges: util.Collection[OverlayEdge] = graph.getEdges
    for (edge <- edges.asScala)
      if (edge.isInResultLine) {
        if (!edge.isVisited) {
          lines.add(buildLine(edge))
        }
      }
  }

  /**
   * Traverses edges from edgeStart which lie in a single line (have degree = 2).
   *
   * The direction of the linework is preserved as far as possible. Specifically, the direction of
   * the line is determined by the start edge direction. This implies that if all edges are
   * reversed, the created line will be reversed to match. (Other more complex strategies would be
   * possible. E.g. using the direction of the majority of segments, or preferring the direction of
   * the A edges.)
   *
   * @param node
   * @return
   */
  private def buildLine(node: OverlayEdge): LineString = {
    val pts: CoordinateList       = new CoordinateList
    pts.add(node.orig, false)
    val isForward: Boolean        = node.isForward
    var e: OverlayEdge            = node
    while ({
      e.markVisitedBoth()
      e.addCoordinates(pts)
      // end line if next vertex is a node
      if (LineBuilder.degreeOfLines(e.symOE) != 2) false
      else
        e = LineBuilder.nextLineEdgeUnvisited(e.symOE)
      // e will be null if next edge has been visited, which indicates a ring
      e != null
    }) ()
    val ptsOut: Array[Coordinate] = pts.toCoordinateArray(isForward)
    val line: LineString          = geometryFactory.createLineString(ptsOut)
    return line
  }
}
