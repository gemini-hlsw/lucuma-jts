// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.operation.overlayng

import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.Polygon
import org.locationtech.jts.geom.TopologyException
import org.locationtech.jts.util.Assert

import java.util
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

object PolygonBuilder {

  /**
   * For all OverlayEdges in result, form them into MaximalEdgeRings
   */
  private def buildMaximalRings(edges: util.Collection[OverlayEdge]) = {
    val edgeRings = new util.ArrayList[MaximalEdgeRing]
    for (e <- edges.asScala)
      if (e.isInResultArea && e.getLabel.isBoundaryEither) { // if this edge has not yet been processed
        if (e.getEdgeRingMax == null) {
          val er = new MaximalEdgeRing(e)
          edgeRings.add(er)
        }
      }
    edgeRings
  }

  /**
   * For the set of minimal rings comprising a maximal ring, assigns the holes to the shell known to
   * contain them. Assigning the holes directly to the shell serves two purposes: <ul> <li>it is
   * faster than using a point-in-polygon check later on. <li>it ensures correctness, since if the
   * PIP test was used the point chosen might lie on the shell, which might return an incorrect
   * result from the PIP test </ul>
   */
  private def assignHoles(shell: OverlayEdgeRing, edgeRings: util.List[OverlayEdgeRing]): Unit =
    for (er <- edgeRings.asScala)
      if (er.isHole) er.setShell(shell)
}

class PolygonBuilder(
  val resultAreaEdges:    util.List[OverlayEdge],
  var geometryFactory:    GeometryFactory,
  val isEnforcePolygonal: Boolean
) {
  buildRings(resultAreaEdges)
  private val shellList    = new util.ArrayList[OverlayEdgeRing]
  private val freeHoleList = new util.ArrayList[OverlayEdgeRing]

  def this(resultAreaEdges: util.List[OverlayEdge], geomFact: GeometryFactory) =
    this(resultAreaEdges, geomFact, true)

  def getPolygons: util.List[Polygon] = computePolygons(shellList)

  def getShellRings: util.List[OverlayEdgeRing] = shellList

  private def computePolygons(shellList: util.List[OverlayEdgeRing]) = {
    val resultPolyList = new util.ArrayList[Polygon]
    // add Polygons for all shells
    for (er <- shellList.asScala) {
      val poly = er.toPolygon(geometryFactory)
      resultPolyList.add(poly)
    }
    resultPolyList
  }

  private def buildRings(resultAreaEdges: util.List[OverlayEdge]): Unit = {
    linkResultAreaEdgesMax(resultAreaEdges)
    val maxRings = PolygonBuilder.buildMaximalRings(resultAreaEdges)
    buildMinimalRings(maxRings)
    placeFreeHoles(shellList, freeHoleList)
    // Assert: every hole on freeHoleList has a shell assigned to it
  }

  private def linkResultAreaEdgesMax(resultEdges: util.List[OverlayEdge]): Unit =
    for (edge <- resultEdges.asScala) // Assert.isTrue(edge.isInResult());
      // TODO: find some way to skip nodes which are already linked
      MaximalEdgeRing.linkResultAreaMaxRingAtNode(edge)

  private def buildMinimalRings(maxRings: util.List[MaximalEdgeRing]): Unit =
    for (erMax <- maxRings.asScala) {
      val minRings = erMax.buildMinimalRings(geometryFactory)
      assignShellsAndHoles(minRings)
    }

  private def assignShellsAndHoles(minRings: util.List[OverlayEdgeRing]): Unit = {

    /**
     * Two situations may occur:
     *   - the rings are a shell and some holes
     *   - rings are a set of holes This code identifies the situation and places the rings
     *     appropriately
     */
    val shell = findSingleShell(minRings)
    if (shell != null) {
      PolygonBuilder.assignHoles(shell, minRings)
      shellList.add(shell)
    } else { // all rings are holes; their shell will be found later
      freeHoleList.addAll(minRings)
    }
    ()
  }

  /**
   * Finds the single shell, if any, out of a list of minimal rings derived from a maximal ring. The
   * other possibility is that they are a set of (connected) holes, in which case no shell will be
   * found.
   *
   * @return
   *   the shell ring, if there is one or null, if all rings are holes
   */
  private def findSingleShell(edgeRings: util.List[OverlayEdgeRing]) = {
    var shellCount             = 0
    var shell: OverlayEdgeRing = null
    for (er <- edgeRings.asScala)
      if (!er.isHole) {
        shell = er
        shellCount += 1
      }
    Assert.isTrue(shellCount <= 1, "found two shells in EdgeRing list")
    shell
  }

  /**
   * Place holes have not yet been assigned to a shell. These "free" holes should all be
   * <b>properly</b> contained in their parent shells, so it is safe to use the
   * <code>findEdgeRingContaining</code> method. (This is the case because any holes which are NOT
   * properly contained (i.e. are connected to their parent shell) would have formed part of a
   * MaximalEdgeRing and been handled in a previous step).
   *
   * @throws TopologyException
   *   if a hole cannot be assigned to a shell
   */
  private def placeFreeHoles(
    shellList:    util.List[OverlayEdgeRing],
    freeHoleList: util.List[OverlayEdgeRing]
  ): Unit = // TODO: use a spatial index to improve performance
    for (hole <- freeHoleList.asScala) // only place this hole if it doesn't yet have a shell
      if (hole.getShell == null) {
        val shell = hole.findEdgeRingContaining(shellList)
        // only when building a polygon-valid result
        if (isEnforcePolygonal && shell == null)
          throw new TopologyException("unable to assign free hole to a shell", hole.getCoordinate)
        hole.setShell(shell)
      }
}
