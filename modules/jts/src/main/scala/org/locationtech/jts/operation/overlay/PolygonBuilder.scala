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
package org.locationtech.jts.operation.overlay

import java.util

import org.locationtech.jts.algorithm.PointLocation
import org.locationtech.jts.geom.CoordinateArrays
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.Polygon
import org.locationtech.jts.geom.TopologyException
import org.locationtech.jts.geomgraph.{ DirectedEdge, EdgeEnd, EdgeRing, Node, PlanarGraph }
import org.locationtech.jts.util.Assert

/**
 * Forms {link Polygon}s out of a graph of {link DirectedEdge}s. The edges to use are marked as
 * being in the result Area. <p>
 *
 * @version 1.7
 */
object PolygonBuilder {

  /**
   * Find the innermost enclosing shell EdgeRing containing the argument EdgeRing, if any. The
   * innermost enclosing ring is the <i>smallest</i> enclosing ring. The algorithm used depends on
   * the fact that: <br> ring A contains ring B iff envelope(ring A) contains envelope(ring B) <br>
   * This routine is only safe to use if the chosen point of the hole is known to be properly
   * contained in a shell (which is guaranteed to be the case if the hole does not touch its shell)
   *
   * return containing EdgeRing, if there is one or null if no containing EdgeRing is found
   */
  private def findEdgeRingContaining(testEr: EdgeRing, shellList: util.List[_]): EdgeRing = {
    val testRing              = testEr.getLinearRing
    val testEnv               = testRing.getEnvelopeInternal
    var testPt                = testRing.getCoordinateN(0)
    var minShell: EdgeRing    = null
    var minShellEnv: Envelope = null
    val it                    = shellList.iterator
    while (it.hasNext) {
      val tryShell     = it.next.asInstanceOf[EdgeRing]
      val tryShellRing = tryShell.getLinearRing
      val tryShellEnv  = tryShellRing.getEnvelopeInternal
      // the hole envelope cannot equal the shell envelope
      // (also guards against testing rings against themselves)
      if (tryShellEnv != testEnv) {
        // hole must be contained in shell
        if (tryShellEnv.contains(testEnv)) {
          testPt =
            CoordinateArrays.ptNotInList(testRing.getCoordinates, tryShellRing.getCoordinates)
          var isContained = false
          if (PointLocation.isInRing(testPt, tryShellRing.getCoordinates)) isContained = true
          // check if this new containing ring is smaller than the current minimum ring
          if (isContained) if (minShell == null || minShellEnv.contains(tryShellEnv)) {
            minShell = tryShell
            minShellEnv = minShell.getLinearRing.getEnvelopeInternal
          }
        }
        return minShell
      }
    }
    null
  }
}
class PolygonBuilder(var geometryFactory: GeometryFactory) {
  private val shellList = new util.ArrayList[EdgeRing]

  /**
   * Add a complete graph. The graph is assumed to contain one or more polygons, possibly with
   * holes.
   */
  def add(graph: PlanarGraph): Unit = add(graph.getEdgeEnds, graph.getNodes)

  /**
   * Add a set of edges and nodes, which form a graph. The graph is assumed to contain one or more
   * polygons, possibly with holes.
   */
  def add(dirEdges: util.Collection[EdgeEnd], nodes: util.Collection[Node]): Unit = {
    PlanarGraph.linkResultDirectedEdges(nodes)
    val maxEdgeRings = buildMaximalEdgeRings(dirEdges)
    val freeHoleList = new util.ArrayList[EdgeRing]
    val edgeRings    = buildMinimalEdgeRings(maxEdgeRings, shellList, freeHoleList)
    sortShellsAndHoles(edgeRings, shellList, freeHoleList)
    placeFreeHoles(shellList, freeHoleList)
    //Assert: every hole on freeHoleList has a shell assigned to it
  }

  def getPolygons: util.ArrayList[Polygon] = {
    val resultPolyList = computePolygons(shellList)
    resultPolyList
  }

  /**
   * for all DirectedEdges in result, form them into MaximalEdgeRings
   */
  private def buildMaximalEdgeRings(
    dirEdges: util.Collection[EdgeEnd]
  ): util.ArrayList[MaximalEdgeRing] = {
    val maxEdgeRings = new util.ArrayList[MaximalEdgeRing]
    val it           = dirEdges.iterator
    while (it.hasNext) {
      val de = it.next.asInstanceOf[DirectedEdge]
      if (de.isInResult && de.getLabel.isArea) { // if this edge has not yet been processed
        if (de.getEdgeRing == null) {
          val er = new MaximalEdgeRing(de, geometryFactory)
          maxEdgeRings.add(er)
          er.setInResult()
          //System.out.println("max node degree = " + er.getMaxDegree());
        }
      }
    }
    maxEdgeRings
  }

  private def buildMinimalEdgeRings(
    maxEdgeRings: util.List[MaximalEdgeRing],
    shellList:    util.List[EdgeRing],
    freeHoleList: util.List[EdgeRing]
  ): util.ArrayList[EdgeRing] = {
    val edgeRings = new util.ArrayList[EdgeRing]
    val it        = maxEdgeRings.iterator
    while (it.hasNext) {
      val er = it.next
      if (er.getMaxNodeDegree > 2) {
        er.linkDirectedEdgesForMinimalEdgeRings()
        val minEdgeRings = er.buildMinimalRings
        // at this point we can go ahead and attempt to place holes, if this EdgeRing is a polygon
        val shell        = findShell(minEdgeRings)
        if (shell != null) {
          placePolygonHoles(shell, minEdgeRings)
          shellList.add(shell)
        } else freeHoleList.addAll(minEdgeRings)
      } else edgeRings.add(er)
    }
    edgeRings
  }

  /**
   * This method takes a list of MinimalEdgeRings derived from a MaximalEdgeRing, and tests whether
   * they form a Polygon. This is the case if there is a single shell in the list. In this case the
   * shell is returned. The other possibility is that they are a series of connected holes, in which
   * case no shell is returned.
   *
   * return the shell EdgeRing, if there is one or null, if all the rings are holes
   */
  private def findShell(minEdgeRings: util.List[MinimalEdgeRing]): MinimalEdgeRing = {
    var shellCount             = 0
    var shell: MinimalEdgeRing = null
    val it                     = minEdgeRings.iterator
    while (it.hasNext) {
      val er = it.next
      if (!er.isHole) {
        shell = er
        shellCount += 1
      }
    }
    Assert.isTrue(shellCount <= 1, "found two shells in MinimalEdgeRing list")
    shell
  }

  /**
   * This method assigns the holes for a Polygon (formed from a list of MinimalEdgeRings) to its
   * shell. Determining the holes for a MinimalEdgeRing polygon serves two purposes: <ul> <li>it is
   * faster than using a point-in-polygon check later on. <li>it ensures correctness, since if the
   * PIP test was used the point chosen might lie on the shell, which might return an incorrect
   * result from the PIP test </ul>
   */
  private def placePolygonHoles(shell: EdgeRing, minEdgeRings: util.List[MinimalEdgeRing]): Unit = {
    val it = minEdgeRings.iterator
    while (it.hasNext) {
      val er = it.next
      if (er.isHole) er.setShell(shell)
    }
  }

  /**
   * For all rings in the input list, determine whether the ring is a shell or a hole and add it to
   * the appropriate list. Due to the way the DirectedEdges were linked, a ring is a shell if it is
   * oriented CW, a hole otherwise.
   */
  private def sortShellsAndHoles(
    edgeRings:    util.List[EdgeRing],
    shellList:    util.List[EdgeRing],
    freeHoleList: util.List[EdgeRing]
  ): Unit = {
    val it = edgeRings.iterator
    while (it.hasNext) {
      val er = it.next
      //      er.setInResult();
      if (er.isHole) freeHoleList.add(er)
      else shellList.add(er)
    }
  }

  /**
   * This method determines finds a containing shell for all holes which have not yet been assigned
   * to a shell. These "free" holes should all be <b>properly</b> contained in their parent shells,
   * so it is safe to use the <code>findEdgeRingContaining</code> method. (This is the case because
   * any holes which are NOT properly contained (i.e. are connected to their parent shell) would
   * have formed part of a MaximalEdgeRing and been handled in a previous step).
   *
   * throws TopologyException if a hole cannot be assigned to a shell
   */
  private def placeFreeHoles(shellList: util.List[_], freeHoleList: util.List[EdgeRing]): Unit = {
    val it = freeHoleList.iterator
    while (it.hasNext) {
      val hole = it.next
      // only place this hole if it doesn't yet have a shell
      if (hole.getShell == null) {
        val shell = PolygonBuilder.findEdgeRingContaining(hole, shellList)
        if (shell == null)
          throw new TopologyException("unable to assign hole to a shell", hole.getCoordinate(0))
        //        Assert.isTrue(shell != null, "unable to assign hole to a shell");
        hole.setShell(shell)
      }
    }
  }

  private def computePolygons(shellList: util.List[EdgeRing]): util.ArrayList[Polygon] = {
    val resultPolyList = new util.ArrayList[Polygon]
    // add Polygons for all shells
    val it             = shellList.iterator
    while (it.hasNext) {
      val er   = it.next
      val poly = er.toPolygon(geometryFactory)
      resultPolyList.add(poly)
    }
    resultPolyList
  }
}
