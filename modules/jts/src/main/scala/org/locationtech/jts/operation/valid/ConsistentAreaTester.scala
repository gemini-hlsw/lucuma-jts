// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
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
package org.locationtech.jts.operation.valid

import org.locationtech.jts.algorithm.RobustLineIntersector
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geomgraph.GeometryGraph
import org.locationtech.jts.operation.relate.EdgeEndBundle
import org.locationtech.jts.operation.relate.RelateNode
import org.locationtech.jts.operation.relate.RelateNodeGraph

/**
 * Checks that a {link GeometryGraph} representing an area (a {link Polygon} or {link MultiPolygon}
 * ) has consistent semantics for area geometries. This check is required for any reasonable
 * polygonal model (including the OGC-SFS model, as well as models which allow ring
 * self-intersection at single points) <p> Checks include: <ul> <li>test for rings which properly
 * intersect (but not for ring self-intersection, or intersections at vertices) <li>test for
 * consistent labelling at all node points (this detects vertex intersections with invalid topology,
 * i.e. where the exterior side of an edge lies in the interior of the area) <li>test for duplicate
 * rings </ul> If an inconsistency is found the location of the problem is recorded and is available
 * to the caller.
 *
 * @version 1.7
 */
class ConsistentAreaTester(var geomGraph: GeometryGraph) {

  /**
   * Creates a new tester for consistent areas.
   *
   * @param geomGraph
   *   the topology graph of the area geometry
   */
  final private val li                 = new RobustLineIntersector
  private val nodeGraph                = new RelateNodeGraph
  // the intersection point found (if any)
  private var invalidPoint: Coordinate = null

  /**
   * return the intersection point, or <code>null</code> if none was found
   */
  def getInvalidPoint: Coordinate = invalidPoint

  /**
   * Check all nodes to see if their labels are consistent with area topology.
   *
   * return <code>true</code> if this area has a consistent node labelling
   */
  def isNodeConsistentArea: Boolean = {

    /**
     * To fully check validity, it is necessary to compute ALL intersections, including
     * self-intersections within a single edge.
     */
    val intersector = geomGraph.computeSelfNodes(li, true, true)

    /**
     * A proper intersection means that the area is not consistent.
     */
    if (intersector.hasProperIntersection) {
      invalidPoint = intersector.getProperIntersectionPoint
      return false
    }
    nodeGraph.build(geomGraph)
    isNodeEdgeAreaLabelsConsistent
  }

  /**
   * Check all nodes to see if their labels are consistent. If any are not, return false
   *
   * return <code>true</code> if the edge area labels are consistent at this node
   */
  private def isNodeEdgeAreaLabelsConsistent: Boolean = {
    val nodeIt = nodeGraph.getNodeIterator
    while (nodeIt.hasNext) {
      val node = nodeIt.next.asInstanceOf[RelateNode]
      if (!node.getEdges.isAreaLabelsConsistent(geomGraph)) {
        invalidPoint = node.getCoordinate.copy
        return false
      }
    }
    true
  }

  /**
   * Checks for two duplicate rings in an area. Duplicate rings are rings that are topologically
   * equal (that is, which have the same sequence of points up to point order). If the area is
   * topologically consistent (determined by calling the <code>isNodeConsistentArea</code>,
   * duplicate rings can be found by checking for EdgeBundles which contain more than one EdgeEnd.
   * (This is because topologically consistent areas cannot have two rings sharing the same line
   * segment, unless the rings are equal). The start point of one of the equal rings will be placed
   * in invalidPoint.
   *
   * return true if this area Geometry is topologically consistent but has two duplicate rings
   */
  def hasDuplicateRings: Boolean = {
    val nodeIt = nodeGraph.getNodeIterator
    while (nodeIt.hasNext) {
      val node = nodeIt.next.asInstanceOf[RelateNode]
      val i    = node.getEdges.iterator
      while (i.hasNext) {
        val eeb = i.next.asInstanceOf[EdgeEndBundle]
        if (eeb.getEdgeEnds.size > 1) {
          invalidPoint = eeb.getEdge.getCoordinate(0)
          return true
        }
      }
    }
    false
  }
}
