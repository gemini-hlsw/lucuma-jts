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
package org.locationtech.jts.operation.relate

/**
 * @version 1.7
 */

import org.locationtech.jts.algorithm.PointLocator
import org.locationtech.jts.algorithm.RobustLineIntersector
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.IntersectionMatrix
import org.locationtech.jts.geom.Location
import org.locationtech.jts.geomgraph.Edge
import org.locationtech.jts.geomgraph.EdgeEnd
import org.locationtech.jts.geomgraph.GeometryGraph
import org.locationtech.jts.geomgraph.Node
import org.locationtech.jts.geomgraph.NodeMap
import org.locationtech.jts.geomgraph.index.SegmentIntersector
import org.locationtech.jts.util.Assert

import java.util

/**
 * Computes the topological relationship between two Geometries. <p> RelateComputer does not need to
 * build a complete graph structure to compute the IntersectionMatrix. The relationship between the
 * geometries can be computed by simply examining the labelling of edges incident on each node. <p>
 * RelateComputer does not currently support arbitrary GeometryCollections. This is because
 * GeometryCollections can contain overlapping Polygons. In order to correct compute relate on
 * overlapping Polygons, they would first need to be noded and merged (if not explicitly, at least
 * implicitly).
 *
 * @version 1.7
 */
class RelateComputer(
  var arg: Array[GeometryGraph] // the arg(s) of the operation
) {
  private val li            = new RobustLineIntersector
  private val ptLocator     = new PointLocator
  private val nodes         = new NodeMap(new RelateNodeFactory)
  // this intersection matrix will hold the results compute for the relate
//  private val im: IntersectionMatrix = null
  private val isolatedEdges = new util.ArrayList[Edge]
  // the intersection point found (if any)
//  private val invalidPoint = null

  def computeIM: IntersectionMatrix = {
    val im          = new IntersectionMatrix
    // since Geometries are finite and embedded in a 2-D space, the EE element must always be 2
    im.set(Location.EXTERIOR, Location.EXTERIOR, 2)
    // if the Geometries don't overlap there is nothing to do
    if (
      !arg(0).getGeometry.getEnvelopeInternal.intersects(arg(1).getGeometry.getEnvelopeInternal)
    ) {
      computeDisjointIM(im)
      return im
    }
    arg(0).computeSelfNodes(li, false)
    arg(1).computeSelfNodes(li, false)
    // compute intersections between edges of the two input geometries
    val intersector = arg(0).computeEdgeIntersections(arg(1), li, false)
    // System.out.println("computeIM: # segment intersection tests: " + intersector.numTests);
    computeIntersectionNodes(0)
    computeIntersectionNodes(1)

    /**
     * Copy the labelling for the nodes in the parent Geometries. These override any labels
     * determined by intersections between the geometries.
     */
    copyNodesAndLabels(0)
    copyNodesAndLabels(1)
    // complete the labelling for any nodes which only have a label for a single geometry
    // Debug.addWatch(nodes.find(new Coordinate(110, 200)));
    // Debug.printWatch();
    labelIsolatedNodes()
    // If a proper intersection was found, we can set a lower bound on the IM.
    computeProperIntersectionIM(intersector, im)

    /**
     * Now process improper intersections (eg where one or other of the geometries has a vertex at
     * the intersection point) We need to compute the edge graph at all nodes to determine the IM.
     */
    // build EdgeEnds for all intersections
    val eeBuilder = new EdgeEndBuilder
    val ee0       = eeBuilder.computeEdgeEnds(arg(0).getEdgeIterator)
    insertEdgeEnds(ee0)
    val ee1       = eeBuilder.computeEdgeEnds(arg(1).getEdgeIterator)
    insertEdgeEnds(ee1)
    // Debug.println("==== NodeList ===");
    // Debug.print(nodes);
    labelNodeEdges()

    /**
     * Compute the labeling for isolated components <br> Isolated components are components that do
     * not touch any other components in the graph. They can be identified by the fact that they
     * will contain labels containing ONLY a single element, the one for their parent geometry. We
     * only need to check components contained in the input graphs, since isolated components will
     * not have been replaced by new components formed by intersections.
     */
    // debugPrintln("Graph A isolated edges - ");
    labelIsolatedEdges(0, 1)
    // debugPrintln("Graph B isolated edges - ");
    labelIsolatedEdges(1, 0)
    // update the IM from all components
    updateIM(im)
    im
  }

  private def insertEdgeEnds(ee: util.List[EdgeEnd]): Unit = {
    val i = ee.iterator
    while (i.hasNext) {
      val e = i.next
      nodes.add(e)
    }
  }

  private def computeProperIntersectionIM(
    intersector: SegmentIntersector,
    im:          IntersectionMatrix
  ): Unit = { // If a proper intersection is found, we can set a lower bound on the IM.
    val dimA              = arg(0).getGeometry.getDimension
    val dimB              = arg(1).getGeometry.getDimension
    val hasProper         = intersector.hasProperIntersection
    val hasProperInterior = intersector.hasProperInteriorIntersection
    // For Geometry's of dim 0 there can never be proper intersections.
    /**
     * If edge segments of Areas properly intersect, the areas must properly overlap.
     */
    if (dimA == 2 && dimB == 2) { if (hasProper) im.setAtLeast("212101212") }
    else {

      /**
       * If an Line segment properly intersects an edge segment of an Area, it follows that the
       * Interior of the Line intersects the Boundary of the Area. If the intersection is a proper
       * <i>interior</i> intersection, then there is an Interior-Interior intersection too. Note
       * that it does not follow that the Interior of the Line intersects the Exterior of the Area,
       * since there may be another Area component which contains the rest of the Line.
       */
      if (dimA == 2 && dimB == 1) {
        if (hasProper) im.setAtLeast("FFF0FFFF2")
        if (hasProperInterior) im.setAtLeast("1FFFFF1FF")
      } else if (dimA == 1 && dimB == 2) {
        if (hasProper) im.setAtLeast("F0FFFFFF2")
        if (hasProperInterior) im.setAtLeast("1F1FFFFFF")
      } else {
        /* If edges of LineStrings properly intersect *in an interior point*, all
               we can deduce is that
               the interiors intersect.  (We can NOT deduce that the exteriors intersect,
               since some other segments in the geometries might cover the points in the
               neighbourhood of the intersection.)
               It is important that the point be known to be an interior point of
               both Geometries, since it is possible in a self-intersecting geometry to
               have a proper intersection on one segment that is also a boundary point of another segment.
         */
        if (dimA == 1 && dimB == 1) if (hasProperInterior) im.setAtLeast("0FFFFFFFF")
      }
    }
  }

  /**
   * Copy all nodes from an arg geometry into this graph. The node label in the arg geometry
   * overrides any previously computed label for that argIndex. (E.g. a node may be an intersection
   * node with a computed label of BOUNDARY, but in the original arg Geometry it is actually in the
   * interior due to the Boundary Determination Rule)
   */
  private def copyNodesAndLabels(argIndex: Int): Unit = {
    val i = arg(argIndex).getNodeIterator
    while (i.hasNext) {
      val graphNode = i.next
      val newNode   = nodes.addNode(graphNode.getCoordinate)
      newNode.setLabel(argIndex, graphNode.getLabel.getLocation(argIndex))
      // node.print(System.out);
    }
  }

  /**
   * Insert nodes for all intersections on the edges of a Geometry. Label the created nodes the same
   * as the edge label if they do not already have a label. This allows nodes created by either
   * self-intersections or mutual intersections to be labelled. Endpoint nodes will already be
   * labelled from when they were inserted.
   */
  private def computeIntersectionNodes(argIndex: Int): Unit = {
    val i = arg(argIndex).getEdgeIterator
    while (i.hasNext) {
      val e    = i.next
      val eLoc = e.getLabel.getLocation(argIndex)
      val eiIt = e.getEdgeIntersectionList.iterator
      while (eiIt.hasNext) {
        val ei = eiIt.next
        val n  = nodes.addNode(ei.coord).asInstanceOf[RelateNode]
        if (eLoc == Location.BOUNDARY) n.setLabelBoundary(argIndex)
        else if (n.getLabel.isNull(argIndex)) n.setLabel(argIndex, Location.INTERIOR)
        // Debug.println(n);
      }
    }
  }

  /**
   * For all intersections on the edges of a Geometry, label the corresponding node IF it doesn't
   * already have a label. This allows nodes created by either self-intersections or mutual
   * intersections to be labelled. Endpoint nodes will already be labelled from when they were
   * inserted.
   */
//  private def labelIntersectionNodes(argIndex: Int): Unit = {
//    val i = arg(argIndex).getEdgeIterator
//    while ( {
//      i.hasNext
//    }) {
//      val e = i.next
//      val eLoc = e.getLabel.getLocation(argIndex)
//      val eiIt = e.getEdgeIntersectionList.iterator
//      while ( {
//        eiIt.hasNext
//      }) {
//        val ei = eiIt.next
//        val n = nodes.find(ei.coord).asInstanceOf[RelateNode]
//        if (n.getLabel.isNull(argIndex)) if (eLoc == Location.BOUNDARY) n.setLabelBoundary(argIndex)
//        else n.setLabel(argIndex, Location.INTERIOR)
//        //n.print(System.out);
//      }
//    }
//  }

  /**
   * If the Geometries are disjoint, we need to enter their dimension and boundary dimension in the
   * Ext rows in the IM
   */
  private def computeDisjointIM(im: IntersectionMatrix): Unit = {
    val ga = arg(0).getGeometry
    if (!ga.isEmpty) {
      im.set(Location.INTERIOR, Location.EXTERIOR, ga.getDimension)
      im.set(Location.BOUNDARY, Location.EXTERIOR, ga.getBoundaryDimension)
    }
    val gb = arg(1).getGeometry
    if (!gb.isEmpty) {
      im.set(Location.EXTERIOR, Location.INTERIOR, gb.getDimension)
      im.set(Location.EXTERIOR, Location.BOUNDARY, gb.getBoundaryDimension)
    }
  }

  private def labelNodeEdges(): Unit = {
    val ni = nodes.iterator
    while (ni.hasNext) {
      val node = ni.next.asInstanceOf[RelateNode]
      node.getEdges.computeLabelling(arg)
      // Debug.print(node.getEdges());
    }
  }

  /**
   * update the IM with the sum of the IMs for each component
   */
  private def updateIM(im: IntersectionMatrix): Unit = { // Debug.println(im);
    val ei = isolatedEdges.iterator
    while (ei.hasNext) {
      val e = ei.next
      e.updateIM(im)
    }
    val ni = nodes.iterator
    while (ni.hasNext) {
      val node = ni.next.asInstanceOf[RelateNode]
      node.updateIM(im)
      node.updateIMFromEdges(im)
    }
  }

  /**
   * Processes isolated edges by computing their labelling and adding them to the isolated edges
   * list. Isolated edges are guaranteed not to touch the boundary of the target (since if they did,
   * they would have caused an intersection to be computed and hence would not be isolated)
   */
  private def labelIsolatedEdges(thisIndex: Int, targetIndex: Int): Unit = {
    val ei = arg(thisIndex).getEdgeIterator
    while (ei.hasNext) {
      val e = ei.next
      if (e.isIsolated) {
        labelIsolatedEdge(e, targetIndex, arg(targetIndex).getGeometry)
        isolatedEdges.add(e)
      }
    }
  }

  /**
   * Label an isolated edge of a graph with its relationship to the target geometry. If the target
   * has dim 2 or 1, the edge can either be in the interior or the exterior. If the target has dim
   * 0, the edge must be in the exterior
   */
  private def labelIsolatedEdge(
    e:           Edge,
    targetIndex: Int,
    target:      Geometry
  ): Unit = // this won't work for GeometryCollections with both dim 2 and 1 geoms
    if (target.getDimension > 0) { // since edge is not in boundary, may not need the full generality of PointLocator?
      // Possibly should use ptInArea locator instead?  We probably know here
      // that the edge does not touch the bdy of the target Geometry
      val loc = ptLocator.locate(e.getCoordinate, target)
      e.getLabel.setAllLocations(targetIndex, loc)
    } else e.getLabel.setAllLocations(targetIndex, Location.EXTERIOR)
  // System.out.println(e.getLabel());

  /**
   * Isolated nodes are nodes whose labels are incomplete (e.g. the location for one Geometry is
   * null). This is the case because nodes in one graph which don't intersect nodes in the other are
   * not completely labelled by the initial process of adding nodes to the nodeList. To complete the
   * labelling we need to check for nodes that lie in the interior of edges, and in the interior of
   * areas.
   */
  private def labelIsolatedNodes(): Unit = {
    val ni = nodes.iterator
    while (ni.hasNext) {
      val n     = ni.next
      val label = n.getLabel
      // isolated nodes should always have at least one geometry in their label
      Assert.isTrue(label.getGeometryCount > 0, "node with empty label found")
      if (n.isIsolated)
        if (label.isNull(0)) labelIsolatedNode(n, 0)
        else labelIsolatedNode(n, 1)
    }
  }

  /**
   * Label an isolated node with its relationship to the target geometry.
   */
  private def labelIsolatedNode(n: Node, targetIndex: Int): Unit = {
    val loc = ptLocator.locate(n.getCoordinate, arg(targetIndex).getGeometry)
    n.getLabel.setAllLocations(targetIndex, loc)
    // debugPrintln(n.getLabel());
  }
}
