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
package org.locationtech.jts.operation.buffer

/**
 * @version 1.7
 */

import java.util
import java.util.Collections

import org.locationtech.jts.algorithm.RobustLineIntersector
import org.locationtech.jts.geom.{ Geometry, GeometryFactory, Location, Polygon, PrecisionModel }
import org.locationtech.jts.geomgraph._
import org.locationtech.jts.noding.{ IntersectionAdder, MCIndexNoder, Noder, SegmentString }
import org.locationtech.jts.operation.overlay.{ OverlayNodeFactory, PolygonBuilder }

import scala.jdk.CollectionConverters._

// /**
//  * Builds the buffer geometry for a given input geometry and precision model.
//  * Allows setting the level of approximation for circular arcs,
//  * and the precision model in which to carry out the computation.
//  * <p>
//  * When computing buffers in floating point double-precision
//  * it can happen that the process of iterated noding can fail to converge (terminate).
//  * In this case a {link TopologyException} will be thrown.
//  * Retrying the computation in a fixed precision
//  * can produce more robust results.
//  *
//  * @version 1.7
//  */
object BufferBuilder {

  /**
   * Compute the change in depth as an edge is crossed from R to L
   */
  private def depthDelta(label: Label): Int = {
    val lLoc = label.getLocation(0, Position.LEFT)
    val rLoc = label.getLocation(0, Position.RIGHT)
    if (lLoc == Location.INTERIOR && rLoc == Location.EXTERIOR) return 1
    else if (lLoc == Location.EXTERIOR && rLoc == Location.INTERIOR) return -1
    0
  }

//  private def convertSegStrings(it: util.Iterator[SegmentString]) = {
//    val fact = new GeometryFactory
//    val lines = new util.ArrayList[Geometry]
//    while ( {
//      it.hasNext
//    }) {
//      val ss = it.next
//      val line = fact.createLineString(ss.getCoordinates)
//      lines.add(line)
//    }
//    fact.buildGeometry(lines)
//  }
}

class BufferBuilder(var bufParams: BufferParameters) {

  /**
   * Creates a new BufferBuilder,
   * using the given parameters.
   *
   * @param bufParams the buffer parameters to use
   */
  private var workingPrecisionModel: PrecisionModel = null
  private var workingNoder: Noder[SegmentString]    = null
  private var geomFact: GeometryFactory             = null
  private var graph: PlanarGraph                    = null
  private val edgeList                              = new EdgeList

  /**
   * Sets the precision model to use during the curve computation and noding,
   * if it is different to the precision model of the Geometry.
   * If the precision model is less than the precision of the Geometry precision model,
   * the Geometry must have previously been rounded to that precision.
   *
   * @param pm the precision model to use
   */
  def setWorkingPrecisionModel(pm: PrecisionModel): Unit = workingPrecisionModel = pm

  // /**
  //  * Sets the {link Noder} to use during noding.
  //  * This allows choosing fast but non-robust noding, or slower
  //  * but robust noding.
  //  *
  //  * @param noder the noder to use
  //  */
  def setNoder(noder: Noder[SegmentString]): Unit = workingNoder = noder

  def buffer(g: Geometry, distance: Double): Geometry = {
    var precisionModel   = workingPrecisionModel
    if (precisionModel == null) precisionModel = g.getPrecisionModel
    // factory must be the same as the one used by the input
    geomFact = g.getFactory
    val curveBuilder     = new OffsetCurveBuilder(precisionModel, bufParams)
    val curveSetBuilder  = new OffsetCurveSetBuilder(g, distance, curveBuilder)
    val bufferSegStrList = curveSetBuilder.getCurves
    // short-circuit test
    if (bufferSegStrList.size <= 0) return createEmptyResultGeometry
    //BufferDebug.runCount++;
    //String filename = "run" + BufferDebug.runCount + "_curves";
    //System.out.println("saving " + filename);
    //BufferDebug.saveEdges(bufferEdgeList, filename);
    // DEBUGGING ONLY
    //WKTWriter wktWriter = new WKTWriter();
    //Debug.println("Rings: " + wktWriter.write(convertSegStrings(bufferSegStrList.iterator())));
    //wktWriter.setMaxCoordinatesPerLine(10);
    //System.out.println(wktWriter.writeFormatted(convertSegStrings(bufferSegStrList.iterator())));
    computeNodedEdges(bufferSegStrList, precisionModel)
    graph = new PlanarGraph(new OverlayNodeFactory)
    graph.addEdges(edgeList.getEdges)
    val subgraphList     = createSubgraphs(graph)
    val polyBuilder      = new PolygonBuilder(geomFact)
    buildSubgraphs(subgraphList, polyBuilder)
    val resultPolyList   = polyBuilder.getPolygons
    // just in case...
    if (resultPolyList.size <= 0) return createEmptyResultGeometry
    val resultGeom       = geomFact.buildGeometry(resultPolyList.asScala.map(x => x: Geometry).asJava)
    resultGeom
  }

  private def getNoder(precisionModel: PrecisionModel): Noder[SegmentString] = {
    if (workingNoder != null) return workingNoder
    // otherwise use a fast (but non-robust) noder
    val noder = new MCIndexNoder
    val li    = new RobustLineIntersector
    li.setPrecisionModel(precisionModel)
    noder.setSegmentIntersector(new IntersectionAdder(li))
    //    Noder noder = new IteratedNoder(precisionModel);
    noder
    //    Noder noder = new SimpleSnapRounder(precisionModel);
    //    Noder noder = new MCIndexSnapRounder(precisionModel);
    //    Noder noder = new ScaledNoder(new MCIndexSnapRounder(new PrecisionModel(1.0)),
    //                                  precisionModel.getScale());
  }

  private def computeNodedEdges(
    bufferSegStrList: util.List[SegmentString],
    precisionModel:   PrecisionModel
  ): Unit = {
    val noder           = getNoder(precisionModel)
    noder.computeNodes(bufferSegStrList)
    val nodedSegStrings = noder.getNodedSubstrings
    //BufferDebug.saveEdges(nodedEdges, "run" + BufferDebug.runCount + "_nodedEdges");
    val i               = nodedSegStrings.iterator
    while (i.hasNext) {
      val segStr = i.next.asInstanceOf[SegmentString]

      /**
       * Discard edges which have zero length,
       * since they carry no information and cause problems with topology building
       */
      val pts = segStr.getCoordinates
      if (!(pts.length == 2 && pts(0).equals2D(pts(1)))) {
        val oldLabel = segStr.getData.asInstanceOf[Label]
        val edge     = new Edge(segStr.getCoordinates, new Label(oldLabel))
        insertUniqueEdge(edge)
      }
    }
    //saveEdges(edgeList.getEdges(), "run" + runCount + "_collapsedEdges");
  }

  /**
   * Inserted edges are checked to see if an identical edge already exists.
   * If so, the edge is not inserted, but its label is merged
   * with the existing edge.
   */
  protected def insertUniqueEdge(e: Edge): Unit = { //<FIX> MD 8 Oct 03  speed up identical edge lookup
    // fast lookup
    val existingEdge = edgeList.findEqualEdge(e)
    // If an identical edge already exists, simply update its label
    if (existingEdge != null) {
      val existingLabel = existingEdge.getLabel
      var labelToMerge  = e.getLabel
      // check if new edge is in reverse direction to existing edge
      // if so, must flip the label before merging it
      if (!existingEdge.isPointwiseEqual(e)) {
        labelToMerge = new Label(e.getLabel)
        labelToMerge.flip()
      }
      existingLabel.merge(labelToMerge)
      // compute new depth delta of sum of edges
      val mergeDelta    = BufferBuilder.depthDelta(labelToMerge)
      val existingDelta = existingEdge.getDepthDelta
      val newDelta      = existingDelta + mergeDelta
      existingEdge.setDepthDelta(newDelta)
    } else { // no matching existing edge was found
      // add this new edge to the list of edges in this graph
      //e.setName(name + edges.size());
      edgeList.add(e)
      e.setDepthDelta(BufferBuilder.depthDelta(e.getLabel))
    }
  }

  private def createSubgraphs(graph: PlanarGraph) = {
    val subgraphList = new util.ArrayList[BufferSubgraph]
    val i            = graph.getNodes.iterator
    while (i.hasNext) {
      val node = i.next
      if (!node.isVisited) {
        val subgraph = new BufferSubgraph
        subgraph.create(node)
        subgraphList.add(subgraph)
      }
    }

    /**
     * Sort the subgraphs in descending order of their rightmost coordinate.
     * This ensures that when the Polygons for the subgraphs are built,
     * subgraphs for shells will have been built before the subgraphs for
     * any holes they contain.
     */
    Collections.sort(subgraphList, Collections.reverseOrder[BufferSubgraph])
    subgraphList
  }

  /**
   * Completes the building of the input subgraphs by depth-labelling them,
   * and adds them to the PolygonBuilder.
   * The subgraph list must be sorted in rightmost-coordinate order.
   *
   * @param subgraphList the subgraphs to build
   * @param polyBuilder  the PolygonBuilder which will build the final polygons
   */
  private def buildSubgraphs(
    subgraphList: util.List[BufferSubgraph],
    polyBuilder:  PolygonBuilder
  ) = {
    val processedGraphs = new util.ArrayList[BufferSubgraph]
    val i               = subgraphList.iterator
    while (i.hasNext) {
      val subgraph     = i.next
      val p            = subgraph.getRightmostCoordinate
      //      int outsideDepth = 0;
      //      if (polyBuilder.containsPoint(p))
      //        outsideDepth = 1;
      val locater      = new SubgraphDepthLocater(processedGraphs)
      val outsideDepth = locater.getDepth(p)
      //      try {
      subgraph.computeDepth(outsideDepth)
      //      }
      //      catch (RuntimeException ex) {
      //        // debugging only
      //        //subgraph.saveDirEdges();
      //        throw ex;
      subgraph.findResultEdges()
      processedGraphs.add(subgraph)
      polyBuilder.add(subgraph.getDirectedEdges.asScala.map(x => x: EdgeEnd).asJava,
                      subgraph.getNodes
      )
    }
  }

  /**
   * Gets the standard result for an empty buffer.
   * Since buffer always returns a polygonal result,
   * this is chosen to be an empty polygon.
   *
   * return the empty result geometry
   */
  private def createEmptyResultGeometry: Polygon = {
    val emptyGeom = geomFact.createPolygon
    emptyGeom
  }
}
