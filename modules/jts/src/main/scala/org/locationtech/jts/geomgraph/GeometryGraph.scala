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
package org.locationtech.jts.geomgraph

import java.util

import org.locationtech.jts.algorithm.BoundaryNodeRule
import org.locationtech.jts.algorithm.LineIntersector
import org.locationtech.jts.algorithm.Orientation
import org.locationtech.jts.algorithm.PointLocator
import org.locationtech.jts.algorithm.locate.IndexedPointInAreaLocator
import org.locationtech.jts.algorithm.locate.PointOnGeometryLocator
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateArrays
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryCollection
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.LinearRing
import org.locationtech.jts.geom.Location
import org.locationtech.jts.geom.MultiLineString
import org.locationtech.jts.geom.MultiPoint
import org.locationtech.jts.geom.MultiPolygon
import org.locationtech.jts.geom.Point
import org.locationtech.jts.geom.Polygon
import org.locationtech.jts.geom.Polygonal
import org.locationtech.jts.geomgraph.index.SegmentIntersector
import org.locationtech.jts.geomgraph.index.SimpleMCSweepLineIntersector
import org.locationtech.jts.util.Assert

/**
 * A GeometryGraph is a graph that models a given Geometry
 *
 * @version 1.7
 */
object GeometryGraph {

  /**
   * This method implements the Boundary Determination Rule for determining whether a component
   * (node or edge) that appears multiple times in elements of a MultiGeometry is in the boundary or
   * the interior of the Geometry <br> The SFS uses the "Mod-2 Rule", which this function implements
   * <br> An alternative (and possibly more intuitive) rule would be the "At Most One Rule":
   * isInBoundary = (componentCount == 1)
   */
  /*
      public static boolean isInBoundary(int boundaryCount)
      {
        // the "Mod-2 Rule"
        return boundaryCount % 2 == 1;
      }
      public static int determineBoundary(int boundaryCount)
      {
        return isInBoundary(boundaryCount) ? Location.BOUNDARY : Location.INTERIOR;
      }
   */
  def determineBoundary(boundaryNodeRule: BoundaryNodeRule, boundaryCount: Int): Int = if (
    boundaryNodeRule.isInBoundary(boundaryCount)
  ) Location.BOUNDARY
  else Location.INTERIOR
}

class GeometryGraph(
  var argIndex:         Int // the index of this geometry as an argument to a spatial function (used for labelling)
  ,
  var parentGeom:       Geometry,
  val boundaryNodeRule: BoundaryNodeRule
) extends PlanarGraph {

  /**
   * The lineEdgeMap is a map of the linestring components of the parentGeometry to the edges which
   * are derived from them. This is used to efficiently perform findEdge queries
   */
  private val lineEdgeMap = new util.HashMap[LineString, Edge]
  if (parentGeom != null) { //      precisionModel = parentGeom.getPrecisionModel();
    //      SRID = parentGeom.getSRID();
    add(parentGeom)
  }

  /**
   * If this flag is true, the Boundary Determination Rule will used when deciding whether nodes are
   * in the boundary or not
   */
  private var useBoundaryDeterminationRule          = true
  private var boundaryNodes: util.Collection[Node]  = null
  var hasTooFewPoints                               = false
  private var invalidPoint: Coordinate              = null
  private var areaPtLocator: PointOnGeometryLocator = null
  // for use if geometry is not Polygonal
  final private val ptLocator: PointLocator         = new PointLocator

  private def createEdgeSetIntersector = // various options for computing intersections, from slowest to fastest
    // private EdgeSetIntersector esi = new SimpleEdgeSetIntersector();
    // private EdgeSetIntersector esi = new MonotoneChainIntersector();
    // private EdgeSetIntersector esi = new NonReversingChainIntersector();
    // private EdgeSetIntersector esi = new SimpleSweepLineIntersector();
    // private EdgeSetIntersector esi = new MCSweepLineIntersector();
    // return new SimpleEdgeSetIntersector();
    new SimpleMCSweepLineIntersector

  def this(argIndex: Int, parentGeom: Geometry) =
    this(argIndex, parentGeom, BoundaryNodeRule.OGC_SFS_BOUNDARY_RULE)

  /**
   * This constructor is used by clients that wish to add Edges explicitly, rather than adding a
   * Geometry. (An example is BufferOp).
   */
  // no longer used
  //  public GeometryGraph(int argIndex, PrecisionModel precisionModel, int SRID) {
  //    this(argIndex, null);
  //    this.precisionModel = precisionModel;
  //    this.SRID = SRID;
  //  }
  //  public PrecisionModel getPrecisionModel()
  //  {
  //    return precisionModel;
  //  public int getSRID() { return SRID; }

  def getInvalidPoint: Coordinate = invalidPoint

  def getGeometry: Geometry = parentGeom

  def getBoundaryNodeRule: BoundaryNodeRule = boundaryNodeRule

  def getBoundaryNodes: util.Collection[Node] = {
    if (boundaryNodes == null) boundaryNodes = nodes.getBoundaryNodes(argIndex)
    boundaryNodes
  }

  def getBoundaryPoints: Array[Coordinate] = {
    val coll = getBoundaryNodes
    val pts  = new Array[Coordinate](coll.size)
    var i    = 0
    val it   = coll.iterator
    while (it.hasNext) {
      val node = it.next.asInstanceOf[Node]
      pts {
        i += 1; i - 1
      } = node.getCoordinate.copy
    }
    pts
  }

  def findEdge(line: LineString): Edge = lineEdgeMap.get(line)

  def computeSplitEdges(edgelist: util.List[Edge]): Unit = {
    val i = edges.iterator
    while (i.hasNext) {
      val e = i.next
      e.eiList.addSplitEdges(edgelist)
    }
  }

  private def add(g: Geometry): Unit = {
    if (g.isEmpty) return
    // check if this Geometry should obey the Boundary Determination Rule
    // all collections except MultiPolygons obey the rule
    if (g.isInstanceOf[MultiPolygon]) useBoundaryDeterminationRule = false
    if (g.isInstanceOf[Polygon]) addPolygon(g.asInstanceOf[Polygon])
    else { // LineString also handles LinearRings
      if (g.isInstanceOf[LineString]) addLineString(g.asInstanceOf[LineString])
      else if (g.isInstanceOf[Point]) addPoint(g.asInstanceOf[Point])
      else if (g.isInstanceOf[MultiPoint]) addCollection(g.asInstanceOf[MultiPoint])
      else if (g.isInstanceOf[MultiLineString]) addCollection(g.asInstanceOf[MultiLineString])
      else if (g.isInstanceOf[MultiPolygon]) addCollection(g.asInstanceOf[MultiPolygon])
      else if (g.isInstanceOf[GeometryCollection]) addCollection(g.asInstanceOf[GeometryCollection])
      else throw new UnsupportedOperationException(g.getClass.getName)
    }
  }

  private def addCollection(gc: GeometryCollection): Unit = {
    var i = 0
    while (i < gc.getNumGeometries) {
      val g = gc.getGeometryN(i)
      add(g)
      i += 1
    }
  }

  /**
   * Add a Point to the graph.
   */
  private def addPoint(p: Point): Unit = {
    val coord = p.getCoordinate
    insertPoint(argIndex, coord, Location.INTERIOR)
  }

  /**
   * Adds a polygon ring to the graph. Empty rings are ignored.
   *
   * The left and right topological location arguments assume that the ring is oriented CW. If the
   * ring is in the opposite orientation, the left and right locations must be interchanged.
   */
  private def addPolygonRing(lr: LinearRing, cwLeft: Int, cwRight: Int): Unit = { // don't bother adding empty holes
    if (lr.isEmpty) return
    val coord = CoordinateArrays.removeRepeatedPoints(lr.getCoordinates)
    if (coord.length < 4) {
      hasTooFewPoints = true
      invalidPoint = coord(0)
      return
    }
    var left  = cwLeft
    var right = cwRight
    if (Orientation.isCCW(coord)) {
      left = cwRight
      right = cwLeft
    }
    val e     = new Edge(coord, new Label(argIndex, Location.BOUNDARY, left, right))
    lineEdgeMap.put(lr, e)
    insertEdge(e)
    // insert the endpoint as a node, to mark that it is on the boundary
    insertPoint(argIndex, coord(0), Location.BOUNDARY)
  }

  private def addPolygon(p: Polygon): Unit = {
    addPolygonRing(p.getExteriorRing, Location.EXTERIOR, Location.INTERIOR)
    var i = 0
    while (i < p.getNumInteriorRing) {
      val hole = p.getInteriorRingN(i)
      // Holes are topologically labelled opposite to the shell, since
      // the interior of the polygon lies on their opposite side
      // (on the left, if the hole is oriented CW)
      addPolygonRing(hole, Location.INTERIOR, Location.EXTERIOR)
      i += 1
    }
  }

  private def addLineString(line: LineString): Unit = {
    val coord = CoordinateArrays.removeRepeatedPoints(line.getCoordinates)
    if (coord.length < 2) {
      hasTooFewPoints = true
      invalidPoint = coord(0)
      return
    }
    // add the edge for the LineString
    // line edges do not have locations for their left and right sides
    val e     = new Edge(coord, new Label(argIndex, Location.INTERIOR))
    lineEdgeMap.put(line, e)
    insertEdge(e)

    /**
     * Add the boundary points of the LineString, if any. Even if the LineString is closed, add both
     * points as if they were endpoints. This allows for the case that the node already exists and
     * is a boundary point.
     */
    Assert.isTrue(coord.length >= 2, "found LineString with single point")
    insertBoundaryPoint(argIndex, coord(0))
    insertBoundaryPoint(argIndex, coord(coord.length - 1))
  }

  /**
   * Add an Edge computed externally. The label on the Edge is assumed to be correct.
   */
  def addEdge(e: Edge): Unit = {
    insertEdge(e)
    val coord = e.getCoordinates
    insertPoint(argIndex, coord(0), Location.BOUNDARY)
    insertPoint(argIndex, coord(coord.length - 1), Location.BOUNDARY)
  }

  /**
   * Add a point computed externally. The point is assumed to be a Point Geometry part, which has a
   * location of INTERIOR.
   */
  def addPoint(pt: Coordinate): Unit = insertPoint(argIndex, pt, Location.INTERIOR)

  /**
   * Compute self-nodes, taking advantage of the Geometry type to minimize the number of
   * intersection tests. (E.g. rings are not tested for self-intersection, since they are assumed to
   * be valid).
   *
   * @param li
   *   the LineIntersector to use
   * @param computeRingSelfNodes
   *   if <code>false</code>, intersection checks are optimized to not test rings for
   *   self-intersection return the computed SegmentIntersector containing information about the
   *   intersections found
   */
  def computeSelfNodes(li: LineIntersector, computeRingSelfNodes: Boolean): SegmentIntersector =
    computeSelfNodes(li, computeRingSelfNodes, false)

  /**
   * Compute self-nodes, taking advantage of the Geometry type to minimize the number of
   * intersection tests. (E.g. rings are not tested for self-intersection, since they are assumed to
   * be valid).
   *
   * @param li
   *   the LineIntersector to use
   * @param computeRingSelfNodes
   *   if <code>false</code>, intersection checks are optimized to not test rings for
   *   self-intersection
   * @param isDoneIfProperInt
   *   short-circuit the intersection computation if a proper intersection is found return the
   *   computed SegmentIntersector containing information about the intersections found
   */
  def computeSelfNodes(
    li:                   LineIntersector,
    computeRingSelfNodes: Boolean,
    isDoneIfProperInt:    Boolean
  ): SegmentIntersector = {
    val si                 = new SegmentIntersector(li, true, false)
    si.setIsDoneIfProperInt(isDoneIfProperInt)
    val esi                = createEdgeSetIntersector
    // optimize intersection search for valid Polygons and LinearRings
    val isRings            =
      parentGeom.isInstanceOf[LinearRing] || parentGeom.isInstanceOf[Polygon] || parentGeom
        .isInstanceOf[MultiPolygon]
    val computeAllSegments = computeRingSelfNodes || !isRings
    esi.computeIntersections(edges, si, computeAllSegments)
    // System.out.println("SegmentIntersector # tests = " + si.numTests);
    addSelfIntersectionNodes(argIndex)
    si
  }

  def computeEdgeIntersections(
    g:             GeometryGraph,
    li:            LineIntersector,
    includeProper: Boolean
  ): SegmentIntersector = {
    val si  = new SegmentIntersector(li, includeProper, true)
    si.setBoundaryNodes(this.getBoundaryNodes, g.getBoundaryNodes)
    val esi = createEdgeSetIntersector
    esi.computeIntersections(edges, g.edges, si)
    /*
    for (Iterator i = g.edges.iterator(); i.hasNext();) {
    Edge e = (Edge) i.next();
    Debug.print(e.getEdgeIntersectionList());
    }
     */
    si
  }

  private def insertPoint(argIndex: Int, coord: Coordinate, onLocation: Int): Unit = {
    val n   = nodes.addNode(coord)
    val lbl = n.getLabel
    if (lbl == null) n.label = new Label(argIndex, onLocation)
    else lbl.setLocation(argIndex, onLocation)
  }

  /**
   * Adds candidate boundary points using the current {link BoundaryNodeRule}. This is used to add
   * the boundary points of dim-1 geometries (Curves/MultiCurves).
   */
  private def insertBoundaryPoint(argIndex: Int, coord: Coordinate): Unit = {
    val n             = nodes.addNode(coord)
    // nodes always have labels
    val lbl           = n.getLabel
    // the new point to insert is on a boundary
    var boundaryCount = 1
    // determine the current location for the point (if any)
    var loc           = Location.NONE
    loc = lbl.getLocation(argIndex, Position.ON)
    if (loc == Location.BOUNDARY) {
      boundaryCount += 1; boundaryCount - 1
    }
    // determine the boundary status of the point according to the Boundary Determination Rule
    val newLoc        = GeometryGraph.determineBoundary(boundaryNodeRule, boundaryCount)
    lbl.setLocation(argIndex, newLoc)
  }

  private def addSelfIntersectionNodes(argIndex: Int): Unit = {
    val i = edges.iterator
    while (i.hasNext) {
      val e    = i.next
      val eLoc = e.getLabel.getLocation(argIndex)
      val eiIt = e.eiList.iterator
      while (eiIt.hasNext) {
        val ei = eiIt.next
        addSelfIntersectionNode(argIndex, ei.coord, eLoc)
      }
    }
  }

  /**
   * Add a node for a self-intersection. If the node is a potential boundary node (e.g. came from an
   * edge which is a boundary) then insert it as a potential boundary node. Otherwise, just add it
   * as a regular node.
   */
  private def addSelfIntersectionNode(argIndex: Int, coord: Coordinate, loc: Int): Unit = { // if this node is already a boundary node, don't change it
    if (isBoundaryNode(argIndex, coord)) return
    if (loc == Location.BOUNDARY && useBoundaryDeterminationRule)
      insertBoundaryPoint(argIndex, coord)
    else insertPoint(argIndex, coord, loc)
  }

  /**
   * Determines the {link Location} of the given {link Coordinate} in this geometry.
   *
   * @param pt
   *   the point to test return the location of the point in the geometry
   */
  def locate(pt: Coordinate): Int = {
    if (parentGeom.isInstanceOf[Polygonal] && parentGeom.getNumGeometries > 50) { // lazily init point locator
      if (areaPtLocator == null) areaPtLocator = new IndexedPointInAreaLocator(parentGeom)
      return areaPtLocator.locate(pt)
    }
    ptLocator.locate(pt, parentGeom)
  }
}
