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
package org.locationtech.jts.operation

import org.locationtech.jts.algorithm.BoundaryNodeRule
import org.locationtech.jts.algorithm.RobustLineIntersector
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryCollection
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.LinearRing
import org.locationtech.jts.geom.MultiLineString
import org.locationtech.jts.geom.MultiPoint
import org.locationtech.jts.geom.Point
import org.locationtech.jts.geom.Polygonal
import org.locationtech.jts.geom.util.LinearComponentExtracter
import org.locationtech.jts.geomgraph.Edge
import org.locationtech.jts.geomgraph.EdgeIntersection
import org.locationtech.jts.geomgraph.GeometryGraph

import java.util
import scala.collection.mutable

// /**
//  * Tests whether a <code>Geometry</code> is simple.
//  * In general, the SFS specification of simplicity
//  * follows the rule:
//  * <ul>
//  * <li> A Geometry is simple if and only if the only self-intersections are at
//  * boundary points.
//  * </ul>
//  * <p>
//  * Simplicity is defined for each {link Geometry} type as follows:
//  * <ul>
//  * <li><b>Polygonal</b> geometries are simple by definition, so
//  * <code>isSimple</code> trivially returns true.
//  * (Note: this means that <tt>isSimple</tt> cannot be used to test
//  * for (invalid) self-intersections in <tt>Polygon</tt>s.
//  * In order to check if a <tt>Polygonal</tt> geometry has self-intersections,
//  * use {link Geometry#isValid()}).
//  * <li><b>Linear</b> geometries are simple iff they do <i>not</i> self-intersect at interior points
//  * (i.e. points other than boundary points).
//  * This is equivalent to saying that no two linear components satisfy the SFS {link Geometry#touches(Geometry)}
//  * predicate.
//  * <li><b>Zero-dimensional (point)</b> geometries are simple if and only if they have no
//  * repeated points.
//  * <li><b>Empty</b> geometries are <i>always</i> simple, by definition
//  * </ul>
//  * For {link Lineal} geometries the evaluation of simplicity
//  * can be customized by supplying a {link BoundaryNodeRule}
//  * to define how boundary points are determined.
//  * The default is the SFS-standard {link BoundaryNodeRule#MOD2_BOUNDARY_RULE}.
//  * Note that under the <tt>Mod-2</tt> rule, closed <tt>LineString</tt>s (rings)
//  * will never satisfy the <tt>touches</tt> predicate at their endpoints, since these are
//  * interior points, not boundary points.
//  * If it is required to test whether a set of <code>LineString</code>s touch
//  * only at their endpoints, use <code>IsSimpleOp</code> with {link BoundaryNodeRule#ENDPOINT_BOUNDARY_RULE}.
//  * For example, this can be used to validate that a set of lines form a topologically valid
//  * linear network.
//  *
//  * @see BoundaryNodeRule
//  * @version 1.7
//  */
object IsSimpleOp {

  private class EndpointInfo(var pt: Coordinate) {
    private[operation] var isClosed = false
    private[operation] var degree   = 0

    def getCoordinate: Coordinate = pt

    def addEndpoint(isClosed: Boolean): Unit = {
      degree += 1
      this.isClosed |= isClosed
    }
  }

}

class IsSimpleOp(inputGeom: Geometry) {

  /**
   * Creates a simplicity checker using the default SFS Mod-2 Boundary Node Rule
   *
   * @deprecated
   *   use IsSimpleOp(Geometry)
   */
  private var isClosedEndpointsInInterior   = true
  private var nonSimpleLocation: Coordinate = null

  /**
   * Creates a simplicity checker using the default SFS Mod-2 Boundary Node Rule
   *
   * @param geom
   *   the geometry to test
   */
//  def this {
//    this()
//    this.inputGeom = geom
//  }

  // /**
  //  * Creates a simplicity checker using a given {link BoundaryNodeRule}
  //  *
  //  * @param geom             the geometry to test
  //  * @param boundaryNodeRule the rule to use.
  //  */
  def this(geom: Geometry, boundaryNodeRule: BoundaryNodeRule) = {
    this(geom)
    isClosedEndpointsInInterior = !boundaryNodeRule.isInBoundary(2)
  }

  /**
   * Tests whether the geometry is simple.
   *
   * return true if the geometry is simple
   */
  def isSimple: Boolean = {
    nonSimpleLocation = null
    computeSimple(inputGeom)
  }

  private def computeSimple(geom: Geometry): Boolean = {
    nonSimpleLocation = null
    if (geom.isEmpty) return true
    if (geom.isInstanceOf[LineString]) return isSimpleLinearGeometry(geom)
    if (geom.isInstanceOf[MultiLineString]) return isSimpleLinearGeometry(geom)
    if (geom.isInstanceOf[MultiPoint]) return isSimpleMultiPoint(geom.asInstanceOf[MultiPoint])
    if (geom.isInstanceOf[Polygonal]) return isSimplePolygonal(geom)
    if (geom.isInstanceOf[GeometryCollection]) return isSimpleGeometryCollection(geom)
    // all other geometry types are simple by definition
    true
  }

  // /**
  //  * Gets a coordinate for the location where the geometry
  //  * fails to be simple.
  //  * (i.e. where it has a non-boundary self-intersection).
  //  * {link #isSimple} must be called before this method is called.
  //  *
  //  * return a coordinate for the location of the non-boundary self-intersection
  //  *         or null if the geometry is simple
  //  */
  def getNonSimpleLocation: Coordinate = nonSimpleLocation

  // /**
  //  * Reports whether a {link LineString} is simple.
  //  *
  //  * @param geom the lineal geometry to test
  //  * return true if the geometry is simple
  //  * @deprecated use isSimple()
  //  */
  def isSimple(geom: LineString): Boolean = isSimpleLinearGeometry(geom)

  // /**
  //  * Reports whether a {link MultiLineString} geometry is simple.
  //  *
  //  * @param geom the lineal geometry to test
  //  * return true if the geometry is simple
  //  * @deprecated use isSimple()
  //  */
  def isSimple(geom: MultiLineString): Boolean = isSimpleLinearGeometry(geom)

  /**
   * A MultiPoint is simple iff it has no repeated points
   *
   * @deprecated
   *   use isSimple()
   */
  def isSimple(mp: MultiPoint): Boolean = isSimpleMultiPoint(mp)

  private def isSimpleMultiPoint(mp: MultiPoint): Boolean = {
    if (mp.isEmpty) return true
    val points = new util.TreeSet[Coordinate]
    var i      = 0
    while (i < mp.getNumGeometries) {
      val pt = mp.getGeometryN(i).asInstanceOf[Point]
      val p  = pt.getCoordinate
      if (points.contains(p)) {
        nonSimpleLocation = p
        return false
      }
      points.add(p)
      i += 1
    }
    true
  }

  /**
   * Computes simplicity for polygonal geometries. Polygonal geometries are simple if and only if
   * all of their component rings are simple.
   *
   * @param geom
   *   a Polygonal geometry return true if the geometry is simple
   */
  private def isSimplePolygonal(geom: Geometry): Boolean = {
    val rings = LinearComponentExtracter.getLines(geom)
    val i     = rings.iterator
    while (i.hasNext) {
      val ring = i.next.asInstanceOf[LinearRing]
      if (!isSimpleLinearGeometry(ring)) return false
    }
    true
  }

  /**
   * Semantics for GeometryCollection is simple iff all components are simple.
   *
   * @param geom
   *   return true if the geometry is simple
   */
  private def isSimpleGeometryCollection(geom: Geometry): Boolean = {
    var i = 0
    while (i < geom.getNumGeometries) {
      val comp = geom.getGeometryN(i)
      if (!computeSimple(comp)) return false
      i += 1
    }
    true
  }

  private def isSimpleLinearGeometry(geom: Geometry): Boolean = {
    if (geom.isEmpty) return true
    val graph = new GeometryGraph(0, geom)
    val li    = new RobustLineIntersector
    val si    = graph.computeSelfNodes(li, true)
    // if no self-intersection, must be simple
    if (!si.hasIntersection) return true
    if (si.hasProperIntersection) {
      nonSimpleLocation = si.getProperIntersectionPoint
      return false
    }
    if (hasNonEndpointIntersection(graph)) return false
    if (isClosedEndpointsInInterior) if (hasClosedEndpointIntersection(graph)) return false
    true
  }

  /**
   * For all edges, check if there are any intersections which are NOT at an endpoint. The Geometry
   * is not simple if there are intersections not at endpoints.
   */
  private def hasNonEndpointIntersection(graph: GeometryGraph): Boolean = {
    val i = graph.getEdgeIterator
    while (i.hasNext) {
      val e               = i.next.asInstanceOf[Edge]
      val maxSegmentIndex = e.getMaximumSegmentIndex
      val eiIt            = e.getEdgeIntersectionList.iterator
      while (eiIt.hasNext) {
        val ei = eiIt.next.asInstanceOf[EdgeIntersection]
        if (!ei.isEndPoint(maxSegmentIndex)) {
          nonSimpleLocation = ei.getCoordinate
          return true
        }
      }
    }
    false
  }

  /**
   * Tests that no edge intersection is the endpoint of a closed line. This ensures that closed
   * lines are not touched at their endpoint, which is an interior point according to the Mod-2 rule
   * To check this we compute the degree of each endpoint. The degree of endpoints of closed lines
   * must be exactly 2.
   */
  private def hasClosedEndpointIntersection(graph: GeometryGraph): Boolean = {
    val endPoints = mutable.TreeMap.empty[Coordinate, IsSimpleOp.EndpointInfo]
    val i         = graph.getEdgeIterator
    while (i.hasNext) {
      val e        = i.next
      val isClosed = e.isClosed
      val p0       = e.getCoordinate(0)
      addEndpoint(endPoints, p0, isClosed)
      val p1       = e.getCoordinate(e.getNumPoints - 1)
      addEndpoint(endPoints, p1, isClosed)
    }
    val j         = endPoints.values.iterator
    while (j.hasNext) {
      val eiInfo = j.next()
      if (eiInfo.isClosed && eiInfo.degree != 2) {
        nonSimpleLocation = eiInfo.getCoordinate
        return true
      }
    }
    false
  }

  /**
   * Add an endpoint to the map, creating an entry for it if none exists
   */
  private def addEndpoint(
    endPoints: mutable.Map[Coordinate, IsSimpleOp.EndpointInfo],
    p:         Coordinate,
    isClosed:  Boolean
  ): Unit = {
    var eiInfo = endPoints.get(p).orNull
    if (eiInfo == null) {
      eiInfo = new IsSimpleOp.EndpointInfo(p)
      endPoints.put(p, eiInfo)
    }
    eiInfo.addEndpoint(isClosed)
  }
}
