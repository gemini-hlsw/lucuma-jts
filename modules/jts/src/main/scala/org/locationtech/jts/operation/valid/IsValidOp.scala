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

import java.util
import org.locationtech.jts.algorithm.PointLocation
import org.locationtech.jts.algorithm.RobustLineIntersector
import org.locationtech.jts.algorithm.locate.IndexedPointInAreaLocator
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryCollection
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.LinearRing
import org.locationtech.jts.geom.Location
import org.locationtech.jts.geom.MultiPoint
import org.locationtech.jts.geom.MultiPolygon
import org.locationtech.jts.geom.Point
import org.locationtech.jts.geom.Polygon
import org.locationtech.jts.geomgraph.Edge
import org.locationtech.jts.geomgraph.EdgeIntersection
import org.locationtech.jts.geomgraph.EdgeIntersectionList
import org.locationtech.jts.geomgraph.GeometryGraph
import org.locationtech.jts.util.Assert

// /**
//  * Implements the algorithms required to compute the <code>isValid()</code> method
//  * for {link Geometry}s.
//  * See the documentation for the various geometry types for a specification of validity.
//  *
//  * @version 1.7
//  */
object IsValidOp {
  // /**
  //  * Tests whether a {link Geometry} is valid.
  //  *
  //  * @param geom the Geometry to test
  //  * return true if the geometry is valid
  //  */
  def isValid(geom: Geometry): Boolean = {
    val isValidOp = new IsValidOp(geom)
    isValidOp.isValid
  }

  /**
   * Checks whether a coordinate is valid for processing.
   * Coordinates are valid iff their x and y ordinates are in the
   * range of the floating point representation.
   *
   * @param coord the coordinate to validate
   * return <code>true</code> if the coordinate is valid
   */
  def isValid(coord: Coordinate): Boolean = {
    if (java.lang.Double.isNaN(coord.x)) return false
    if (java.lang.Double.isInfinite(coord.x)) return false
    if (java.lang.Double.isNaN(coord.y)) return false
    if (java.lang.Double.isInfinite(coord.y)) return false
    true
  }

  /**
   * Find a point from the list of testCoords
   * that is NOT a node in the edge for the list of searchCoords
   *
   * return the point found, or <code>null</code> if none found
   */
  def findPtNotNode(
    testCoords: Array[Coordinate],
    searchRing: LinearRing,
    graph:      GeometryGraph
  ): Coordinate = { // find edge corresponding to searchRing.
    val searchEdge = graph.findEdge(searchRing)
    // find a point in the testCoords which is not a node of the searchRing
    val eiList     = searchEdge.getEdgeIntersectionList
    // somewhat inefficient - is there a better way? (Use a node map, for instance?)
    var i          = 0
    while (i < testCoords.length) {
      val pt = testCoords(i)
      if (!eiList.isIntersection(pt)) return pt
      i += 1
    }
    null
  }
}

class IsValidOp(var parentGeometry: Geometry) { // the base Geometry to be validated ) {
  /**
   * If the following condition is TRUE JTS will validate inverted shells and exverted holes
   * (the ESRI SDE model)
   */
  private var isSelfTouchingRingFormingHoleValid = false
  private var validErr: TopologyValidationError  = null

  /**
   * Sets whether polygons using <b>Self-Touching Rings</b> to form
   * holes are reported as valid.
   * If this flag is set, the following Self-Touching conditions
   * are treated as being valid:
   * <ul>
   * <li>the shell ring self-touches to create a hole touching the shell
   * <li>a hole ring self-touches to create two holes touching at a point
   * </ul>
   * <p>
   * The default (following the OGC SFS standard)
   * is that this condition is <b>not</b> valid (<code>false</code>).
   * <p>
   * This does not affect whether Self-Touching Rings
   * disconnecting the polygon interior are considered valid
   * (these are considered to be <b>invalid</b> under the SFS, and many other
   * spatial models as well).
   * This includes "bow-tie" shells,
   * which self-touch at a single point causing the interior to
   * be disconnected,
   * and "C-shaped" holes which self-touch at a single point causing an island to be formed.
   *
   * @param isValid states whether geometry with this condition is valid
   */
  def setSelfTouchingRingFormingHoleValid(isValid: Boolean): Unit =
    isSelfTouchingRingFormingHoleValid = isValid

  /**
   * Computes the validity of the geometry,
   * and returns <tt>true</tt> if it is valid.
   *
   * return true if the geometry is valid
   */
  def isValid: Boolean = {
    checkValid(parentGeometry)
    validErr == null
  }

  /**
   * Computes the validity of the geometry,
   * and if not valid returns the validation error for the geometry,
   * or null if the geometry is valid.
   *
   * return the validation error, if the geometry is invalid
   *         or null if the geometry is valid
   */
  def getValidationError: TopologyValidationError = {
    checkValid(parentGeometry)
    validErr
  }

  private def checkValid(g: Geometry): Unit = {
    validErr = null
    // empty geometries are always valid!
    if (g.isEmpty) return
    if (g.isInstanceOf[Point]) checkValid(g.asInstanceOf[Point])
    else if (g.isInstanceOf[MultiPoint]) checkValid(g.asInstanceOf[MultiPoint])
    else { // LineString also handles LinearRings
      if (g.isInstanceOf[LinearRing]) checkValid(g.asInstanceOf[LinearRing])
      else if (g.isInstanceOf[LineString]) checkValid(g.asInstanceOf[LineString])
      else if (g.isInstanceOf[Polygon]) checkValid(g.asInstanceOf[Polygon])
      else if (g.isInstanceOf[MultiPolygon]) checkValid(g.asInstanceOf[MultiPolygon])
      else if (g.isInstanceOf[GeometryCollection]) checkValid(g.asInstanceOf[GeometryCollection])
      else throw new UnsupportedOperationException(g.getClass.getName)
    }
  }

  /**
   * Checks validity of a Point.
   */
  private def checkValid(g: Point): Unit = checkInvalidCoordinates(g.getCoordinates)

  /**
   * Checks validity of a MultiPoint.
   */
  private def checkValid(g: MultiPoint): Unit = checkInvalidCoordinates(g.getCoordinates)

  /**
   * Checks validity of a LineString.  Almost anything goes for linestrings!
   */
  private def checkValid(g: LineString): Unit = {
    checkInvalidCoordinates(g.getCoordinates)
    if (validErr != null) return
    val graph = new GeometryGraph(0, g)
    checkTooFewPoints(graph)
  }

  /**
   * Checks validity of a LinearRing.
   */
  private def checkValid(g: LinearRing): Unit = {
    checkInvalidCoordinates(g.getCoordinates)
    if (validErr != null) return
    checkClosedRing(g)
    if (validErr != null) return
    val graph = new GeometryGraph(0, g)
    checkTooFewPoints(graph)
    if (validErr != null) return
    val li    = new RobustLineIntersector
    graph.computeSelfNodes(li, true, true)
    checkNoSelfIntersectingRings(graph)
  }

  /**
   * Checks the validity of a polygon.
   * Sets the validErr flag.
   */
  private def checkValid(g: Polygon): Unit = {
    checkInvalidCoordinates(g)
    if (validErr != null) return
    checkClosedRings(g)
    if (validErr != null) return
    val graph = new GeometryGraph(0, g)
    checkTooFewPoints(graph)
    if (validErr != null) return
    checkConsistentArea(graph)
    if (validErr != null) return
    if (!isSelfTouchingRingFormingHoleValid) {
      checkNoSelfIntersectingRings(graph)
      if (validErr != null) return
    }
    checkHolesInShell(g, graph)
    if (validErr != null) return
    //SLOWcheckHolesNotNested(g);
    checkHolesNotNested(g, graph)
    if (validErr != null) return
    checkConnectedInteriors(graph)
  }

  private def checkValid(g: MultiPolygon): Unit = {
    var i     = 0
    while (i < g.getNumGeometries) {
      val p = g.getGeometryN(i).asInstanceOf[Polygon]
      checkInvalidCoordinates(p)
      if (validErr != null) return ()
      checkClosedRings(p)
      if (validErr != null) return ()
      i += 1
    }
    val graph = new GeometryGraph(0, g)
    checkTooFewPoints(graph)
    if (validErr != null) return ()
    checkConsistentArea(graph)
    if (validErr != null) return ()
    if (!isSelfTouchingRingFormingHoleValid) {
      checkNoSelfIntersectingRings(graph)
      if (validErr != null) return ()
    }
    i = 0
    while (i < g.getNumGeometries) {
      val p = g.getGeometryN(i).asInstanceOf[Polygon]
      checkHolesInShell(p, graph)
      if (validErr != null) return ()
      i += 1
    }
    i = 0
    while (i < g.getNumGeometries) {
      val p = g.getGeometryN(i).asInstanceOf[Polygon]
      checkHolesNotNested(p, graph)
      if (validErr != null) return ()
      i += 1
    }
    checkShellsNotNested(g, graph)
    if (validErr != null) return ()
    checkConnectedInteriors(graph)
  }

  private def checkValid(gc: GeometryCollection): Unit = {
    var i = 0
    while (i < gc.getNumGeometries) {
      val g = gc.getGeometryN(i)
      checkValid(g)
      if (validErr != null) return ()
      i += 1
    }
  }

  private def checkInvalidCoordinates(coords: Array[Coordinate]): Unit = {
    var i = 0
    while (i < coords.length) {
      if (!IsValidOp.isValid(coords(i))) {
        validErr =
          new TopologyValidationError(TopologyValidationError.INVALID_COORDINATE, coords(i))
        return ()
      }
      i += 1
    }
  }

  private def checkInvalidCoordinates(poly: Polygon): Unit = {
    checkInvalidCoordinates(poly.getExteriorRing.getCoordinates)
    if (validErr != null) return ()
    var i = 0
    while (i < poly.getNumInteriorRing) {
      checkInvalidCoordinates(poly.getInteriorRingN(i).getCoordinates)
      if (validErr != null) return ()
      i += 1
    }
  }

  private def checkClosedRings(poly: Polygon): Unit = {
    checkClosedRing(poly.getExteriorRing)
    if (validErr != null) return ()
    var i = 0
    while (i < poly.getNumInteriorRing) {
      checkClosedRing(poly.getInteriorRingN(i))
      if (validErr != null) return ()
      i += 1
    }
  }

  private def checkClosedRing(ring: LinearRing): Unit = {
    if (ring.isEmpty) return ()
    if (!ring.isClosed) {
      var pt: Coordinate = null
      if (ring.getNumPoints >= 1) pt = ring.getCoordinateN(0)
      validErr = new TopologyValidationError(TopologyValidationError.RING_NOT_CLOSED, pt)
    }
  }

  private def checkTooFewPoints(graph: GeometryGraph): Unit = if (graph.hasTooFewPoints) {
    validErr =
      new TopologyValidationError(TopologyValidationError.TOO_FEW_POINTS, graph.getInvalidPoint)
  }

  /**
   * Checks that the arrangement of edges in a polygonal geometry graph
   * forms a consistent area.
   *
   * @param graph
   * @see ConsistentAreaTester
   */
  private def checkConsistentArea(graph: GeometryGraph): Unit = {
    val cat         = new ConsistentAreaTester(graph)
    val isValidArea = cat.isNodeConsistentArea
    if (!isValidArea) {
      validErr =
        new TopologyValidationError(TopologyValidationError.SELF_INTERSECTION, cat.getInvalidPoint)
      return
    }
    if (cat.hasDuplicateRings)
      validErr =
        new TopologyValidationError(TopologyValidationError.DUPLICATE_RINGS, cat.getInvalidPoint)
  }

  /**
   * Check that there is no ring which self-intersects (except of course at its endpoints).
   * This is required by OGC topology rules (but not by other models
   * such as ESRI SDE, which allow inverted shells and exverted holes).
   *
   * @param graph the topology graph of the geometry
   */
  private def checkNoSelfIntersectingRings(graph: GeometryGraph): Unit = {
    val i = graph.getEdgeIterator
    while (i.hasNext) {
      val e = i.next.asInstanceOf[Edge]
      checkNoSelfIntersectingRing(e.getEdgeIntersectionList)
      if (validErr != null) return
    }
  }

  /**
   * Check that a ring does not self-intersect, except at its endpoints.
   * Algorithm is to count the number of times each node along edge occurs.
   * If any occur more than once, that must be a self-intersection.
   */
  private def checkNoSelfIntersectingRing(eiList: EdgeIntersectionList): Unit = {
    val nodeSet = new util.TreeSet[Coordinate]
    var isFirst = true
    val i       = eiList.iterator
    while (i.hasNext) {
      val ei = i.next.asInstanceOf[EdgeIntersection]
      if (isFirst) {
        isFirst = false
      } else {
        if (nodeSet.contains(ei.coord)) {
          validErr =
            new TopologyValidationError(TopologyValidationError.RING_SELF_INTERSECTION, ei.coord)
          return
        } else nodeSet.add(ei.coord)
      }
    }
  }

  /**
   * Tests that each hole is inside the polygon shell.
   * This routine assumes that the holes have previously been tested
   * to ensure that all vertices lie on the shell or on the same side of it
   * (i.e. that the hole rings do not cross the shell ring).
   * In other words, this test is only correct if the ConsistentArea test is passed first.
   * Given this, a simple point-in-polygon test of a single point in the hole can be used,
   * provided the point is chosen such that it does not lie on the shell.
   *
   * @param p     the polygon to be tested for hole inclusion
   * @param graph a GeometryGraph incorporating the polygon
   */
  private def checkHolesInShell(p: Polygon, graph: GeometryGraph): Unit = { // skip test if no holes are present
    if (p.getNumInteriorRing <= 0) return
    val shell        = p.getExteriorRing
    val isShellEmpty = shell.isEmpty
    //PointInRing pir = new SimplePointInRing(shell); // testing only
    val pir          = new IndexedPointInAreaLocator(shell)
    var i            = 0
    while (i < p.getNumInteriorRing) {
      val hole               = p.getInteriorRingN(i)
      var holePt: Coordinate = null
      if (!hole.isEmpty) {
        holePt = IsValidOp.findPtNotNode(hole.getCoordinates, shell, graph)

        /**
         * If no non-node hole vertex can be found, the hole must
         * split the polygon into disconnected interiors.
         * This will be caught by a subsequent check.
         */
        if (holePt == null) return
        val outside = isShellEmpty || (Location.EXTERIOR == pir.locate(holePt))
        if (outside) {
          validErr = new TopologyValidationError(TopologyValidationError.HOLE_OUTSIDE_SHELL, holePt)
          return
        }
      }
      i += 1
    }
  }

  /**
   * Tests that no hole is nested inside another hole.
   * This routine assumes that the holes are disjoint.
   * To ensure this, holes have previously been tested
   * to ensure that:
   * <ul>
   * <li>they do not partially overlap
   * (checked by <code>checkRelateConsistency</code>)
   * <li>they are not identical
   * (checked by <code>checkRelateConsistency</code>)
   * </ul>
   */
  private def checkHolesNotNested(p: Polygon, graph: GeometryGraph): Unit = {
    if (p.getNumInteriorRing <= 0) return ()
    val nestedTester = new IndexedNestedRingTester(graph)
    //SimpleNestedRingTester nestedTester = new SimpleNestedRingTester(arg[0]);
    //SweeplineNestedRingTester nestedTester = new SweeplineNestedRingTester(arg[0]);
    var i            = 0
    while (i < p.getNumInteriorRing) {
      val innerHole = p.getInteriorRingN(i)
      if (!innerHole.isEmpty) {
        nestedTester.add(innerHole)
      }
      i += 1
    }
    val isNonNested  = nestedTester.isNonNested
    if (!isNonNested)
      validErr = new TopologyValidationError(TopologyValidationError.NESTED_HOLES,
                                             nestedTester.getNestedPoint
      )
  }

  /**
   * Tests that no element polygon is wholly in the interior of another element polygon.
   * <p>
   * Preconditions:
   * <ul>
   * <li>shells do not partially overlap
   * <li>shells do not touch along an edge
   * <li>no duplicate rings exist
   * </ul>
   * This routine relies on the fact that while polygon shells may touch at one or
   * more vertices, they cannot touch at ALL vertices.
   */
  private def checkShellsNotNested(mp: MultiPolygon, graph: GeometryGraph): Unit = {
    var i = 0
    while (i < mp.getNumGeometries) {
      val p     = mp.getGeometryN(i).asInstanceOf[Polygon]
      val shell = p.getExteriorRing
      var j     = 0
      while (j < mp.getNumGeometries) {
        if (i != j) {
          val p2 = mp.getGeometryN(j).asInstanceOf[Polygon]
          checkShellNotNested(shell, p2, graph)
          if (validErr != null) return
          j += 1
        }
        i += 1
      }
    }
  }

  /**
   * Check if a shell is incorrectly nested within a polygon.  This is the case
   * if the shell is inside the polygon shell, but not inside a polygon hole.
   * (If the shell is inside a polygon hole, the nesting is valid.)
   * <p>
   * The algorithm used relies on the fact that the rings must be properly contained.
   * E.g. they cannot partially overlap (this has been previously checked by
   * <code>checkRelateConsistency</code> )
   */
  private def checkShellNotNested(shell: LinearRing, p: Polygon, graph: GeometryGraph): Unit = {
    val shellPts        = shell.getCoordinates
    // test if shell is inside polygon shell
    val polyShell       = p.getExteriorRing
    if (polyShell.isEmpty) return ()
    val polyPts         = polyShell.getCoordinates
    val shellPt         = IsValidOp.findPtNotNode(shellPts, polyShell, graph)
    // if no point could be found, we can assume that the shell is outside the polygon
    if (shellPt == null) return ()
    val insidePolyShell = PointLocation.isInRing(shellPt, polyPts)
    if (!insidePolyShell) return ()
    // if no holes, this is an error!
    if (p.getNumInteriorRing <= 0) {
      validErr = new TopologyValidationError(TopologyValidationError.NESTED_SHELLS, shellPt)
      return ()
    }

    /**
     * Check if the shell is inside one of the holes.
     * This is the case if one of the calls to checkShellInsideHole
     * returns a null coordinate.
     * Otherwise, the shell is not properly contained in a hole, which is an error.
     */
    var badNestedPt: Coordinate = null
    var i                       = 0
    while (i < p.getNumInteriorRing) {
      val hole = p.getInteriorRingN(i)
      badNestedPt = checkShellInsideHole(shell, hole, graph)
      if (badNestedPt == null) return ()
      i += 1
    }
    validErr = new TopologyValidationError(TopologyValidationError.NESTED_SHELLS, badNestedPt)
  }

  /**
   * This routine checks to see if a shell is properly contained in a hole.
   * It assumes that the edges of the shell and hole do not
   * properly intersect.
   *
   * return <code>null</code> if the shell is properly contained, or
   *         a Coordinate which is not inside the hole if it is not
   */
  private def checkShellInsideHole(
    shell: LinearRing,
    hole:  LinearRing,
    graph: GeometryGraph
  ): Coordinate = {
    val shellPts = shell.getCoordinates
    val holePts  = hole.getCoordinates
    // TODO: improve performance of this - by sorting pointlists for instance?
    val shellPt  = IsValidOp.findPtNotNode(shellPts, hole, graph)
    // if point is on shell but not hole, check that the shell is inside the hole
    if (shellPt != null) {
      val insideHole = PointLocation.isInRing(shellPt, holePts)
      if (!insideHole) return shellPt
    }
    val holePt   = IsValidOp.findPtNotNode(holePts, shell, graph)
    // if point is on hole but not shell, check that the hole is outside the shell
    if (holePt != null) {
      val insideShell = PointLocation.isInRing(holePt, shellPts)
      if (insideShell) return holePt
      return null
    }
    Assert.shouldNeverReachHere("points in shell and hole appear to be equal")
    null
  }

  private def checkConnectedInteriors(graph: GeometryGraph): Unit = {
    val cit = new ConnectedInteriorTester(graph)
    if (!cit.isInteriorsConnected)
      validErr = new TopologyValidationError(TopologyValidationError.DISCONNECTED_INTERIOR,
                                             cit.getCoordinate
      )
  }
}
