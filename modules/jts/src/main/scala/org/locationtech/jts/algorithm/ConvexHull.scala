// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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
package org.locationtech.jts.algorithm

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateArrays
import org.locationtech.jts.geom.CoordinateList
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.LinearRing
import org.locationtech.jts.util.Assert
import org.locationtech.jts.util.UniqueCoordinateArrayFilter

import java.util
import java.util.Comparator

/**
 * Computes the convex hull of a {link Geometry}. The convex hull is the smallest convex Geometry
 * that contains all the points in the input Geometry. <p> Uses the Graham Scan algorithm.
 *
 * @version 1.7
 */
object ConvexHull {
  private def extractCoordinates(geom: Geometry) = {
    val filter = new UniqueCoordinateArrayFilter
    geom.applyF(filter)
    filter.getCoordinates
  }

  /**
   * Compares {link Coordinate}s for their angle and distance relative to an origin.
   *
   * @author
   *   Martin Davis
   * @version 1.7
   */
  private object RadialComparator {

    /**
     * Given two points p and q compare them with respect to their radial ordering about point o.
     * First checks radial ordering. If points are collinear, the comparison is based on their
     * distance to the origin. <p> p < q iff <ul> <li>ang(o-p) < ang(o-q) (e.g. o-p-q is CCW) <li>or
     * ang(o-p) == ang(o-q) && dist(o,p) < dist(o,q) </ul>
     *
     * @param o
     *   the origin
     * @param p
     *   a point
     * @param q
     *   another point return -1, 0 or 1 depending on whether p is less than, equal to or greater
     *   than q
     */
    private def polarCompare(o: Coordinate, p: Coordinate, q: Coordinate): Int = {
      val dxp = p.x - o.x
      val dyp = p.y - o.y
      val dxq = q.x - o.x
      val dyq = q.y - o.y

      /*
            // MD - non-robust
            int result = 0;
            double alph = Math.atan2(dxp, dyp);
            double beta = Math.atan2(dxq, dyq);
            if (alph < beta) {
              result = -1;
            }
            if (alph > beta) {
              result = 1;
            }
            if (result !=  0) return result;
       */
      val orient: Int = Orientation.index(o, p, q)

      if (orient == Orientation.COUNTERCLOCKWISE) {
        return 1
      }
      if (orient == Orientation.CLOCKWISE) {
        return -1
      }
      // points are collinear - check distance
      val op: Double = dxp * dxp + dyp * dyp
      val oq: Double = dxq * dxq + dyq * dyq
      if (op < oq) {
        return -1
      }
      if (op > oq) {
        return 1
      }
      return 0
    }
  }

  private class RadialComparator(var origin: Coordinate) extends Comparator[Coordinate] {
    override def compare(p1: Coordinate, p2: Coordinate): Int =
      RadialComparator.polarCompare(origin, p1, p2)
  }

}

class ConvexHull(val pts: Array[Coordinate], var geomFactory: GeometryFactory) {

  /**
   * Create a new convex hull construction for the input {link Coordinate} array.
   */
//inputPts = pts;

  private val inputPts: Array[Coordinate] = UniqueCoordinateArrayFilter.filterCoordinates(pts)

  /**
   * Create a new convex hull construction for the input {link Geometry}.
   */
  def this(geometry: Geometry) =
    this(ConvexHull.extractCoordinates(geometry), geometry.getFactory)

  /**
   * Returns a {link Geometry} that represents the convex hull of the input geometry. The returned
   * geometry contains the minimal number of points needed to represent the convex hull. In
   * particular, no more than two consecutive points will be collinear.
   *
   * return if the convex hull contains 3 or more points, a {@link Polygon} ; 2 points, a { @link
   * LineString}; 1 point, a {@link Point} ; 0 points, an empty {@link GeometryCollection} .
   */
  def getConvexHull: Geometry = {
    if (inputPts.length == 0) {
      return geomFactory.createGeometryCollection
    }
    if (inputPts.length == 1) {
      return geomFactory.createPoint(inputPts(0))
    }
    if (inputPts.length == 2) {
      return geomFactory.createLineString(inputPts)
    }
    var reducedPts: Array[Coordinate] = inputPts
    // use heuristic to reduce points, if large
    if (inputPts.length > 50) {
      reducedPts = reduce(inputPts)
    }
    // sort points for Graham scan.
    val sortedPts: Array[Coordinate]  = preSort(reducedPts)
    // Use Graham scan to find convex hull.
    val cHS: util.Stack[_]            = grahamScan(sortedPts)
    // Convert stack to an array.
    val cH: Array[Coordinate]         = toCoordinateArray(cHS)
    // Convert array to appropriate output geometry.
    return lineOrPolygon(cH)
  }

  /**
   * An alternative to Stack.toArray, which is not present in earlier versions of Java.
   */
  protected def toCoordinateArray(stack: util.Stack[_]): Array[Coordinate] = {
    val coordinates: Array[Coordinate] = new Array[Coordinate](stack.size)
    var i: Int                         = 0
    while (i < stack.size) {
      val coordinate: Coordinate = stack.get(i).asInstanceOf[Coordinate]
      coordinates(i) = coordinate

      {
        i += 1; i - 1
      }
    }
    return coordinates
  }

  /**
   * Uses a heuristic to reduce the number of points scanned to compute the hull. The heuristic is
   * to find a polygon guaranteed to be in (or on) the hull, and eliminate all points inside it. A
   * quadrilateral defined by the extremal points in the four orthogonal directions can be used, but
   * even more inclusive is to use an octilateral defined by the points in the 8 cardinal
   * directions. <p> Note that even if the method used to determine the polygon vertices is not 100%
   * robust, this does not affect the robustness of the convex hull. <p> To satisfy the requirements
   * of the Graham Scan algorithm, the returned array has at least 3 entries.
   *
   * @param pts
   *   the points to reduce return the reduced list of points (at least 3)
   */
  private def reduce(inputPts: Array[Coordinate]): Array[Coordinate] = { // Coordinate[] polyPts = computeQuad(inputPts);
    val polyPts: Array[Coordinate]           = computeOctRing(inputPts)
    // Coordinate[] polyPts = null;
    // unable to compute interior polygon for some reason
    if (polyPts == null) {
      return inputPts
    }
    //    LinearRing ring = geomFactory.createLinearRing(polyPts);
    //    System.out.println(ring);
    // add points defining polygon
    val reducedSet: util.TreeSet[Coordinate] = new util.TreeSet[Coordinate]
    var i: Int                               = 0
    while (i < polyPts.length) {
      reducedSet.add(polyPts(i))

      {
        i += 1; i - 1
      }
    }

    /**
     * Add all unique points not in the interior poly. CGAlgorithms.isPointInRing is not defined for
     * points actually on the ring, but this doesn't matter since the points of the interior polygon
     * are forced to be in the reduced set.
     */
    i = 0
    while (i < inputPts.length) {
      if (!PointLocation.isInRing(inputPts(i), polyPts)) {
        reducedSet.add(inputPts(i))
      }
      i += 1
    }
    val reducedPts: Array[Coordinate] = CoordinateArrays.toCoordinateArray(reducedSet)
    // ensure that computed array has at least 3 points (not necessarily unique)
    if (reducedPts.length < 3) {
      return padArray3(reducedPts)
    }
    reducedPts
  }

  private def padArray3(pts: Array[Coordinate]): Array[Coordinate] = {
    val pad: Array[Coordinate] = new Array[Coordinate](3)
    var i: Int                 = 0
    while (i < pad.length) {
      if (i < pts.length) {
        pad(i) = pts(i)
      } else {
        pad(i) = pts(0)
      }
      i += 1
    }
    pad
  }

  private def preSort(pts: Array[Coordinate]): Array[Coordinate] = {
    var t: Coordinate = null
    // find the lowest point in the set. If two or more points have
    // the same minimum y coordinate choose the one with the minimu x.
    // This focal point is put in array location pts[0].
    var i: Int        = 1
    while (i < pts.length) {
      if ((pts(i).y < pts(0).y) || ((pts(i).y == pts(0).y) && (pts(i).x < pts(0).x))) {
        t = pts(0)
        pts(0) = pts(i)
        pts(i) = t
      }
      i += 1
    }
    // sort the points radially around the focal point.
    util.Arrays.sort(pts, 1, pts.length, new ConvexHull.RadialComparator(pts(0)))
    // radialSort(pts);
    return pts
  }

  /**
   * Uses the Graham Scan algorithm to compute the convex hull vertices.
   *
   * @param c
   *   a list of points, with at least 3 entries return a Stack containing the ordered points of the
   *   convex hull ring
   */
  private def grahamScan(c: Array[Coordinate]): util.Stack[Coordinate] = {
    var p: Coordinate              = null
    val ps: util.Stack[Coordinate] = new util.Stack[Coordinate]
    ps.push(c(0))
    ps.push(c(1))
    ps.push(c(2))
    var i: Int                     = 3
    while (i < c.length) {
      p = ps.pop.asInstanceOf[Coordinate]
      // check for empty stack to guard against robustness problems
      while (!ps.empty && Orientation.index(ps.peek.asInstanceOf[Coordinate], p, c(i)) > 0)
        p = ps.pop.asInstanceOf[Coordinate]
      ps.push(p)
      ps.push(c(i))

      {
        i += 1; i - 1
      }
    }
    ps.push(c(0))
    return ps
  }

  /**
   * return whether the three coordinates are collinear and c2 lies between c1 and c3 inclusive
   */
  private def isBetween(c1: Coordinate, c2: Coordinate, c3: Coordinate): Boolean = {
    if (Orientation.index(c1, c2, c3) != 0) {
      return false
    }
    if (c1.x != c3.x) {
      if (c1.x <= c2.x && c2.x <= c3.x) {
        return true
      }
      if (c3.x <= c2.x && c2.x <= c1.x) {
        return true
      }
    }
    if (c1.y != c3.y) {
      if (c1.y <= c2.y && c2.y <= c3.y) {
        return true
      }
      if (c3.y <= c2.y && c2.y <= c1.y) {
        return true
      }
    }
    return false
  }

  private def computeOctRing(inputPts: Array[Coordinate]): Array[Coordinate] = {
    val octPts: Array[Coordinate] = computeOctPts(inputPts)
    val coordList: CoordinateList = new CoordinateList(Array.empty[Coordinate])
    coordList.add(octPts, false)
    // points must all lie in a line
    if (coordList.size < 3) {
      return null
    }
    coordList.closeRing()
    coordList.toCoordinateArray
  }

  private def computeOctPts(inputPts: Array[Coordinate]): Array[Coordinate] = {
    val pts: Array[Coordinate] = new Array[Coordinate](8)
    var j: Int                 = 0
    while (j < pts.length) {
      pts(j) = inputPts(0)

      {
        j += 1; j - 1
      }
    }
    var i: Int                 = 1
    while (i < inputPts.length) {
      if (inputPts(i).x < pts(0).x) {
        pts(0) = inputPts(i)
      }
      if (inputPts(i).x - inputPts(i).y < pts(1).x - pts(1).y) {
        pts(1) = inputPts(i)
      }
      if (inputPts(i).y > pts(2).y) {
        pts(2) = inputPts(i)
      }
      if (inputPts(i).x + inputPts(i).y > pts(3).x + pts(3).y) {
        pts(3) = inputPts(i)
      }
      if (inputPts(i).x > pts(4).x) {
        pts(4) = inputPts(i)
      }
      if (inputPts(i).x - inputPts(i).y > pts(5).x - pts(5).y) {
        pts(5) = inputPts(i)
      }
      if (inputPts(i).y < pts(6).y) {
        pts(6) = inputPts(i)
      }
      if (inputPts(i).x + inputPts(i).y < pts(7).x + pts(7).y) {
        pts(7) = inputPts(i)
      }

      {
        i += 1; i - 1
      }
    }
    return pts
  }

  /**
   * @param vertices
   *   the vertices of a linear ring, which may or may not be flattened (i.e. vertices collinear)
   *   return a 2-vertex <code>LineString</code> if the vertices are collinear; otherwise, a
   *   <code>Polygon</code> with unnecessary (collinear) vertices removed
   */
  private def lineOrPolygon(coordinate: Array[Coordinate]): Geometry = {
    val coordinates            = cleanRing(coordinate)
    if (coordinates.length == 3) {
      return geomFactory.createLineString(Array[Coordinate](coordinates(0), coordinates(1)))
      //      return new LineString(new Coordinate[]{coordinates[0], coordinates[1]},
      //          geometry.getPrecisionModel(), geometry.getSRID());
    }
    val linearRing: LinearRing = geomFactory.createLinearRing(coordinates)
    geomFactory.createPolygon(linearRing)
  }

  /**
   * @param vertices
   *   the vertices of a linear ring, which may or may not be flattened (i.e. vertices collinear)
   *   return the coordinates with unnecessary (collinear) vertices removed
   */
  private def cleanRing(original: Array[Coordinate]): Array[Coordinate] = {
    Assert.equals(original(0), original(original.length - 1))
    val cleanedRing: util.ArrayList[Coordinate]   = new util.ArrayList[Coordinate]
    var previousDistinctCoordinate: Coordinate    = null
    var i: Int                                    = 0
    while (i <= original.length - 2) {
      val currentCoordinate: Coordinate = original(i)
      val nextCoordinate: Coordinate    = original(i + 1)
      if (
        !(currentCoordinate == nextCoordinate || (previousDistinctCoordinate != null && isBetween(
          previousDistinctCoordinate,
          currentCoordinate,
          nextCoordinate
        )))
      ) {
        cleanedRing.add(currentCoordinate)
        previousDistinctCoordinate = currentCoordinate
      }
      i += 1
    }
    cleanedRing.add(original(original.length - 1))
    val cleanedRingCoordinates: Array[Coordinate] = new Array[Coordinate](cleanedRing.size)
    return cleanedRing.toArray(cleanedRingCoordinates).asInstanceOf[Array[Coordinate]]
  }
}
