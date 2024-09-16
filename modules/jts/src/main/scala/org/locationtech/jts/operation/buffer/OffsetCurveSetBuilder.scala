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
package org.locationtech.jts.operation.buffer

/**
 * @version 1.7
 */

import org.locationtech.jts.algorithm.Distance
import org.locationtech.jts.algorithm.Orientation
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
import org.locationtech.jts.geom.Position
import org.locationtech.jts.geom.Triangle
import org.locationtech.jts.geomgraph.Label
import org.locationtech.jts.noding.NodedSegmentString
import org.locationtech.jts.noding.SegmentString

import java.util

/**
 * Creates all the raw offset curves for a buffer of a {link Geometry}. Raw curves need to be noded
 * together and polygonized to form the final buffer area.
 *
 * @version 1.7
 */
class OffsetCurveSetBuilder(
  var inputGeom:    Geometry,
  var distance:     Double,
  var curveBuilder: OffsetCurveBuilder
) {
  private val curveList = new util.ArrayList[SegmentString]

  // /**
  //  * Computes the set of raw offset curves for the buffer.
  //  * Each offset curve has an attached {link Label} indicating
  //  * its left and right location.
  //  *
  //  * return a Collection of SegmentStrings representing the raw buffer curves
  //  */
  def getCurves: util.List[SegmentString] = {
    add(inputGeom)
    curveList
  }

  /**
   * Creates a {link SegmentString} for a coordinate list which is a raw offset curve, and adds it
   * to the list of buffer curves. The SegmentString is tagged with a Label giving the topology of
   * the curve. The curve may be oriented in either direction. If the curve is oriented CW, the
   * locations will be: <br>Left: Location.EXTERIOR <br>Right: Location.INTERIOR
   */
  private def addCurve(coord: Array[Coordinate], leftLoc: Int, rightLoc: Int): Unit = { // don't add null or trivial curves
    if (coord == null || coord.length < 2) return
    // add the edge for a coordinate list which is a raw offset curve
    val e = new NodedSegmentString(coord, new Label(0, Location.BOUNDARY, leftLoc, rightLoc))
    curveList.add(e)
    ()
  }

  private def add(g: Geometry): Unit = {
    if (g.isEmpty) return
    if (g.isInstanceOf[Polygon]) addPolygon(g.asInstanceOf[Polygon])
    else { // LineString also handles LinearRings
      g match {
        case string1: LineString => addLineString(string1)
        case _                   =>
          g match {
            case point: Point                   => addPoint(point)
            case point: MultiPoint              => addCollection(point)
            case string: MultiLineString        => addCollection(string)
            case polygon: MultiPolygon          => addCollection(polygon)
            case collection: GeometryCollection => addCollection(collection)
            case _                              => throw new UnsupportedOperationException(g.getClass.getName)
          }
      }
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
  private def addPoint(p: Point): Unit = { // a zero or negative width buffer of a point is empty
    if (distance <= 0.0) return
    val coord = p.getCoordinates
    val curve = curveBuilder.getLineCurve(coord, distance)
    addCurve(curve, Location.EXTERIOR, Location.INTERIOR)
  }

  private def addLineString(line: LineString): Unit = {
    if (curveBuilder.isLineOffsetEmpty(distance)) return
    val coord = CoordinateArrays.removeRepeatedPoints(line.getCoordinates)

    /**
     * Rings (closed lines) are generated with a continuous curve, with no end arcs. This produces
     * better quality linework, and avoids noding issues with arcs around almost-parallel end
     * segments. See JTS #523 and #518.
     *
     * Singled-sided buffers currently treat rings as if they are lines.
     */
    if (CoordinateArrays.isRing(coord) && !curveBuilder.getBufferParameters.isSingleSided)
      addRingBothSides(coord, distance)
    else {
      val curve = curveBuilder.getLineCurve(coord, distance)
      addCurve(curve, Location.EXTERIOR, Location.INTERIOR)
    }
    // TESTING
    // Coordinate[] curveTrim = BufferCurveLoopPruner.prune(curve);
    // addCurve(curveTrim, Location.EXTERIOR, Location.INTERIOR);
  }

  private def addPolygon(p: Polygon): Unit = {
    var offsetDistance = distance
    var offsetSide     = Position.LEFT
    if (distance < 0.0) {
      offsetDistance = -distance
      offsetSide = Position.RIGHT
    }
    val shell          = p.getExteriorRing
    val shellCoord     = CoordinateArrays.removeRepeatedPoints(shell.getCoordinates)
    // optimization - don't bother computing buffer
    // if the polygon would be completely eroded
    if (distance < 0.0 && isErodedCompletely(shell, distance)) return
    // don't attempt to buffer a polygon with too few distinct vertices
    if (distance <= 0.0 && shellCoord.length < 3) return
    addRingSide(shellCoord, offsetDistance, offsetSide, Location.EXTERIOR, Location.INTERIOR)
    var i              = 0
    while (i < p.getNumInteriorRing) {
      val hole      = p.getInteriorRingN(i)
      val holeCoord = CoordinateArrays.removeRepeatedPoints(hole.getCoordinates)
      // optimization - don't bother computing buffer for this hole
      // if the hole would be completely covered
      if (!(distance > 0.0 && isErodedCompletely(hole, -distance))) {
        // Holes are topologically labelled opposite to the shell, since
        // the interior of the polygon lies on their opposite side
        // (on the left, if the hole is oriented CCW)
        addRingSide(holeCoord,
                    offsetDistance,
                    Position.opposite(offsetSide),
                    Location.INTERIOR,
                    Location.EXTERIOR
        )
      }
      i += 1
    }
  }

  private def addRingBothSides(coord: Array[Coordinate], distance: Double): Unit = {
    addRingSide(coord, distance, Position.LEFT, Location.EXTERIOR, Location.INTERIOR)
    /* Add the opposite side of the ring
     */
    addRingSide(coord, distance, Position.RIGHT, Location.INTERIOR, Location.EXTERIOR)
  }

  /**
   * Adds an offset curve for one side of a ring. The side and left and right topological location
   * arguments are provided as if the ring is oriented CW. (If the ring is in the opposite
   * orientation, this is detected and the left and right locations are interchanged and the side is
   * flipped.)
   *
   * @param coord
   *   the coordinates of the ring (must not contain repeated points)
   * @param offsetDistance
   *   the positive distance at which to create the buffer
   * @param side
   *   the side {@link Position} of the ring on which to construct the buffer line
   * @param cwLeftLoc
   *   the location on the L side of the ring (if it is CW)
   * @param cwRightLoc
   *   the location on the R side of the ring (if it is CW)
   */
  private def addRingSide(
    coord:          Array[Coordinate],
    offsetDistance: Double,
    sideArg:        Int,
    cwLeftLoc:      Int,
    cwRightLoc:     Int
  ): Unit = {
    // don't bother adding ring if it is "flat" and will disappear in the output
    var side     = sideArg
    if (offsetDistance == 0.0 && coord.length < LinearRing.MINIMUM_VALID_SIZE) return
    var leftLoc  = cwLeftLoc
    var rightLoc = cwRightLoc
    if (coord.length >= LinearRing.MINIMUM_VALID_SIZE && Orientation.isCCWArea(coord)) {
      leftLoc = cwRightLoc
      rightLoc = cwLeftLoc
      side = Position.opposite(side)
    }
    val curve    = curveBuilder.getRingCurve(coord, side, offsetDistance)
    addCurve(curve, leftLoc, rightLoc)
  }

  /**
   * The ringCoord is assumed to contain no repeated points. It may be degenerate (i.e. contain only
   * 1, 2, or 3 points). In this case it has no area, and hence has a minimum diameter of 0.
   *
   * @param ringCoord
   * @param offsetDistance
   *   return
   */
  private def isErodedCompletely(ring: LinearRing, bufferDistance: Double): Boolean = {
    val ringCoord       = ring.getCoordinates
    // degenerate ring has no area
    if (ringCoord.length < 4) return bufferDistance < 0
    // important test to eliminate inverted triangle bug
    // also optimizes erosion test for triangles
    if (ringCoord.length == 4) return isTriangleErodedCompletely(ringCoord, bufferDistance)
    // if envelope is narrower than twice the buffer distance, ring is eroded
    val env             = ring.getEnvelopeInternal
    val envMinDimension = Math.min(env.getHeight, env.getWidth)
    if (bufferDistance < 0.0 && 2 * Math.abs(bufferDistance) > envMinDimension) return true
    false

    /**
     * The following is a heuristic test to determine whether an inside buffer will be eroded
     * completely. It is based on the fact that the minimum diameter of the ring pointset provides
     * an upper bound on the buffer distance which would erode the ring. If the buffer distance is
     * less than the minimum diameter, the ring may still be eroded, but this will be determined by
     * a full topological computation.
     */
    // System.out.println(ring);
    /* MD  7 Feb 2005 - there's an unknown bug in the MD code, so disable this for now
          MinimumDiameter md = new MinimumDiameter(ring);
          minDiam = md.getLength();
          //System.out.println(md.getDiameter());
          return minDiam < 2 * Math.abs(bufferDistance);
     */
  }

  /**
   * Tests whether a triangular ring would be eroded completely by the given buffer distance. This
   * is a precise test. It uses the fact that the inner buffer of a triangle converges on the
   * inCentre of the triangle (the point equidistant from all sides). If the buffer distance is
   * greater than the distance of the inCentre from a side, the triangle will be eroded completely.
   *
   * This test is important, since it removes a problematic case where the buffer distance is
   * slightly larger than the inCentre distance. In this case the triangle buffer curve "inverts"
   * with incorrect topology, producing an incorrect hole in the buffer.
   *
   * @param triangleCoord
   * @param bufferDistance
   *   return
   */
  private def isTriangleErodedCompletely(
    triangleCoord:  Array[Coordinate],
    bufferDistance: Double
  ) = {
    val tri          = new Triangle(triangleCoord(0), triangleCoord(1), triangleCoord(2))
    val inCentre     = tri.inCentre
    val distToCentre = Distance.pointToSegment(inCentre, tri.p0, tri.p1)
    distToCentre < Math.abs(bufferDistance)
  }
}
