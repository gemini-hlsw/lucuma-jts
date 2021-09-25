/*
 * Copyright (c) 2019 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */ /*
 * Copyright (c) 2019 Martin Davis.
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

import java.util
import org.locationtech.jts.algorithm.Angle
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateList
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.LineSegment
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.Polygon

/**
 * Creates a buffer polygon with a varying buffer distance at each vertex along a line. <p> Only
 * single lines are supported as input, since buffer widths are typically specified individually for
 * each line.
 *
 * @author
 *   Martin Davis
 */
object VariableBuffer {

  /**
   * Creates a buffer polygon along a line with the buffer distance interpolated between a start
   * distance and an end distance.
   *
   * @param line
   *   the line to buffer
   * @param startDistance
   *   the buffer width at the start of the line
   * @param endDistance
   *   the buffer width at the end of the line return the variable-distance buffer polygon
   */
  def buffer(line: Geometry, startDistance: Double, endDistance: Double) = {
    val distance = interpolate(line.asInstanceOf[LineString], startDistance, endDistance)
    val vb       = new VariableBuffer(line, distance)
    vb.getResult
  }

  // /**
  //  * Creates a buffer polygon along a line with the buffer distance interpolated
  //  * between a start distance, a middle distance and an end distance.
  //  * The middle distance is attained at
  //  * the vertex at or just past the half-length of the line.
  //  * For smooth buffering of a {link LinearRing} (or the rings of a {link Polygon})
  //  * the start distance and end distance should be equal.
  //  *
  //  * @param line          the line to buffer
  //  * @param startDistance the buffer width at the start of the line
  //  * @param midDistance   the buffer width at the middle vertex of the line
  //  * @param endDistance   the buffer width at the end of the line
  //  * return the variable-distance buffer polygon
  //  */
  def buffer(line: Geometry, startDistance: Double, midDistance: Double, endDistance: Double) = {
    val distance =
      interpolate(line.asInstanceOf[LineString], startDistance, midDistance, endDistance)
    val vb       = new VariableBuffer(line, distance)
    vb.getResult
  }

  /**
   * Creates a buffer polygon along a line with the distance specified at each vertex.
   *
   * @param line
   *   the line to buffer
   * @param distance
   *   the buffer distance for each vertex of the line return the variable-distance buffer polygon
   */
  def buffer(line: Geometry, distance: Array[Double]) = {
    val vb = new VariableBuffer(line, distance)
    vb.getResult
  }

  /**
   * Computes a list of values for the points along a line by interpolating between values for the
   * start and end point. The interpolation is based on the distance of each point along the line
   * relative to the total line length.
   *
   * @param line
   *   the line to interpolate along
   * @param startValue
   *   the start value
   * @param endValue
   *   the end value return the array of interpolated values
   */
  private def interpolate(line: LineString, startValueArg: Double, endValueArg: Double) = {
    val startValue      = Math.abs(startValueArg)
    val endValue        = Math.abs(endValueArg)
    val values          = new Array[Double](line.getNumPoints)
    values(0) = startValue
    values(values.length - 1) = endValue
    val totalLen        = line.getLength
    val pts             = line.getCoordinates
    var currLen: Double = 0
    var i               = 1
    while (i < values.length - 1) {
      val segLen  = pts(i).distance(pts(i - 1))
      currLen += segLen
      val lenFrac = currLen / totalLen
      val delta   = lenFrac * (endValue - startValue)
      values(i) = startValue + delta
      i += 1
    }
    values
  }

  /**
   * Computes a list of values for the points along a line by interpolating between values for the
   * start, middle and end points. The interpolation is based on the distance of each point along
   * the line relative to the total line length. The middle distance is attained at the vertex at or
   * just past the half-length of the line.
   *
   * @param line
   *   the line to interpolate along
   * @param startValue
   *   the start value
   * @param midValue
   *   the start value
   * @param endValue
   *   the end value return the array of interpolated values
   */
  private def interpolate(
    line:          LineString,
    startValueArg: Double,
    midValueArg:   Double,
    endValueArg:   Double
  ) = {
    val startValue      = Math.abs(startValueArg)
    val midValue        = Math.abs(midValueArg)
    val endValue        = Math.abs(endValueArg)
    val values          = new Array[Double](line.getNumPoints)
    values(0) = startValue
    values(values.length - 1) = endValue
    val pts             = line.getCoordinates
    val lineLen         = line.getLength
    val midIndex        = indexAtLength(pts, lineLen / 2)
    val delMidStart     = midValue - startValue
    val delEndMid       = endValue - midValue
    val lenSM           = length(pts, 0, midIndex)
    var currLen: Double = 0
    var i               = 1
    while (i <= midIndex) {
      val segLen  = pts(i).distance(pts(i - 1))
      currLen += segLen
      val lenFrac = currLen / lenSM
      val `val`   = startValue + lenFrac * delMidStart
      values(i) = `val`
      i += 1
    }
    val lenME           = length(pts, midIndex, pts.length - 1)
    currLen = 0
    i = midIndex + 1
    while (i < values.length - 1) {
      val segLen  = pts(i).distance(pts(i - 1))
      currLen += segLen
      val lenFrac = currLen / lenME
      val `val`   = midValue + lenFrac * delEndMid
      values(i) = `val`
      i += 1
    }
    values
  }

  private def indexAtLength(pts: Array[Coordinate], targetLen: Double): Int = {
    var len: Double = 0
    var i           = 1
    while (i < pts.length) {
      len += pts(i).distance(pts(i - 1))
      if (len > targetLen) return i
      i += 1
    }
    pts.length - 1
  }

  private def length(pts: Array[Coordinate], i1: Int, i2: Int)              = {
    var len: Double = 0
    var i           = i1 + 1
    while (i <= i2) {
      len += pts(i).distance(pts(i - 1))
      i += 1
    }
    len
  }

  /**
   * Computes the two circumference points defining the outer tangent line between two circles. <p>
   * For the algorithm see <a
   * href='https://en.wikipedia.org/wiki/Tangent_lines_to_circles#Outer_tangent'>Wikipedia</a>.
   *
   * @param c1
   *   the centre of circle 1
   * @param r1
   *   the radius of circle 1
   * @param c2
   *   the centre of circle 2
   * @param r2
   *   the center of circle 2 return the outer tangent line segment, or null if none exists
   */
  private def outerTangent(c1: Coordinate, r1: Double, c2: Coordinate, r2: Double): LineSegment = {

    /**
     * If distances are inverted then flip to compute and flip result back.
     */
    if (r1 > r2) {
      val seg = outerTangent(c2, r2, c1, r1)
      return new LineSegment(seg.p1, seg.p0)
    }
    val x1 = c1.getX
    val y1 = c1.getY
    val x2 = c2.getX
    val y2 = c2.getY
    // TODO: handle r1 == r2?
    val a3 = -Math.atan2(y2 - y1, x2 - x1)
    val dr = r2 - r1
    val d  = Math.sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
    val a2 = Math.asin(dr / d)
    // check if no tangent exists
    if (java.lang.Double.isNaN(a2)) return null
    val a1 = a3 - a2
    val aa = Math.PI / 2 - a1
    val x3 = x1 + r1 * Math.cos(aa)
    val y3 = y1 + r1 * Math.sin(aa)
    val x4 = x2 + r2 * Math.cos(aa)
    val y4 = y2 + r2 * Math.sin(aa)
    new LineSegment(x3, y3, x4, y4)
  }

  private def projectPolar(p: Coordinate, r: Double, ang: Double) = {
    val x = p.getX + r * snapTrig(Math.cos(ang))
    val y = p.getY + r * snapTrig(Math.sin(ang))
    new Coordinate(x, y)
  }

  private val SNAP_TRIG_TOL                                       = 1e-6

  /**
   * Snap trig values to integer values for better consistency.
   *
   * @param x
   *   the result of a trigonometric function return x snapped to the integer interval
   */
  private def snapTrig(x: Double): Double = {
    if (x > (1 - SNAP_TRIG_TOL)) return 1
    if (x < (-1 + SNAP_TRIG_TOL)) return -1
    if (Math.abs(x) < SNAP_TRIG_TOL) return 0
    x
  }
}

class VariableBuffer(val lineArg: Geometry, var distance: Array[Double]) {

  /**
   * Creates a generator for a variable-distance line buffer.
   *
   * @param line
   *   the linestring to buffer
   * @param distance
   *   the buffer distance for each vertex of the line
   */
  if (distance.length != lineArg.getNumPoints)
    throw new IllegalArgumentException("Number of distances is not equal to number of vertices")
  private val line         = lineArg.asInstanceOf[LineString]
  private val geomFactory  = line.getFactory
  private val quadrantSegs = BufferParameters.DEFAULT_QUADRANT_SEGMENTS

  /**
   * Computes the buffer polygon.
   *
   * return a buffer polygon
   */
  def getResult: Geometry = {
    val parts     = new util.ArrayList[Geometry]
    val pts       = line.getCoordinates
    // construct segment buffers
    var i         = 1
    while (i < pts.length) {
      val dist0 = distance(i - 1)
      val dist1 = distance(i)
      if (dist0 > 0 || dist1 > 0) {
        val poly = segmentBuffer(pts(i - 1), pts(i), dist0, dist1)
        if (poly != null) parts.add(poly)
      }
      {
        i += 1; i - 1
      }
    }
    val partsGeom = geomFactory.createGeometryCollection(GeometryFactory.toGeometryArray(parts))
    val buffer    = partsGeom.union
    // ensure an empty polygon is returned if needed
    if (buffer.isEmpty) return geomFactory.createPolygon
    buffer
  }

  /**
   * Computes a variable buffer polygon for a single segment, with the given endpoints and buffer
   * distances. The individual segment buffers are unioned to form the final buffer.
   *
   * @param p0
   *   the segment start point
   * @param p1
   *   the segment end point
   * @param dist0
   *   the buffer distance at the start point
   * @param dist1
   *   the buffer distance at the end point return the segment buffer.
   */
  private def segmentBuffer(
    p0:    Coordinate,
    p1:    Coordinate,
    dist0: Double,
    dist1: Double
  ): Polygon = {

    /**
     * Compute for increasing distance only, so flip if needed
     */
    if (dist0 > dist1) return segmentBuffer(p1, p0, dist1, dist0)
    // forward tangent line
    val tangent = VariableBuffer.outerTangent(p0, dist0, p1, dist1)
    // if tangent is null then compute a buffer for largest circle
    if (tangent == null) {
      var center = p0
      var dist   = dist0
      if (dist1 > dist0) {
        center = p1
        dist = dist1
      }
      return circle(center, dist)
    }
    val t0      = tangent.getCoordinate(0)
    val t1      = tangent.getCoordinate(1)
    // reverse tangent line on other side of segment
    val seg     = new LineSegment(p0, p1)
    val tr0     = seg.reflect(t0)
    val tr1     = seg.reflect(t1)
    val coords  = new CoordinateList(Array.empty)
    coords.add(t0)
    coords.add(t1)
    // end cap
    addCap(p1, dist1, t1, tr1, coords)
    coords.add(tr1)
    coords.add(tr0)
    // start cap
    addCap(p0, dist0, tr0, t0, coords)
    // close
    coords.add(t0)
    val pts     = coords.toCoordinateArray
    val polygon = geomFactory.createPolygon(pts)
    polygon
  }

  /**
   * Returns a circular polygon.
   *
   * @param center
   *   the circle center point
   * @param radius
   *   the radius return a polygon, or null if the radius is 0
   */
  private def circle(center: Coordinate, radius: Double): Polygon = {
    if (radius <= 0) return null
    val nPts   = 4 * quadrantSegs
    val pts    = new Array[Coordinate](nPts + 1)
    val angInc = Math.PI / 2 / quadrantSegs
    var i      = 0
    while (i < nPts) {
      pts(i) = VariableBuffer.projectPolar(center, radius, i * angInc)
      i += 1
    }
    pts(pts.length - 1) = pts(0).copy
    geomFactory.createPolygon(pts)
  }

  /**
   * Adds a semi-circular cap CCW around the point p.
   *
   * @param p
   *   the centre point of the cap
   * @param r
   *   the cap radius
   * @param t1
   *   the starting point of the cap
   * @param t2
   *   the ending point of the cap
   * @param coords
   *   the coordinate list to add to
   */
  private def addCap(
    p:      Coordinate,
    r:      Double,
    t1:     Coordinate,
    t2:     Coordinate,
    coords: CoordinateList
  ) = {
    var angStart   = Angle.angle(p, t1)
    val angEnd     = Angle.angle(p, t2)
    if (angStart < angEnd) angStart += 2 * Math.PI
    val indexStart = capAngleIndex(angStart)
    val indexEnd   = capAngleIndex(angEnd)
    var i          = indexStart
    while (i > indexEnd) { // use negative increment to create points CW
      val ang = capAngle(i)
      coords.add(VariableBuffer.projectPolar(p, r, ang))
      i -= 1
    }
  }

  /**
   * Computes the angle for the given cap point index.
   *
   * @param index
   *   the fillet angle index return
   */
  private def capAngle(index: Int) = {
    val capSegAng = Math.PI / 2 / quadrantSegs
    index * capSegAng
  }

  /**
   * Computes the canonical cap point index for a given angle. The angle is rounded down to the next
   * lower index. <p> In order to reduce the number of points created by overlapping end caps, cap
   * points are generated at the same locations around a circle. The index is the index of the
   * points around the circle, with 0 being the point at (1,0). The total number of points around
   * the circle is <code>4 * quadrantSegs</code>.
   *
   * @param ang
   *   the angle return the index for the angle.
   */
  private def capAngleIndex(ang: Double) = {
    val capSegAng = Math.PI / 2 / quadrantSegs
    val index     = (ang / capSegAng).toInt
    index
  }
}
