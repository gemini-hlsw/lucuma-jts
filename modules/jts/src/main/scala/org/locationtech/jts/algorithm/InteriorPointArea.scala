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

import java.util
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryCollection
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.LinearRing
import org.locationtech.jts.geom.Polygon
import org.locationtech.jts.util.Assert

/**
 * Computes a point in the interior of an areal geometry. The point will lie in the geometry
 * interior in all except certain pathological cases.
 *
 * <h2>Algorithm</h2> For each input polygon: <ul> <li>Determine a horizontal scan line on which the
 * interior point will be located. To increase the chance of the scan line having non-zero-width
 * intersection with the polygon the scan line Y ordinate is chosen to be near the centre of the
 * polygon's Y extent but distinct from all of vertex Y ordinates. <li>Compute the sections of the
 * scan line which lie in the interior of the polygon. <li>Choose the widest interior section and
 * take its midpoint as the interior point. </ul> The final interior point is chosen as the one
 * occurring in the widest interior section. <p> This algorithm is a tradeoff between performance
 * and point quality (where points further from the geometry boundary are considered to be higher
 * quality) Priority is given to performance. This means that the computed interior point may not be
 * suitable for some uses (such as label positioning). <p> The algorithm handles some kinds of
 * invalid/degenerate geometry, including zero-area and self-intersecting polygons. <p> Empty
 * geometry is handled by returning a <code>null</code> point.
 *
 * <h3>KNOWN BUGS</h3> <ul> <li>If a fixed precision model is used, in some cases this method may
 * return a point which does not lie in the interior. <li>If the input polygon is <i>extremely</i>
 * narrow the computed point may not lie in the interior of the polygon. </ul>
 *
 * @version 1.17
 */
object InteriorPointArea {

  /**
   * Computes an interior point for the polygonal components of a Geometry.
   *
   * @param geom
   *   the geometry to compute return the computed interior point, or <code>null</code> if the
   *   geometry has no polygonal components
   */
  def getInteriorPoint(geom: Geometry): Coordinate = {
    val intPt = new InteriorPointArea(geom)
    intPt.getInteriorPoint
  }

  private def avg(a: Double, b: Double)            = (a + b) / 2.0

  /**
   * Computes an interior point in a single {link Polygon}, as well as the width of the scan-line
   * section it occurs in to allow choosing the widest section occurrence.
   *
   * @author
   *   mdavis
   */
  private object InteriorPointPolygon {

    /**
     * Tests if an edge intersection contributes to the crossing count. Some crossing situations are
     * not counted, to ensure that the list of crossings captures strict inside/outside topology.
     *
     * @param p0
     *   an endpoint of the segment
     * @param p1
     *   an endpoint of the segment
     * @param scanY
     *   the Y-ordinate of the horizontal line return true if the edge crossing is counted
     */
    private def isEdgeCrossingCounted(p0: Coordinate, p1: Coordinate, scanY: Double): Boolean = {
      val y0 = p0.getY
      val y1 = p1.getY
      // skip horizontal lines
      if (y0 == y1) return false
      // handle cases where vertices lie on scan-line
      // downward segment does not include start point
      if (y0 == scanY && y1 < scanY) return false
      // upward segment does not include endpoint
      if (y1 == scanY && y0 < scanY) return false
      true
    }

    /**
     * Computes the intersection of a segment with a horizontal line. The segment is expected to
     * cross the horizontal line
     *   - this condition is not checked. Computation uses regular double-precision arithmetic. Test
     *     seems to indicate this is as good as using DD arithmetic.
     *
     * @param p0
     *   an endpoint of the segment
     * @param p1
     *   an endpoint of the segment
     * @param Y
     *   the Y-ordinate of the horizontal line return
     */
    private def intersection(p0: Coordinate, p1: Coordinate, Y: Double): Double = {
      val x0    = p0.getX
      val x1    = p1.getX
      if (x0 == x1) return x0
      // Assert: segDX is non-zero, due to previous equality test
      val segDX = x1 - x0
      val segDY = p1.getY - p0.getY
      val m     = segDY / segDX
      val x     = x0 + ((Y - p0.getY) / m)
      x
    }

    /**
     * Tests if an envelope intersects a horizontal line.
     *
     * @param env
     *   the envelope to test
     * @param y
     *   the Y-ordinate of the horizontal line return true if the envelope and line intersect
     */
    private def intersectsHorizontalLine(env: Envelope, y: Double): Boolean = {
      if (y < env.getMinY) return false
      if (y > env.getMaxY) return false
      true
    }

    /**
     * Tests if a line segment intersects a horizontal line.
     *
     * @param p0
     *   a segment endpoint
     * @param p1
     *   a segment endpoint
     * @param y
     *   the Y-ordinate of the horizontal line return true if the segment and line intersect
     */
    private def intersectsHorizontalLine(p0: Coordinate, p1: Coordinate, y: Double): Boolean = { // both ends above?
      if (p0.getY > y && p1.getY > y) return false
      // both ends below?
      if (p0.getY < y && p1.getY < y) return false
      // segment must intersect line
      true
    }
  }

  private class InteriorPointPolygon(var polygon: Polygon) {

    /**
     * Creates a new InteriorPointPolygon instance.
     *
     * @param polygon
     *   the polygon to test
     */
    private val interiorPointY            = ScanLineYOrdinateFinder.getScanLineY(polygon)
    private var interiorSectionWidth      = 0.0
    private var interiorPoint: Coordinate = null

    /**
     * Gets the computed interior point.
     *
     * return the interior point coordinate, or <code>null</code> if the input geometry is empty
     */
    def getInteriorPoint: Coordinate = interiorPoint

    /**
     * Gets the width of the scanline section containing the interior point. Used to determine the
     * best point to use.
     *
     * return the width
     */
    def getWidth: Double = interiorSectionWidth

    /**
     * Compute the interior point.
     */
    def process(): Unit = {

      /**
       * This results in returning a null Coordinate
       */
      if (polygon.isEmpty) return

      /**
       * set default interior point in case polygon has zero area
       */
      interiorPoint = new Coordinate(polygon.getCoordinate)
      val crossings = new util.ArrayList[Double]
      scanRing(polygon.getExteriorRing, crossings)
      var i         = 0
      while (i < polygon.getNumInteriorRing) {
        scanRing(polygon.getInteriorRingN(i), crossings)
        i += 1
      }
      findBestMidpoint(crossings)
    }

    private def scanRing(ring: LinearRing, crossings: util.List[Double]): Unit = { // skip rings which don't cross scan line
      if (!InteriorPointPolygon.intersectsHorizontalLine(ring.getEnvelopeInternal, interiorPointY))
        return
      val seq = ring.getCoordinateSequence
      var i   = 1
      while (i < seq.size) {
        val ptPrev = seq.getCoordinate(i - 1)
        val pt     = seq.getCoordinate(i)
        addEdgeCrossing(ptPrev, pt, interiorPointY, crossings)
        i += 1
      }
    }

    private def addEdgeCrossing(
      p0:        Coordinate,
      p1:        Coordinate,
      scanY:     Double,
      crossings: util.List[Double]
    ): Unit = { // skip non-crossing segments
      if (!InteriorPointPolygon.intersectsHorizontalLine(p0, p1, scanY)) return
      if (!InteriorPointPolygon.isEdgeCrossingCounted(p0, p1, scanY)) return
      // edge intersects scan line, so add a crossing
      val xInt = InteriorPointPolygon.intersection(p0, p1, scanY)
      crossings.add(xInt)
      ()
      //checkIntersectionDD(p0, p1, scanY, xInt);
    }

    /**
     * Finds the midpoint of the widest interior section. Sets the {link #interiorPoint} location
     * and the {link #interiorSectionWidth}
     *
     * @param crossings
     *   the list of scan-line crossing X ordinates
     */
    private def findBestMidpoint(crossings: util.List[Double]): Unit = { // zero-area polygons will have no crossings
      if (crossings.size == 0) return
      // TODO: is there a better way to verify the crossings are correct?
      Assert.isTrue(0 == crossings.size % 2,
                    "Interior Point robustness failure: odd number of scanline crossings"
      )
      val comparator: util.Comparator[Double] = java.lang.Double.compare _
      crossings.sort(comparator)
      /*
       * Entries in crossings list are expected to occur in pairs representing a
       * section of the scan line interior to the polygon (which may be zero-length)
       */
      var i                                   = 0
      while (i < crossings.size) {
        val x1    = crossings.get(i)
        // crossings count must be even so this should be safe
        val x2    = crossings.get(i + 1)
        val width = x2 - x1
        if (width > interiorSectionWidth) {
          interiorSectionWidth = width
          val interiorPointX = avg(x1, x2)
          interiorPoint = new Coordinate(interiorPointX, interiorPointY)
        }
        i += 2
      }
    }

    /*
        // for testing only
        private static void checkIntersectionDD(Coordinate p0, Coordinate p1, double scanY, double xInt) {
          double xIntDD = intersectionDD(p0, p1, scanY);
          System.out.println(
              ((xInt != xIntDD) ? ">>" : "")
              + "IntPt x - DP: " + xInt + ", DD: " + xIntDD
              + "   y: " + scanY + "   " + WKTWriter.toLineString(p0, p1) );
        }

        private static double intersectionDD(Coordinate p0, Coordinate p1, double Y) {
          double x0 = p0.getX();
          double x1 = p1.getX();

          if ( x0 == x1 )
            return x0;

          DD segDX = DD.valueOf(x1).selfSubtract(x0);
          // Assert: segDX is non-zero, due to previous equality test
          DD segDY = DD.valueOf(p1.getY()).selfSubtract(p0.getY());
          DD m = segDY.divide(segDX);
          DD dy = DD.valueOf(Y).selfSubtract(p0.getY());
          DD dx = dy.divide(m);
          DD xInt = DD.valueOf(x0).selfAdd(dx);
          return xInt.doubleValue();
        }
     */
  }

  /**
   * Finds a safe scan line Y ordinate by projecting the polygon segments to the Y axis and finding
   * the Y-axis interval which contains the centre of the Y extent. The centre of this interval is
   * returned as the scan line Y-ordinate. <p> Note that in the case of (degenerate, invalid)
   * zero-area polygons the computed Y value may be equal to a vertex Y-ordinate.
   *
   * @author
   *   mdavis
   */
  private object ScanLineYOrdinateFinder                   {
    def getScanLineY(poly: Polygon): Double = {
      val finder = new InteriorPointArea.ScanLineYOrdinateFinder(poly)
      finder.getScanLineY
    }
  }

  private class ScanLineYOrdinateFinder(var poly: Polygon) { // initialize using extremal values
    private var centreY = .0
    private var hiY     = Double.MaxValue
    private var loY     = -Double.MaxValue
    hiY = poly.getEnvelopeInternal.getMaxY
    loY = poly.getEnvelopeInternal.getMinY
    centreY = avg(loY, hiY)

    def getScanLineY: Double                    = {
      process(poly.getExteriorRing)
      var i         = 0
      while (i < poly.getNumInteriorRing) {
        process(poly.getInteriorRingN(i))
        i += 1
      }
      val scanLineY = avg(hiY, loY)
      scanLineY
    }

    private def process(line: LineString): Unit = {
      val seq = line.getCoordinateSequence
      var i   = 0
      while (i < seq.size) {
        val y = seq.getY(i)
        updateInterval(y)
        i += 1
      }
    }

    private def updateInterval(y: Double): Unit = if (y <= centreY)
      if (y > loY) loY = y
      else if (y > centreY) if (y < hiY) hiY = y
  }

}

class InteriorPointArea(val g: Geometry) {

  /**
   * Creates a new interior point finder for an areal geometry.
   *
   * @param g
   *   an areal geometry
   */
  process(g)
  private var interiorPoint: Coordinate = null
  private var maxWidth: Double          = -1

  /**
   * Gets the computed interior point.
   *
   * return the coordinate of an interior point or <code>null</code> if the input geometry is empty
   */
  def getInteriorPoint: Coordinate = interiorPoint

  /**
   * Processes a geometry to determine the best interior point for all component polygons.
   *
   * @param geom
   *   the geometry to process
   */
  private def process(geom: Geometry): Unit = {
    if (geom.isEmpty) return
    if (geom.isInstanceOf[Polygon]) processPolygon(geom.asInstanceOf[Polygon])
    else if (geom.isInstanceOf[GeometryCollection]) {
      val gc = geom.asInstanceOf[GeometryCollection]
      var i  = 0
      while (i < gc.getNumGeometries) {
        process(gc.getGeometryN(i))
        i += 1
      }
    }
  }

  /**
   * Computes an interior point of a component Polygon and updates current best interior point if
   * appropriate.
   *
   * @param polygon
   *   the polygon to process
   */
  private def processPolygon(polygon: Polygon): Unit = {
    val intPtPoly = new InteriorPointArea.InteriorPointPolygon(polygon)
    intPtPoly.process()
    val width     = intPtPoly.getWidth
    if (width > maxWidth) {
      maxWidth = width
      interiorPoint = intPtPoly.getInteriorPoint
    }
  }
}
