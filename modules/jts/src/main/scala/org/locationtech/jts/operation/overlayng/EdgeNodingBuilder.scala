// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.operation.overlayng

import org.locationtech.jts.algorithm.LineIntersector
import org.locationtech.jts.algorithm.Orientation
import org.locationtech.jts.algorithm.RobustLineIntersector
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateArrays
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryCollection
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.LinearRing
import org.locationtech.jts.geom.MultiLineString
import org.locationtech.jts.geom.MultiPolygon
import org.locationtech.jts.geom.Polygon
import org.locationtech.jts.geom.PrecisionModel
import org.locationtech.jts.noding.IntersectionAdder
import org.locationtech.jts.noding.MCIndexNoder
import org.locationtech.jts.noding.NodedSegmentString
import org.locationtech.jts.noding.Noder
import org.locationtech.jts.noding.SegmentString
import org.locationtech.jts.noding.ValidatingNoder
import org.locationtech.jts.noding.snapround.SnapRoundingNoder

import java.util
import scala.jdk.CollectionConverters._

/*
 * Copyright (c) 2019 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v20.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */

/**
 * Builds a set of noded, unique, labelled Edges from the edges of the two input geometries. <p> It
 * performs the following steps: <ul> <li>Extracts input edges, and attaches topological information
 * <li>if clipping is enabled, handles clipping or limiting input geometry <li>chooses a {@link
 * Noder} based on provided precision model, unless a custom one is supplied <li>calls the chosen
 * Noder, with precision model <li>removes any fully collapsed noded edges <li>builds {@link Edge}s
 * and merges them </ul>
 *
 * @author
 *   mdavis
 */
object EdgeNodingBuilder {

  /**
   * Limiting is skipped for Lines with few vertices, to avoid additional copying.
   */
  private val MIN_LIMIT_PTS = 20

  /**
   * Indicates whether floating precision noder output is validated.
   */
  private val IS_NODING_VALIDATED = true

  private def createFixedPrecisionNoder(pm: PrecisionModel): Noder[SegmentString] = { // Noder noder = new MCIndexSnapRounder(pm);
    // Noder noder = new SimpleSnapRounder(pm);
    val noder = new SnapRoundingNoder(pm)
    noder.asInstanceOf[Noder[SegmentString]]
  }

  private def createFloatingPrecisionNoder(doValidation: Boolean): Noder[SegmentString] = {
    val mcNoder                     = new MCIndexNoder
    val li                          = new RobustLineIntersector
    mcNoder.setSegmentIntersector(new IntersectionAdder(li))
    var noder: Noder[SegmentString] = mcNoder
    if (doValidation)
      noder = new ValidatingNoder(mcNoder)
    noder
  }

  /**
   * Removes any repeated points from a linear component. This is required so that noding can be
   * computed correctly.
   *
   * @param line
   *   the line to process
   * @return
   *   the points of the line with repeated points removed
   */
  private def removeRepeatedPoints(line: LineString) = {
    val pts = line.getCoordinates
    CoordinateArrays.removeRepeatedPoints(pts)
  }

  private def computeDepthDelta(ring: LinearRing, isHole: Boolean) = {

    /**
     * Compute the orientation of the ring, to allow assigning side interior/exterior labels
     * correctly. JTS canonical orientation is that shells are CW, holes are CCW.
     *
     * It is important to compute orientation on the original ring, since topology collapse can make
     * the orientation computation give the wrong answer.
     */
    val isCCW = Orientation.isCCW(ring.getCoordinateSequence)

    /**
     * Compute whether ring is in canonical orientation or not. Canonical orientation for the
     * overlay process is Shells : CW, Holes: CCW
     */
    var isOriented = true
    if (!isHole) isOriented = !isCCW
    else isOriented = isCCW

    /**
     * Depth delta can now be computed. Canonical depth delta is 1 (Exterior on L, Interior on R).
     * It is flipped to -1 if the ring is oppositely oriented.
     */
    val depthDelta =
      if (isOriented) 1
      else -1
    depthDelta
  }
}

class EdgeNodingBuilder(var pm: PrecisionModel, var customNoder: Noder[SegmentString]) {

  /**
   * Creates a new builder, with an optional custom noder. If the noder is not provided, a suitable
   * one will be used based on the supplied precision model.
   *
   * @param pm
   *   the precision model to use
   * @param noder
   *   an optional custom noder to use (may be null)
   */
  private[overlayng] val inputEdges: util.List[SegmentString] =
    new util.ArrayList[SegmentString]
  private var clipEnv: Envelope                               = null
  private var clipper: RingClipper                            = null
  private var limiter: LineLimiter                            = null
  private val hasEdges: Array[Boolean]                        = new Array[Boolean](2)

  /**
   * Gets a noder appropriate for the precision model supplied. This is one of: <ul> <li>Fixed
   * precision: a snap-rounding noder (which should be fully robust) <li>Floating precision: a
   * conventional nodel (which may be non-robust). In this case, a validation step is applied to the
   * output from the noder. </ul>
   *
   * @return
   */
  private def getNoder: Noder[SegmentString] = {
    if (customNoder != null) {
      return customNoder
    }
    if (OverlayUtil.isFloating(pm)) {
      return EdgeNodingBuilder.createFloatingPrecisionNoder(EdgeNodingBuilder.IS_NODING_VALIDATED)
    }
    return EdgeNodingBuilder.createFixedPrecisionNoder(pm)
  }

  def setClipEnvelope(clipEnv: Envelope): Unit = {
    this.clipEnv = clipEnv
    clipper = new RingClipper(clipEnv)
    limiter = new LineLimiter(clipEnv)
  }

  /**
   * Reports whether there are noded edges for the given input geometry. If there are none, this
   * indicates that either the geometry was empty, or has completely collapsed (because it is
   * smaller than the noding precision).
   *
   * @param geomIndex
   *   index of input geometry
   * @return
   *   true if there are edges for the geometry
   */
  def hasEdgesFor(geomIndex: Int): Boolean =
    return hasEdges(geomIndex)

  /**
   * Creates a set of labelled {Edge}s. representing the fully noded edges of the input geometries.
   * Coincident edges (from the same or both geometries) are merged along with their labels into a
   * single unique, fully labelled edge.
   *
   * @param geom0
   *   the first geometry
   * @param geom1
   *   the second geometry
   * @return
   *   the noded, merged, labelled edges
   */
  def build(geom0: Geometry, geom1: Geometry): util.List[Edge] = {
    add(geom0, 0)
    add(geom1, 1)
    val nodedEdges: util.List[Edge] = node(inputEdges)

    /**
     * Merge the noded edges to eliminate duplicates. Labels are combined.
     */
    val mergedEdges: util.List[Edge] = EdgeMerger.merge(nodedEdges)
    return mergedEdges
  }

  /**
   * Nodes a set of segment strings and creates {@link Edge}s from the result. The input segment
   * strings each carry a {@link EdgeSourceInfo} object, which is used to provide source topology
   * info to the constructed Edges (and is then discarded).
   *
   * @param segStrings
   * @return
   */
  private def node(segStrings: util.List[SegmentString]): util.List[Edge] = {
    val noder: Noder[SegmentString]                                                   = getNoder
    noder.computeNodes(segStrings)
    @SuppressWarnings(Array("unchecked")) val nodedSS: util.Collection[SegmentString] =
      noder.getNodedSubstrings
    // scanForEdges(nodedSS);
    val edges: util.List[Edge]                                                        = createEdges(nodedSS)
    return edges
  }

  private def createEdges(segStrings: util.Collection[SegmentString]): util.List[Edge] = {
    val edges: util.List[Edge] = new util.ArrayList[Edge]
    for (ss <- segStrings.asScala) {
      val pts: Array[Coordinate] = ss.getCoordinates
      // don't create edges from collapsed lines
      if (!Edge.isCollapsed(pts)) {
        val info: EdgeSourceInfo = ss.getData.asInstanceOf[EdgeSourceInfo]

        /**
         * Record that a non-collapsed edge exists for the parent geometry
         */
        hasEdges(info.getIndex) = true
        edges.add(new Edge(ss.getCoordinates, info))
      }
    }
    return edges
  }

  private def add(g: Geometry, geomIndex: Int): Unit = {
    if (g == null || g.isEmpty) {
      return
    }
    if (isClippedCompletely(g.getEnvelopeInternal)) {
      return
    }
    if (g.isInstanceOf[Polygon]) {
      addPolygon(g.asInstanceOf[Polygon], geomIndex)
    } else { // LineString also handles LinearRings
      if (g.isInstanceOf[LineString]) {
        addLine(g.asInstanceOf[LineString], geomIndex)
      } else {
        if (g.isInstanceOf[MultiLineString]) {
          addCollection(g.asInstanceOf[MultiLineString], geomIndex)
        } else {
          if (g.isInstanceOf[MultiPolygon]) {
            addCollection(g.asInstanceOf[MultiPolygon], geomIndex)
          } else {
            if (g.isInstanceOf[GeometryCollection]) {
              addCollection(g.asInstanceOf[GeometryCollection], geomIndex)
            }
          }
        }
      }
    }
    // ignore Point geometries - they are handled elsewhere
  }

  private def addCollection(gc: GeometryCollection, geomIndex: Int): Unit =
    for (i <- 0 until gc.getNumGeometries) {
      val g: Geometry = gc.getGeometryN(i)
      add(g, geomIndex)
    }

  private def addPolygon(poly: Polygon, geomIndex: Int): Unit = {
    val shell: LinearRing = poly.getExteriorRing
    addPolygonRing(shell, false, geomIndex)
    for (i <- 0 until poly.getNumInteriorRing) {
      val hole: LinearRing = poly.getInteriorRingN(i)
      // Holes are topologically labelled opposite to the shell, since
      // the interior of the polygon lies on their opposite side
      // (on the left, if the hole is oriented CW)
      addPolygonRing(hole, true, geomIndex)
    }
  }

  /**
   * Adds a polygon ring to the graph. Empty rings are ignored.
   */
  private def addPolygonRing(ring: LinearRing, isHole: Boolean, index: Int): Unit = { // don't add empty rings
    if (ring.isEmpty) {
      return
    }
    if (isClippedCompletely(ring.getEnvelopeInternal)) {
      return
    }
    val pts: Array[Coordinate] = clip(ring)

    /**
     * Don't add edges that collapse to a point
     */
    if (pts.length < 2) {
      return
    }
    // if (pts.length < ring.getNumPoints()) System.out.println("Ring clipped: " + ring.getNumPoints() + " => " + pts.length);
    val depthDelta: Int      = EdgeNodingBuilder.computeDepthDelta(ring, isHole)
    val info: EdgeSourceInfo = new EdgeSourceInfo(index, depthDelta, isHole)
    addEdge(pts, info)
  }

  /**
   * Tests whether a geometry (represented by its envelope) lies completely outside the clip
   * extent(if any).
   *
   * @param env
   *   the geometry envelope
   * @return
   *   true if the geometry envelope is outside the clip extent.
   */
  private def isClippedCompletely(env: Envelope): Boolean = {
    if (clipEnv == null) {
      return false
    }
    return clipEnv.disjoint(env)
  }

  /**
   * If a clipper is present, clip the line to the clip extent. Otherwise, remove duplicate points
   * from the ring. <p> If clipping is enabled, then every ring MUST be clipped, to ensure that
   * holes are clipped to be inside the shell. This means it is not possible to skip clipping for
   * rings with few vertices.
   *
   * @param ring
   *   the line to clip
   * @return
   *   the points in the clipped line
   */
  private def clip(ring: LinearRing): Array[Coordinate] = {
    val pts: Array[Coordinate] = ring.getCoordinates
    val env: Envelope          = ring.getEnvelopeInternal

    /**
     * If no clipper or ring is completely contained then no need to clip. But repeated points must
     * be removed to ensure correct noding.
     */
    if (clipper == null || clipEnv.covers(env)) {
      return EdgeNodingBuilder.removeRepeatedPoints(ring)
    }
    return clipper.clip(pts)
  }

  /**
   * Adds a line geometry, limiting it if enabled, and otherwise removing repeated points.
   *
   * @param line
   *   the line to add
   * @param geomIndex
   *   the index of the parent geometry
   */
  private def addLine(line: LineString, geomIndex: Int): Unit = { // don't add empty lines
    if (line.isEmpty) {
      return
    }
    if (isClippedCompletely(line.getEnvelopeInternal)) {
      return
    }
    if (isToBeLimited(line)) {
      val sections: util.List[Array[Coordinate]] = limit(line)
      for (pts <- sections.asScala)
        addLine(pts, geomIndex)
    } else {
      val ptsNoRepeat: Array[Coordinate] = EdgeNodingBuilder.removeRepeatedPoints(line)
      addLine(ptsNoRepeat, geomIndex)
    }
  }

  private def addLine(pts: Array[Coordinate], geomIndex: Int): Unit = {
    if (pts.length < 2) {
      return
    }
    val info: EdgeSourceInfo = new EdgeSourceInfo(geomIndex)
    addEdge(pts, info)
  }

  private def addEdge(pts: Array[Coordinate], info: EdgeSourceInfo): Unit = {
    val ss: NodedSegmentString = new NodedSegmentString(pts, info)
    inputEdges.add(ss)
  }

  /**
   * Tests whether it is worth limiting a line. Lines that have few vertices or are covered by the
   * clip extent do not need to be limited.
   *
   * @param line
   *   line to test
   * @return
   *   true if the line should be limited
   */
  private def isToBeLimited(line: LineString): Boolean = {
    val pts: Array[Coordinate] = line.getCoordinates
    if (limiter == null || pts.length <= EdgeNodingBuilder.MIN_LIMIT_PTS) {
      return false
    }
    val env: Envelope          = line.getEnvelopeInternal

    /**
     * If line is completely contained then no need to limit
     */
    if (clipEnv.covers(env)) {
      return false
    }
    return true
  }

  /**
   * If limiter is provided, limit the line to the clip envelope.
   *
   * @param line
   *   the line to clip
   * @return
   *   the point sections in the clipped line
   */
  private def limit(line: LineString): util.List[Array[Coordinate]] = {
    val pts: Array[Coordinate] = line.getCoordinates
    return limiter.limit(pts)
  }
}
