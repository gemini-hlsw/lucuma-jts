// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.operation.overlayng

import org.locationtech.jts.algorithm.Orientation
import org.locationtech.jts.algorithm.locate.IndexedPointInAreaLocator
import org.locationtech.jts.algorithm.locate.PointOnGeometryLocator
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateArrays
import org.locationtech.jts.geom.CoordinateList
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.LinearRing
import org.locationtech.jts.geom.Location
import org.locationtech.jts.geom.Polygon
import org.locationtech.jts.geom.TopologyException

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

class OverlayEdgeRing(var startEdge: OverlayEdge, val geometryFactory: GeometryFactory) {
  private var ringPts                         = computeRingPts(startEdge)
  computeRing(ringPts, geometryFactory)
  private var ring: LinearRing                = null
  private var isHole0                         = false
  private var locator: PointOnGeometryLocator = null
  private var shell0: OverlayEdgeRing         = null
  private val holes                           =
    new util.ArrayList[OverlayEdgeRing] // a list of EdgeRings which are holes in this EdgeRing

  def getRing: LinearRing = ring

  /**
   * Tests whether this ring is a hole.
   *
   * @return
   *   <code>true</code> if this ring is a hole
   */
  def isHole: Boolean = isHole0

  /**
   * Sets the containing shell ring of a ring that has been determined to be a hole.
   *
   * @param shell
   *   the shell ring
   */
  def setShell(shell: OverlayEdgeRing): Unit = {
    this.shell0 = shell
    if (shell != null) shell0.addHole(this)
  }

  /**
   * Tests whether this ring has a shell assigned to it.
   *
   * @return
   *   true if the ring has a shell
   */
  def hasShell: Boolean = shell0 != null

  /**
   * Gets the shell for this ring. The shell is the ring itself if it is not a hole, otherwise its
   * parent shell.
   *
   * @return
   *   the shell for this ring
   */
  def getShell: OverlayEdgeRing = {
    if (isHole0) return shell0
    this
  }

  def addHole(ring: OverlayEdgeRing): Unit =
    holes.add(ring)

  private def computeRingPts(start: OverlayEdge) = {
    var edge = start
    val pts  = new CoordinateList
    while ({
      if (edge.getEdgeRing eq this)
        throw new TopologyException(
          "Edge visited twice during ring-building at " + edge.getCoordinate,
          edge.getCoordinate
        )
      // edges.add(de);
      // Debug.println(de);
      // Debug.println(de.getEdge());
      // only valid for polygonal output
      // Assert.isTrue(edge.getLabel().isBoundaryEither());
      edge.addCoordinates(pts)
      edge.setEdgeRing(this)
      if (edge.nextResult == null) throw new TopologyException("Found null edge in ring", edge.dest)
      edge = edge.nextResult
      edge != start
    }) ()
    pts.closeRing()
    pts.toCoordinateArray
  }

  private def computeRing(ringPts: Array[Coordinate], geometryFactory: GeometryFactory): Unit = {
    if (ring != null) return // don't compute more than once
    ring = geometryFactory.createLinearRing(ringPts)
    isHole0 = Orientation.isCCW(ring.getCoordinates)
  }

  /**
   * Computes the list of coordinates which are contained in this ring. The coordinates are computed
   * once only and cached.
   *
   * @return
   *   an array of the {@link Coordinate}s in this ring
   */
  private def getCoordinates = ringPts

  /**
   * Finds the innermost enclosing shell0 OverlayEdgeRing containing this OverlayEdgeRing, if any.
   * The innermost enclosing ring is the <i>smallest</i> enclosing ring. The algorithm used depends
   * on the fact that: <br> ring A contains ring B iff envelope(ring A) contains envelope(ring B)
   * <br> This routine is only safe to use if the chosen point of the hole is known to be properly
   * contained in a shell (which is guaranteed to be the case if the hole does not touch its shell)
   * <p> To improve performance of this function the caller should make the passed shellList as
   * small as possible (e.g. by using a spatial index filter beforehand).
   *
   * @return
   *   containing EdgeRing, if there is one or null if no containing EdgeRing is found
   */
  def findEdgeRingContaining(erList: util.List[OverlayEdgeRing]): OverlayEdgeRing = {
    val testRing                 = this.getRing
    val testEnv                  = testRing.getEnvelopeInternal
    var testPt                   = testRing.getCoordinateN(0)
    var minRing: OverlayEdgeRing = null
    var minRingEnv: Envelope     = null
    for (tryEdgeRing <- erList.asScala) {
      val tryRing     = tryEdgeRing.getRing
      val tryShellEnv = tryRing.getEnvelopeInternal
      // the hole envelope cannot equal the shell envelope
      // (also guards against testing rings against themselves)
      if (tryShellEnv != testEnv) {
        // hole must be contained in shell
        if (tryShellEnv.contains(testEnv)) {
          testPt = CoordinateArrays.ptNotInList(testRing.getCoordinates, tryEdgeRing.getCoordinates)
          val isContained = tryEdgeRing.isInRing(testPt)
          // check if the new containing ring is smaller than the current minimum ring
          if (isContained) if (minRing == null || minRingEnv.contains(tryShellEnv)) {
            minRing = tryEdgeRing
            minRingEnv = minRing.getRing.getEnvelopeInternal
          }
        }
      }
    }
    minRing
  }

  private def getLocator: PointOnGeometryLocator = {
    if (locator == null) locator = new IndexedPointInAreaLocator(getRing)
    locator
  }

  def isInRing(pt: Coordinate): Boolean =
    /**
     * Use an indexed point-in-polygon for performance
     */
    Location.EXTERIOR != getLocator.locate(pt)
    // return PointLocation.isInRing(pt, getCoordinates());

  def getCoordinate: Coordinate = ringPts(0)

  /**
   * Computes the {@link Polygon} formed by this ring and any contained holes.
   *
   * @return
   *   the {@link Polygon} formed by this ring and its holes.
   */
  def toPolygon(factory: GeometryFactory): Polygon = {
    var holeLR: Array[LinearRing] = null
    if (holes != null) {
      holeLR = new Array[LinearRing](holes.size)
      for (i <- 0 until holes.size)
        holeLR(i) = holes.get(i).getRing.asInstanceOf[LinearRing]
    }
    val poly                      = factory.createPolygon(ring, holeLR)
    poly
  }

  def getEdge: OverlayEdge = startEdge
}
