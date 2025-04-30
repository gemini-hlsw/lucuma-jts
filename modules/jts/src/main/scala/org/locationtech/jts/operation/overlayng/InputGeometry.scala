// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.operation.overlayng

import org.locationtech.jts.algorithm.locate.IndexedPointInAreaLocator
import org.locationtech.jts.algorithm.locate.PointOnGeometryLocator
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.Location

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
 * Manages the input geometries for an overlay operation. The second geometry is allowed to be null,
 * to support for instance precision reduction.
 *
 * @author
 *   Martin Davis
 */
class InputGeometry(val geomA: Geometry, val geomB: Geometry) {
  private var geom                               = Array[Geometry](geomA, geomB)
  private var ptLocatorA: PointOnGeometryLocator = null
  private var ptLocatorB: PointOnGeometryLocator = null
  private val isCollapsed                        = new Array[Boolean](2)

  def isSingle: Boolean = geom(1) == null

  def getDimension(index: Int): Int = {
    if (geom(index) == null) return -1
    geom(index).getDimension
  }

  def getGeometry(geomIndex: Int): Geometry = geom(geomIndex)

  def getEnvelope(geomIndex: Int): Envelope = geom(geomIndex).getEnvelopeInternal

  def isEmpty(geomIndex: Int): Boolean = geom(geomIndex).isEmpty

  def isArea(geomIndex: Int): Boolean = geom(geomIndex) != null && geom(geomIndex).getDimension == 2

  /**
   * Gets the index of an input which is an area, if one exists. Otherwise returns -1. If both
   * inputs are areas, returns the index of the first one (0).
   *
   * @return
   *   the index of an area input, or -1
   */
  def getAreaIndex: Int = {
    if (getDimension(0) == 2) return 0
    if (getDimension(1) == 2) return 1
    -1
  }

  def isLine(geomIndex: Int): Boolean = getDimension(geomIndex) == 1

  def isAllPoints: Boolean = getDimension(0) == 0 && geom(1) != null && getDimension(1) == 0

  def hasPoints: Boolean = getDimension(0) == 0 || getDimension(1) == 0

  /**
   * Tests if an input geometry has edges. This indicates that topology needs to be computed for
   * them.
   *
   * @param geomIndex
   * @return
   *   true if the input geometry has edges
   */
  def hasEdges(geomIndex: Int): Boolean =
    geom(geomIndex) != null && geom(geomIndex).getDimension > 0

  /**
   * Determines the location within an area geometry. This allows disconnected edges to be fully
   * located.
   *
   * @param geomIndex
   *   the index of the geometry
   * @param pt
   *   the coordinate to locate
   * @return
   *   the location of the coordinate
   * @see
   *   Location
   */
  def locatePointInArea(geomIndex: Int, pt: Coordinate): Int = { // Assert: only called if dimension(geomIndex) = 2
    if (isCollapsed(geomIndex)) return Location.EXTERIOR
    // return ptLocator.locate(pt, geom[geomIndex]);
    // *
    // this check is required because IndexedPointInAreaLocator can't handle empty polygons
    if (getGeometry(geomIndex).isEmpty || isCollapsed(geomIndex)) return Location.EXTERIOR
    val ptLocator = getLocator(geomIndex)
    ptLocator.locate(pt)
    // */
  }

  private def getLocator(geomIndex: Int) = if (geomIndex == 0) {
    if (ptLocatorA == null) ptLocatorA = new IndexedPointInAreaLocator(getGeometry(geomIndex))
    ptLocatorA
  } else {
    if (ptLocatorB == null) ptLocatorB = new IndexedPointInAreaLocator(getGeometry(geomIndex))
    ptLocatorB
  }

  def setCollapsed(geomIndex: Int, isGeomCollapsed: Boolean): Unit =
    isCollapsed(geomIndex) = isGeomCollapsed
}
