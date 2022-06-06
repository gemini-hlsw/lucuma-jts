// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2016 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */ /*
 * Copyright (c) 2016 Martin Davis.
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

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.PrecisionModel

import java.util

/**
 * A dynamic list of the vertices in a constructed offset curve. Automatically removes adjacent
 * vertices which are closer than a given tolerance.
 *
 * @author
 *   Martin Davis
 */
object OffsetSegmentString {
  private val COORDINATE_ARRAY_TYPE = new Array[Coordinate](0)
}

class OffsetSegmentString() {
  private val ptList                         = new util.ArrayList[Coordinate]
  private var precisionModel: PrecisionModel = null

  /**
   * The distance below which two adjacent points on the curve are considered to be coincident. This
   * is chosen to be a small fraction of the offset distance.
   */
  private var minimimVertexDistance = 0.0

  def setPrecisionModel(precisionModel: PrecisionModel): Unit = this.precisionModel = precisionModel

  def setMinimumVertexDistance(minimimVertexDistance: Double): Unit = this.minimimVertexDistance =
    minimimVertexDistance

  def addPt(pt: Coordinate): Unit = {
    val bufPt = new Coordinate(pt)
    precisionModel.makePrecise(bufPt)
    // don't add duplicate (or near-duplicate) points
    if (isRedundant(bufPt)) return
    ptList.add(bufPt)
    ()
    // System.out.println(bufPt);
  }

  def addPts(pt: Array[Coordinate], isForward: Boolean): Unit = if (isForward) {
    var i = 0
    while (i < pt.length) {
      addPt(pt(i))
      i += 1
    }
  } else {
    var i = pt.length - 1
    while (i >= 0) {
      addPt(pt(i))
      i -= 1
    }
  }

  /**
   * Tests whether the given point is redundant relative to the previous point in the list (up to
   * tolerance).
   *
   * @param pt
   *   return true if the point is redundant
   */
  private def isRedundant(pt: Coordinate): Boolean = {
    if (ptList.size < 1) return false
    val lastPt = ptList.get(ptList.size - 1)
    val ptDist = pt.distance(lastPt)
    if (ptDist < minimimVertexDistance) return true
    false
  }

  def closeRing(): Unit = {
    if (ptList.size < 1) return
    val startPt = new Coordinate(ptList.get(0))
    val lastPt  = ptList.get(ptList.size - 1)
    if (startPt == lastPt) return
    ptList.add(startPt)
    ()
  }

  def reverse(): Unit = {}

  def getCoordinates: Array[Coordinate] = {
    /*
        // check that points are a ring - add the startpoint again if they are not
      if (ptList.size() > 1) {
         Coordinate start  = (Coordinate) ptList.get(0);
         Coordinate end    = (Coordinate) ptList.get(ptList.size() - 1);
         if (! start.equals(end) ) addPt(start);
       }
     */
    val coord = ptList.toArray(OffsetSegmentString.COORDINATE_ARRAY_TYPE)
    coord
  }

  override def toString: String = {
    val fact = new GeometryFactory
    val line = fact.createLineString(getCoordinates)
    line.toString
  }
}
