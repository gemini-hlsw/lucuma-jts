// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.operation.overlayng

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateSequence
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.Point
import org.locationtech.jts.geom.PrecisionModel

import java.util
import java.util.Map.Entry
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
 * Performs an overlay operation on inputs which are both point geometries. <p> Semantics are: <ul>
 * <li>Points are rounded to the precision model if provided <li>Points with identical XY values are
 * merged to a single point <li>Extended ordinate values are preserved in the output, apart from
 * merging <li>An empty result is returned as <code>POINT EMPTY</code> </ul>
 *
 * @author
 *   Martin Davis
 */
object OverlayPoints {

  /**
   * Performs an overlay operation on inputs which are both point geometries.
   *
   * @param geom0
   *   the first geometry argument
   * @param geom1
   *   the second geometry argument
   * @param opCode
   *   the code for the desired overlay operation
   * @param pm
   *   the precision model to use
   * @return
   *   the result of the overlay operation
   */
  def overlay(opCode: Int, geom0: Geometry, geom1: Geometry, pm: PrecisionModel): Geometry = {
    val overlay = new OverlayPoints(opCode, geom0, geom1, pm)
    overlay.getResult
  }

  /**
   * Round the key point if precision model is fixed. Note: return value is only copied if rounding
   * is performed.
   *
   * @param pt
   * @return
   */
  private[overlayng] def roundCoord(pt: Point, pm: PrecisionModel): Coordinate = {
    val p  = pt.getCoordinate
    if (OverlayUtil.isFloating(pm)) return p
    val p2 = p.copy
    pm.makePrecise(p2)
    p2
  }
}

/**
 * Creates an instance of an overlay operation on inputs which are both point geometries.
 *
 * @param geom0
 *   the first geometry argument
 * @param geom1
 *   the second geometry argument
 * @param opCode
 *   the code for the desired overlay operation
 * @param pm
 *   the precision model to use
 */
class OverlayPoints(
  var opCode: Int,
  var geom0:  Geometry,
  var geom1:  Geometry,
  var pm:     PrecisionModel
) {

  private var geometryFactory: GeometryFactory  = geom0.getFactory
  private var resultList: util.ArrayList[Point] = null

  /**
   * Gets the result of the overlay.
   *
   * @return
   *   the overlay result
   */
  def getResult: Geometry = {
    val map0: util.Map[Coordinate, Point] = buildPointMap(geom0)
    val map1: util.Map[Coordinate, Point] = buildPointMap(geom1)
    resultList = new util.ArrayList[Point]
    opCode match {
      case OverlayNG.INTERSECTION =>
        computeIntersection(map0, map1, resultList)

      case OverlayNG.UNION =>
        computeUnion(map0, map1, resultList)

      case OverlayNG.DIFFERENCE =>
        computeDifference(map0, map1, resultList)

      case OverlayNG.SYMDIFFERENCE =>
        computeDifference(map0, map1, resultList)
        computeDifference(map1, map0, resultList)

    }
    if (resultList.isEmpty) {
      return OverlayUtil.createEmptyResult(0, geometryFactory)
    }
    return geometryFactory.buildGeometry(resultList.asScala.map { case p: Geometry =>
      p: Geometry
    }.asJavaCollection)
  }

  private def computeIntersection(
    map0:       util.Map[Coordinate, Point],
    map1:       util.Map[Coordinate, Point],
    resultList: util.ArrayList[Point]
  ): Unit =
    for (entry <- map0.entrySet.asScala)
      if (map1.containsKey(entry.getKey)) {
        resultList.add(copyPoint(entry.getValue))
      }

  private def computeDifference(
    map0:       util.Map[Coordinate, Point],
    map1:       util.Map[Coordinate, Point],
    resultList: util.ArrayList[Point]
  ): Unit =
    for (entry <- map0.entrySet.asScala)
      if (!map1.containsKey(entry.getKey)) {
        resultList.add(copyPoint(entry.getValue))
      }

  private def computeUnion(
    map0:       util.Map[Coordinate, Point],
    map1:       util.Map[Coordinate, Point],
    resultList: util.ArrayList[Point]
  ): Unit = { // copy all A points
    for (p     <- map0.values.asScala)
      resultList.add(copyPoint(p))
    for (entry <- map1.entrySet.asScala)
      if (!map0.containsKey(entry.getKey)) {
        resultList.add(copyPoint(entry.getValue))
      }
  }

  private def copyPoint(pt: Point): Point = { // if pm is floating, the point coordinate is not changed
    if (OverlayUtil.isFloating(pm)) {
      return pt.copy.asInstanceOf[Point]
    }
    // pm is fixed.  Round off X&Y ordinates, copy other ordinates unchanged
    val seq: CoordinateSequence  = pt.getCoordinateSequence
    val seq2: CoordinateSequence = seq.copy
    seq2.setOrdinate(0, CoordinateSequence.X, pm.makePrecise(seq.getX(0)))
    seq2.setOrdinate(0, CoordinateSequence.Y, pm.makePrecise(seq.getY(0)))
    return geometryFactory.createPoint(seq2)
  }

  private def buildPointMap(geom: Geometry): util.HashMap[Coordinate, Point] = {
    val map: util.HashMap[Coordinate, Point] = new util.HashMap[Coordinate, Point]
    for (i <- 0 until geom.getNumGeometries) {
      val elt: Geometry = geom.getGeometryN(i)
      if (!elt.isInstanceOf[Point]) {
        throw new IllegalArgumentException("Non-point geometry input to point overlay")
      }
      // don't add empty points
      if (!elt.isEmpty) {
        val pt: Point     = elt.asInstanceOf[Point]
        val p: Coordinate = OverlayPoints.roundCoord(pt, pm)

        /**
         * Only add first occurrence of a point. This provides the merging semantics of overlay
         */
        if (!map.containsKey(p)) {
          map.put(p, pt)
        }
      }
    }
    return map
  }
}
