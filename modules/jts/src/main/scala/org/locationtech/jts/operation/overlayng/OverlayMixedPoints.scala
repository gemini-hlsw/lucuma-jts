// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.operation.overlayng

import org.locationtech.jts.algorithm.locate.IndexedPointInAreaLocator
import org.locationtech.jts.algorithm.locate.PointOnGeometryLocator
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateList
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.Location
import org.locationtech.jts.geom.Point
import org.locationtech.jts.geom.Polygon
import org.locationtech.jts.geom.PrecisionModel
import org.locationtech.jts.util.Assert

import java.util
import scala.jdk.CollectionConverters._

/*
 * Copyright (c) 2020 Martin Davis.
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
 * Computes an overlay where one input is Point(s) and one is not. This class supports overlay being
 * used as an efficient way to find points within or outside a polygon. <p> Input semantics are:
 * <ul> <li>Duplicates are removed from Point output <li>Non-point output is rounded and noded using
 * the given precision model </ul> Output semantics are: <ul> <ii>An empty result is an empty atomic
 * geometry with dimension determined by the inputs and the operation, as per overlay semantics<li>
 * </ul> For efficiency the following optimizations are used: <ul> <li>Input points are not included
 * in the noding of the non-point input geometry (in particular, they do not participate in
 * snap-rounding if that is used). <li>If the non-point input geometry is not included in the output
 * it is not rounded and noded. This means that points are compared to the non-rounded geometry.
 * This will be apparent in the result. </ul>
 *
 * @author
 *   Martin Davis
 */
object OverlayMixedPoints {
  def overlay(opCode: Int, geom0: Geometry, geom1: Geometry, pm: PrecisionModel): Geometry = {
    val overlay = new OverlayMixedPoints(opCode, geom0, geom1, pm)
    overlay.getResult
  }

  private def extractCoordinates(points: Geometry, pm: PrecisionModel) = {
    val coords = new CoordinateList
    val n      = points.getNumGeometries
    for (i <- 0 until n) {
      val point = points.getGeometryN(i).asInstanceOf[Point]
      if (!point.isEmpty) {
        val coord = OverlayUtil.round(point, pm)
        coords.add(coord, true)
      }
    }
    coords.toCoordinateArray
  }

  private def extractPolygons(geom: Geometry) = {
    val list = new util.ArrayList[Polygon]
    for (i <- 0 until geom.getNumGeometries) {
      val poly = geom.getGeometryN(i).asInstanceOf[Polygon]
      if (!poly.isEmpty) list.add(poly)
    }
    list
  }

  private def extractLines(geom: Geometry) = {
    val list = new util.ArrayList[LineString]
    for (i <- 0 until geom.getNumGeometries) {
      val line = geom.getGeometryN(i).asInstanceOf[LineString]
      if (!line.isEmpty) list.add(line)
    }
    list
  }
}

class OverlayMixedPoints(
  val opCode: Int,
  val geom0:  Geometry,
  val geom1:  Geometry,
  val pm:     PrecisionModel
) {
  private var geometryFactory                 = geom0.getFactory
  private var geomPoint: Geometry             = null
  private var geomNonPointInput: Geometry     = null
  private var isPointRHS                      = false
  resultDim = OverlayUtil.resultDimension(opCode, geom0.getDimension, geom1.getDimension)
  // name the dimensional geometries
  if (geom0.getDimension == 0) {
    this.geomPoint = geom0
    this.geomNonPointInput = geom1
    this.isPointRHS = false
  } else {
    this.geomPoint = geom1
    this.geomNonPointInput = geom0
    this.isPointRHS = true
  }
  private var geomNonPoint: Geometry          = null
  private var geomNonPointDim                 = 0
  private var locator: PointOnGeometryLocator = null
  private var resultDim                       = 0

  def getResult: Geometry = { // reduce precision of non-point input, if required
    geomNonPoint = prepareNonPoint(geomNonPointInput)
    geomNonPointDim = geomNonPoint.getDimension
    locator = createLocator(geomNonPoint)
    val coords = OverlayMixedPoints.extractCoordinates(geomPoint, pm)
    opCode match {
      case OverlayNG.INTERSECTION  =>
        return computeIntersection(coords)
      case OverlayNG.UNION         =>
      case OverlayNG.SYMDIFFERENCE =>
        // UNION and SYMDIFFERENCE have same output
        return computeUnion(coords)
      case OverlayNG.DIFFERENCE    =>
        return computeDifference(coords)
    }
    Assert.shouldNeverReachHere("Unknown overlay op code")
    null
  }

  private def createLocator(geomNonPoint: Geometry) = if (geomNonPointDim == 2)
    new IndexedPointInAreaLocator(geomNonPoint)
  else new IndexedPointOnLineLocator(geomNonPoint)

  private def prepareNonPoint(geomInput: Geometry): Geometry = { // if non-point not in output no need to node it
    if (resultDim == 0) return geomInput
    // Node and round the non-point geometry for output
    val geomPrep = OverlayNG.union(geomNonPointInput, pm)
    geomPrep
  }

  private def computeIntersection(coords: Array[Coordinate]) = createPointResult(
    findPoints(true, coords)
  )

  private def computeUnion(coords: Array[Coordinate]) = {
    val resultPointList                       = findPoints(false, coords)
    var resultLineList: util.List[LineString] = null
    if (geomNonPointDim == 1) resultLineList = OverlayMixedPoints.extractLines(geomNonPoint)
    var resultPolyList: util.List[Polygon]    = null
    if (geomNonPointDim == 2) resultPolyList = OverlayMixedPoints.extractPolygons(geomNonPoint)
    OverlayUtil.createResultGeometry(resultPolyList,
                                     resultLineList,
                                     resultPointList,
                                     geometryFactory
    )
  }

  private def computeDifference(coords: Array[Coordinate]): Geometry = {
    if (isPointRHS) return copyNonPoint
    createPointResult(findPoints(false, coords))
  }

  private def createPointResult(points: util.List[Point]): Geometry = {
    if (points.size == 0) return geometryFactory.createEmpty(0)
    else if (points.size == 1) return points.get(0)
    val pointsArray = GeometryFactory.toPointArray(points)
    geometryFactory.createMultiPoint(pointsArray)
  }

  private def findPoints(isCovered: Boolean, coords: Array[Coordinate]) = {
    val resultCoords = new util.HashSet[Coordinate]
    // keep only points contained
    for (coord <- coords)
      if (hasLocation(isCovered, coord)) { // copy coordinate to avoid aliasing
        resultCoords.add(coord.copy)
      }
    createPoints(resultCoords)
  }

  private def createPoints(coords: util.Set[Coordinate]) = {
    val points = new util.ArrayList[Point]
    for (coord <- coords.asScala) {
      val point = geometryFactory.createPoint(coord)
      points.add(point)
    }
    points
  }

  private def hasLocation(isCovered: Boolean, coord: Coordinate): Boolean = {
    val isExterior = Location.EXTERIOR == locator.locate(coord)
    if (isCovered) return !isExterior
    isExterior
  }

  /**
   * Copy the non-point input geometry if not already done by precision reduction process.
   *
   * @return
   *   a copy of the non-point geometry
   */
  private def copyNonPoint: Geometry = {
    if (geomNonPointInput ne geomNonPoint) return geomNonPoint
    geomNonPoint.copy
  }
}
