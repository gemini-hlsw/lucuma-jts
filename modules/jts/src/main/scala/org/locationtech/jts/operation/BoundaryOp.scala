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
package org.locationtech.jts.operation

import java.util

import org.locationtech.jts.algorithm.BoundaryNodeRule
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateArrays
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.MultiLineString
import org.locationtech.jts.geom.Point

import scala.collection.mutable.TreeMap

// /**
//  * Computes the boundary of a {link Geometry}.
//  * Allows specifying the {link BoundaryNodeRule} to be used.
//  * This operation will always return a {link Geometry} of the appropriate
//  * dimension for the boundary (even if the input geometry is empty).
//  * The boundary of zero-dimensional geometries (Points) is
//  * always the empty {link GeometryCollection}.
//  *
//  * @author Martin Davis
//  * @version 1.7
//  */
object BoundaryOp {

  /**
   * Computes a geometry representing the boundary of a geometry.
   *
   * @param g
   *   the input geometry return the computed boundary
   */
  def getBoundary(g: Geometry): Geometry = {
    val bop = new BoundaryOp(g)
    bop.getBoundary
  }

  // /**
  //  * Computes a geometry representing the boundary of a geometry,
  //  * using an explicit {link BoundaryNodeRule}.
  //  *
  //  * @param g      the input geometry
  //  * @param bnRule the Boundary Node Rule to use
  //  * return the computed boundary
  //  */
  def getBoundary(g: Geometry, bnRule: BoundaryNodeRule): Geometry = {
    val bop = new BoundaryOp(g, bnRule)
    bop.getBoundary
  }
}

class BoundaryOp(var geom: Geometry, var bnRule: BoundaryNodeRule) {

  /**
   * Creates a new instance for the given geometry.
   *
   * @param geom
   *   the input geometry
   * @param bnRule
   *   the Boundary Node Rule to use
   */
  private val geomFact = geom.getFactory

  /**
   * Creates a new instance for the given geometry.
   *
   * @param geom
   *   the input geometry
   */
  def this(geom: Geometry) =
    this(geom, BoundaryNodeRule.MOD2_BOUNDARY_RULE)

  /**
   * Gets the computed boundary.
   *
   * return the boundary geometry
   */
  def getBoundary: Geometry =
    geom match {
      case string: LineString  => boundaryLineString(string)
      case ml: MultiLineString => boundaryMultiLineString(ml)
      case _                   => geom.getBoundary
    }

  private def getEmptyMultiPoint = geomFact.createMultiPoint

  private def boundaryMultiLineString(mLine: MultiLineString): Geometry = {
    if (geom.isEmpty) return getEmptyMultiPoint
    val bdyPts = computeBoundaryCoordinates(mLine)
    // return Point or MultiPoint
    if (bdyPts.length == 1) return geomFact.createPoint(bdyPts(0))
    // this handles 0 points case as well
    geomFact.createMultiPointFromCoords(bdyPts)
  }

  private var endpointMap: TreeMap[Coordinate, Counter] = null

  private def computeBoundaryCoordinates(mLine: MultiLineString): Array[Coordinate] = {
    val bdyPts = new util.ArrayList[Coordinate]
    endpointMap = TreeMap.empty[Coordinate, Counter]
    var i      = 0
    while (i < mLine.getNumGeometries) {
      val line = mLine.getGeometryN(i).asInstanceOf[LineString]
      if (line.getNumPoints != 0) {
        addEndpoint(line.getCoordinateN(0))
        addEndpoint(line.getCoordinateN(line.getNumPoints - 1))
        i += 1
      }
    }
    endpointMap.foreach { case (k, v) =>
      val valence = v.count
      if (bnRule.isInBoundary(valence)) bdyPts.add(k)
    }

    CoordinateArrays.toCoordinateArray(bdyPts)
  }

  private def addEndpoint(pt: Coordinate): Unit = {
    var counter = endpointMap.get(pt).orNull
    if (counter == null) {
      counter = new Counter
      endpointMap.put(pt, counter)
    }
    counter.count += 1
  }

  private def boundaryLineString(line: LineString): Geometry = {
    if (geom.isEmpty) return getEmptyMultiPoint
    if (line.isClosed) { // check whether endpoints of valence 2 are on the boundary or not
      val closedEndpointOnBoundary = bnRule.isInBoundary(2)
      if (closedEndpointOnBoundary) return line.getStartPoint
      else return geomFact.createMultiPoint
    }
    geomFact.createMultiPoint(Array[Point](line.getStartPoint, line.getEndPoint))
  }
}

/**
 * Stores an integer count, for use as a Map entry.
 *
 * @author
 *   Martin Davis
 * @version 1.7
 */
class Counter {

  /**
   * The value of the count
   */
  private[operation] var count = 0
}
