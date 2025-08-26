// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
package org.locationtech.jts.algorithm.distance

import org.locationtech.jts.geom.Coordinate
//import org.locationtech.jts.io.WKTWriter

/**
 * Contains a pair of points and the distance between them. Provides methods to update with a new
 * point pair with either maximum or minimum distance.
 */
class PointPairDistance() {
  private val pt       = Array(new Coordinate, new Coordinate)
  private var distance = Double.NaN
  private var isNull   = true

  def initialize(): Unit = isNull = true

  def initialize(p0: Coordinate, p1: Coordinate): Unit = {
    pt(0).setCoordinate(p0)
    pt(1).setCoordinate(p1)
    distance = p0.distance(p1)
    isNull = false
  }

  /**
   * Initializes the points, avoiding recomputing the distance.
   *
   * @param p0
   * @param p1
   * @param distance
   *   the distance between p0 and p1
   */
  private def initialize(p0: Coordinate, p1: Coordinate, distance: Double): Unit = {
    pt(0).setCoordinate(p0)
    pt(1).setCoordinate(p1)
    this.distance = distance
    isNull = false
  }

  def getDistance: Double = distance

  def getCoordinates: Array[Coordinate] = pt

  def getCoordinate(i: Int): Coordinate = pt(i)

  def setMaximum(ptDist: PointPairDistance): Unit = setMaximum(ptDist.pt(0), ptDist.pt(1))

  def setMaximum(p0: Coordinate, p1: Coordinate): Unit = {
    if (isNull) {
      initialize(p0, p1)
      return
    }
    val dist = p0.distance(p1)
    if (dist > distance) initialize(p0, p1, dist)
  }

  def setMinimum(ptDist: PointPairDistance): Unit = setMinimum(ptDist.pt(0), ptDist.pt(1))

  def setMinimum(p0: Coordinate, p1: Coordinate): Unit = {
    if (isNull) {
      initialize(p0, p1)
      return
    }
    val dist = p0.distance(p1)
    if (dist < distance) initialize(p0, p1, dist)
  }

//  override def toString: String = WKTWriter.toLineString(pt(0), pt(1))
  override def toString: String = (pt(0), pt(1)).toString()
}
