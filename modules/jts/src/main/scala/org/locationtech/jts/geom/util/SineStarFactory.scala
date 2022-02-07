// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
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
package org.locationtech.jts.geom.util

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.Polygon
import org.locationtech.jts.util.GeometricShapeFactory

/**
 * Creates geometries which are shaped like multi-armed stars with each arm shaped like a sine wave.
 * These kinds of geometries are useful as a more complex geometry for testing algorithms.
 *
 * @author
 *   Martin Davis
 */
object SineStarFactory {

  /**
   * Creates a sine star with the given parameters.
   *
   * @param origin
   *   the origin point
   * @param size
   *   the size of the star
   * @param nPts
   *   the number of points in the star
   * @param nArms
   *   the number of arms to generate
   * @param armLengthRatio
   *   the arm length ratio return a sine star shape
   */
  def create(
    origin:         Coordinate,
    size:           Double,
    nPts:           Int,
    nArms:          Int,
    armLengthRatio: Double
  ): Polygon = {
    val gsf  = new SineStarFactory
    gsf.setCentre(origin)
    gsf.setSize(size)
    gsf.setNumPoints(nPts)
    gsf.setArmLengthRatio(armLengthRatio)
    gsf.setNumArms(nArms)
    val poly = gsf.createSineStar
    poly
  }
}

class SineStarFactory(geomFac: GeometryFactory)

/**
 * Creates a factory which will create sine stars using the default
 * {link GeometryFactory}.
 */ extends GeometricShapeFactory(geomFac) {
  protected var numArms        = 8
  protected var armLengthRatio = 0.5

  /**
   * Creates a factory which will create sine stars using the given {link GeometryFactory}.
   *
   * @param geomFact
   *   the factory to use
   */
  def this() = this(new GeometryFactory())
//    this()
//    super (geomFact)
//  }

  /**
   * Sets the number of arms in the star
   *
   * @param numArms
   *   the number of arms to generate
   */
  def setNumArms(numArms: Int): Unit = this.numArms = numArms

  /**
   * Sets the ratio of the length of each arm to the radius of the star. A smaller number makes the
   * arms shorter. Value should be between 0.0 and 1.0
   *
   * @param armLengthRatio
   *   the ratio determining the length of them arms.
   */
  def setArmLengthRatio(armLengthRatio: Double): Unit = this.armLengthRatio = armLengthRatio

  /**
   * Generates the geometry for the sine star
   *
   * return the geometry representing the sine star
   */
  def createSineStar: Polygon = {
    val env          = dim.getEnvelope
    val radius       = env.getWidth / 2.0
    var armRatio     = armLengthRatio
    if (armRatio < 0.0) armRatio = 0.0
    if (armRatio > 1.0) armRatio = 1.0
    val armMaxLen    = armRatio * radius
    val insideRadius = (1 - armRatio) * radius
    val centreX      = env.getMinX + radius
    val centreY      = env.getMinY + radius
    val pts          = new Array[Coordinate](nPts + 1)
    var iPt          = 0
    var i            = 0
    while (i < nPts) { // the fraction of the way through the current arm - in [0,1]
      val ptArcFrac   = (i / nPts.asInstanceOf[Double]) * numArms
      val armAngFrac  = ptArcFrac - Math.floor(ptArcFrac)
      // the angle for the current arm - in [0,2Pi]
      // (each arm is a complete sine wave cycle)
      val armAng      = 2 * Math.PI * armAngFrac
      // the current length of the arm
      val armLenFrac  = (Math.cos(armAng) + 1.0) / 2.0
      // the current radius of the curve (core + arm)
      val curveRadius = insideRadius + armMaxLen * armLenFrac
      // the current angle of the curve
      val ang         = i * (2 * Math.PI / nPts)
      val x           = curveRadius * Math.cos(ang) + centreX
      val y           = curveRadius * Math.sin(ang) + centreY
      pts {
        iPt += 1; iPt - 1
      } = coord(x, y)
      i += 1
    }
    pts(iPt) = new Coordinate(pts(0))
    val ring         = geomFact.createLinearRing(pts)
    val poly         = geomFact.createPolygon(ring)
    poly
  }
}
