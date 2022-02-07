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
package org.locationtech.jts.util

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.Polygon
import org.locationtech.jts.geom.PrecisionModel
import org.locationtech.jts.geom.util.AffineTransformation

/**
 * Computes various kinds of common geometric shapes. Provides various ways of specifying the
 * location and extent and rotations of the generated shapes, as well as number of line segments
 * used to form them. <p> <b>Example of usage:</b> <pre> GeometricShapeFactory gsf = new
 * GeometricShapeFactory(); gsf.setSize(100); gsf.setNumPoints(100); gsf.setBase(new Coordinate(100,
 * 100)); gsf.setRotation(0.5); Polygon rect = gsf.createRectangle(); </pre>
 *
 * @version 1.7
 */
object GeometricShapeFactory {

  protected class Dimensions {
    var base: Coordinate   = null
    var centre: Coordinate = null
    var width: Double      = .0
    var height: Double     = .0

    def setBase(base: Coordinate): Unit = this.base = base

    def getBase: Coordinate = base

    def setCentre(centre: Coordinate): Unit = this.centre = centre

    def getCentre: Coordinate = {
      if (centre == null) centre = new Coordinate(base.x + width / 2, base.y + height / 2)
      centre
    }

    def setSize(size: Double): Unit = {
      height = size
      width = size
    }

    def getMinSize: Double = Math.min(width, height)

    def setWidth(width: Double): Unit = this.width = width

    def getWidth: Double = width

    def getHeight: Double = height

    def setHeight(height: Double): Unit = this.height = height

    def setEnvelope(env: Envelope): Unit = {
      this.width = env.getWidth
      this.height = env.getHeight
      this.base = new Coordinate(env.getMinX, env.getMinY)
      this.centre = new Coordinate(env.centre)
    }

    def getEnvelope: Envelope = {
      if (base != null) return new Envelope(base.x, base.x + width, base.y, base.y + height)
      if (centre != null)
        return new Envelope(centre.x - width / 2,
                            centre.x + width / 2,
                            centre.y - height / 2,
                            centre.y + height / 2
        )
      new Envelope(0, width, 0, height)
    }
  }

}

class GeometricShapeFactory(var geomFact: GeometryFactory) {

// /**
//  * Create a shape factory which will create shapes using the given
//  * {link GeometryFactory}.
//  *
//  * @param geomFact the factory to use
//  */
  protected var precModel: PrecisionModel = geomFact.getPrecisionModel
  protected var dim                       = new GeometricShapeFactory.Dimensions
  protected var nPts                      = 100

  /**
   * Default is no rotation.
   */
  protected var rotationAngle = 0.0

  /**
   * Create a shape factory which will create shapes using the default {link GeometryFactory}.
   */
  def this() =
    this(new GeometryFactory)

  def setEnvelope(env: Envelope): Unit = dim.setEnvelope(env)

  /**
   * Sets the location of the shape by specifying the base coordinate (which in most cases is the
   * lower left point of the envelope containing the shape).
   *
   * @param base
   *   the base coordinate of the shape
   */
  def setBase(base: Coordinate): Unit = dim.setBase(base)

  /**
   * Sets the location of the shape by specifying the centre of the shape's bounding box
   *
   * @param centre
   *   the centre coordinate of the shape
   */
  def setCentre(centre: Coordinate): Unit = dim.setCentre(centre)

  // /**
  //  * Sets the total number of points in the created {link Geometry}.
  //  * The created geometry will have no more than this number of points,
  //  * unless more are needed to create a valid geometry.
  //  */
  def setNumPoints(nPts: Int): Unit = this.nPts = nPts

  /**
   * Sets the size of the extent of the shape in both x and y directions.
   *
   * @param size
   *   the size of the shape's extent
   */
  def setSize(size: Double): Unit = dim.setSize(size)

  /**
   * Sets the width of the shape.
   *
   * @param width
   *   the width of the shape
   */
  def setWidth(width: Double): Unit = dim.setWidth(width)

  /**
   * Sets the height of the shape.
   *
   * @param height
   *   the height of the shape
   */
  def setHeight(height: Double): Unit = dim.setHeight(height)

  /**
   * Sets the rotation angle to use for the shape. The rotation is applied relative to the centre of
   * the shape.
   *
   * @param radians
   *   the rotation angle in radians.
   */
  def setRotation(radians: Double): Unit = rotationAngle = radians

  protected def rotate(geom: Geometry): Geometry = {
    if (rotationAngle != 0.0) {
      val trans =
        AffineTransformation.rotationInstance(rotationAngle, dim.getCentre.x, dim.getCentre.y)
      geom.applyF(trans)
    }
    geom
  }

  // /**
  //  * Creates a rectangular {link Polygon}.
  //  *
  //  * return a rectangular Polygon
  //  *
  //  */
  def createRectangle: Polygon = {
    var i       = 0
    var ipt     = 0
    var nSide   = nPts / 4
    if (nSide < 1) nSide = 1
    val XsegLen = dim.getEnvelope.getWidth / nSide
    val YsegLen = dim.getEnvelope.getHeight / nSide
    val pts     = new Array[Coordinate](4 * nSide + 1)
    val env     = dim.getEnvelope
    // double maxx = env.getMinX() + nSide * XsegLen;
    // double maxy = env.getMinY() + nSide * XsegLen;
    i = 0
    while (i < nSide) {
      val x = env.getMinX + i * XsegLen
      val y = env.getMinY
      pts {
        ipt += 1; ipt - 1
      } = coord(x, y)
      i += 1
    }
    i = 0
    while (i < nSide) {
      val x = env.getMaxX
      val y = env.getMinY + i * YsegLen
      pts {
        ipt += 1; ipt - 1
      } = coord(x, y)
      i += 1
    }
    i = 0
    while (i < nSide) {
      val x = env.getMaxX - i * XsegLen
      val y = env.getMaxY
      pts {
        ipt += 1; ipt - 1
      } = coord(x, y)
      i += 1
    }
    i = 0
    while (i < nSide) {
      val x = env.getMinX
      val y = env.getMaxY - i * YsegLen
      pts {
        ipt += 1; ipt - 1
      } = coord(x, y)
      i += 1
    }
    pts {
      ipt += 1; ipt - 1
    } = new Coordinate(pts(0))
    val ring    = geomFact.createLinearRing(pts)
    val poly    = geomFact.createPolygon(ring)
    rotate(poly).asInstanceOf[Polygon]
  }

  // /**
  //  * Creates a circular or elliptical {link Polygon}.
  //  *
  //  * return a circle or ellipse
  //  */
  def createCircle: Polygon = createEllipse

  // /**
  //  * Creates an elliptical {link Polygon}.
  //  * If the supplied envelope is square the
  //  * result will be a circle.
  //  *
  //  * return an ellipse or circle
  //  */
  def createEllipse: Polygon = {
    val env     = dim.getEnvelope
    val xRadius = env.getWidth / 2.0
    val yRadius = env.getHeight / 2.0
    val centreX = env.getMinX + xRadius
    val centreY = env.getMinY + yRadius
    val pts     = new Array[Coordinate](nPts + 1)
    var iPt     = 0
    var i       = 0
    while (i < nPts) {
      val ang = i * (2 * Math.PI / nPts)
      val x   = xRadius * Math.cos(ang) + centreX
      val y   = yRadius * Math.sin(ang) + centreY
      pts {
        iPt += 1; iPt - 1
      } = coord(x, y)
      i += 1
    }
    pts(iPt) = new Coordinate(pts(0))
    val ring    = geomFact.createLinearRing(pts)
    val poly    = geomFact.createPolygon(ring)
    rotate(poly).asInstanceOf[Polygon]
  }

  // /**
  //  * Creates a squircular {link Polygon}.
  //  *
  //  * return a squircle
  //  */
  def createSquircle: Polygon = createSupercircle(4)

  // /**
  //  * Creates a supercircular {link Polygon}
  //  * of a given positive power.
  //  *
  //  * return a supercircle
  //  */
  def createSupercircle(power: Double): Polygon = {
    val recipPow   = 1.0 / power
    val radius     = dim.getMinSize / 2
    val centre     = dim.getCentre
    val r4         = Math.pow(radius, power)
    val y0         = radius
    val xyInt      = Math.pow(r4 / 2, recipPow)
    val nSegsInOct = nPts / 8
    val totPts     = nSegsInOct * 8 + 1
    val pts        = new Array[Coordinate](totPts)
    val xInc       = xyInt / nSegsInOct
    var i          = 0
    while (i <= nSegsInOct) {
      var x = 0.0
      var y = y0
      if (i != 0) {
        x = xInc * i
        val x4 = Math.pow(x, power)
        y = Math.pow(r4 - x4, recipPow)
      }
      pts(i) = coordTrans(x, y, centre)
      pts(2 * nSegsInOct - i) = coordTrans(y, x, centre)
      pts(2 * nSegsInOct + i) = coordTrans(y, -x, centre)
      pts(4 * nSegsInOct - i) = coordTrans(x, -y, centre)
      pts(4 * nSegsInOct + i) = coordTrans(-x, -y, centre)
      pts(6 * nSegsInOct - i) = coordTrans(-y, -x, centre)
      pts(6 * nSegsInOct + i) = coordTrans(-y, x, centre)
      pts(8 * nSegsInOct - i) = coordTrans(-x, y, centre)
      i += 1
    }
    pts(pts.length - 1) = new Coordinate(pts(0))
    val ring       = geomFact.createLinearRing(pts)
    val poly       = geomFact.createPolygon(ring)
    rotate(poly).asInstanceOf[Polygon]
  }

  /**
   * Creates an elliptical arc, as a {link LineString}. The arc is always created in a
   * counter-clockwise direction. This can easily be reversed if required by using {#link
   * LineString.reverse()}
   *
   * @param startAng
   *   start angle in radians
   * @param angExtent
   *   size of angle in radians return an elliptical arc
   */
  def createArc(startAng: Double, angExtent: Double): LineString = {
    val env     = dim.getEnvelope
    val xRadius = env.getWidth / 2.0
    val yRadius = env.getHeight / 2.0
    val centreX = env.getMinX + xRadius
    val centreY = env.getMinY + yRadius
    var angSize = angExtent
    if (angSize <= 0.0 || angSize > 2 * Math.PI) angSize = 2 * Math.PI
    val angInc  = angSize / (nPts - 1)
    val pts     = new Array[Coordinate](nPts)
    var iPt     = 0
    var i       = 0
    while (i < nPts) {
      val ang = startAng + i * angInc
      val x   = xRadius * Math.cos(ang) + centreX
      val y   = yRadius * Math.sin(ang) + centreY
      pts {
        iPt += 1; iPt - 1
      } = coord(x, y)
      i += 1
    }
    val line    = geomFact.createLineString(pts)
    rotate(line).asInstanceOf[LineString]
  }

  /**
   * Creates an elliptical arc polygon. The polygon is formed from the specified arc of an ellipse
   * and the two radii connecting the endpoints to the centre of the ellipse.
   *
   * @param startAng
   *   start angle in radians
   * @param angExtent
   *   size of angle in radians return an elliptical arc polygon
   */
  def createArcPolygon(startAng: Double, angExtent: Double): Polygon = {
    val env     = dim.getEnvelope
    val xRadius = env.getWidth / 2.0
    val yRadius = env.getHeight / 2.0
    val centreX = env.getMinX + xRadius
    val centreY = env.getMinY + yRadius
    var angSize = angExtent
    if (angSize <= 0.0 || angSize > 2 * Math.PI) angSize = 2 * Math.PI
    val angInc  = angSize / (nPts - 1)
    // double check = angInc * nPts;
    // double checkEndAng = startAng + check;
    val pts     = new Array[Coordinate](nPts + 2)
    var iPt     = 0
    pts {
      iPt += 1; iPt - 1
    } = coord(centreX, centreY)
    var i       = 0
    while (i < nPts) {
      val ang = startAng + angInc * i
      val x   = xRadius * Math.cos(ang) + centreX
      val y   = yRadius * Math.sin(ang) + centreY
      pts {
        iPt += 1; iPt - 1
      } = coord(x, y)
      i += 1
    }
    pts {
      iPt += 1; iPt - 1
    } = coord(centreX, centreY)
    val ring    = geomFact.createLinearRing(pts)
    val poly    = geomFact.createPolygon(ring)
    rotate(poly).asInstanceOf[Polygon]
  }

  protected def coord(x: Double, y: Double): Coordinate = {
    val pt = new Coordinate(x, y)
    precModel.makePrecise(pt)
    pt
  }

  protected def coordTrans(x: Double, y: Double, trans: Coordinate): Coordinate =
    coord(x + trans.x, y + trans.y)
}
