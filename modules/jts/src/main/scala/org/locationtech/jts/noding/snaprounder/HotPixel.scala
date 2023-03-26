// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.noding.snapround

import org.locationtech.jts.algorithm.CGAlgorithmsDD
import org.locationtech.jts.algorithm.LineIntersector
import org.locationtech.jts.algorithm.RobustLineIntersector
import org.locationtech.jts.geom.Coordinate

/*
 * Copyright (c) 2016 Vivid Solutions.
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
 * Implements a "hot pixel" as used in the Snap Rounding algorithm. A hot pixel is a square region
 * centred on the rounded valud of the coordinate given, and of width equal to the size of the scale
 * factor. It is a partially open region, which contains the interior of the tolerance square and
 * the boundary <b>minus</b> the top and right segments. This ensures that every point of the space
 * lies in a unique hot pixel. It also matches the rounding semantics for numbers. <p> The hot pixel
 * operations are all computed in the integer domain to avoid rounding problems. <p> Hot Pixels
 * support being marked as nodes. This is used to prevent introducing nodes at line vertices which
 * do not have other lines snapped to them.
 *
 * @version 1.7
 */
object HotPixel {
  private val TOLERANCE   = 0.5
  private val UPPER_RIGHT = 0
  private val UPPER_LEFT  = 1
  private val LOWER_LEFT  = 2
  private val LOWER_RIGHT = 3
}

class HotPixel(var originalPt: Coordinate, var scaleFactor: Double) {

  /**
   * Indicates if this hot pixel must be a node in the output
   */
  private var isNode0: Boolean = false

  /**
   * Creates a new hot pixel, using a given scale factor. The scale factor must be strictly positive
   * (non-zero).
   *
   * @param pt
   *   the coordinate at the centre of the pixel
   * @param scaleFactor
   *   the scaleFactor determining the pixel size. Must be &gt; 0
   * @param li
   *   the intersector to use for testing intersection with line segments
   */
  if (scaleFactor <= 0) {
    throw new IllegalArgumentException("Scale factor must be non-zero")
  }
  val (hpx, hpy) = if (scaleFactor != 1.0) {
    (scaleRound(originalPt.getX), scaleRound(originalPt.getY))
  } else {
    (originalPt.getX, originalPt.getY)
  }

  /**
   * Gets the coordinate this hot pixel is based at.
   *
   * @return
   *   the coordinate of the pixel
   */
  def getCoordinate: Coordinate =
    return originalPt

  /**
   * Gets the scale factor for the precision grid for this pixel.
   *
   * @return
   *   the pixel scale factor
   */
  def getScaleFactor: Double =
    return scaleFactor

  /**
   * Gets the width of the hot pixel in the original coordinate system.
   *
   * @return
   *   the width of the hot pixel tolerance square
   */
  def getWidth: Double =
    return 1.0 / scaleFactor

  /**
   * Tests whether this pixel has been marked as a node.
   *
   * @return
   *   true if the pixel is marked as a node
   */
  def isNode: Boolean =
    return isNode0

  /**
   * Sets this pixel to be a node.
   */
  def setToNode(): Unit = // System.out.println(this + " set to Node");
    isNode0 = true

  private def scaleRound(`val`: Double): Double =
    return `val` * scaleFactor.round.toDouble

  private def scaleRound(p: Coordinate): Coordinate =
    return new Coordinate(scaleRound(p.x), scaleRound(p.y))

  /**
   * Scale without rounding. This ensures intersections are checked against original linework. This
   * is required to ensure that intersections are not missed because the segment is moved by
   * snapping.
   *
   * @param val
   * @return
   */
  private def scale(`val`: Double): Double =
    return `val` * scaleFactor

  /**
   * Tests whether a coordinate lies in (intersects) this hot pixel.
   *
   * @param p
   *   the coordinate to test
   * @return
   *   true if the coordinate intersects this hot pixel
   */
  def intersects(p: Coordinate): Boolean = {
    val x: Double = scale(p.x)
    val y: Double = scale(p.y)
    if (x >= hpx + HotPixel.TOLERANCE) {
      return false
    }
    // check Left side
    if (x < hpx - HotPixel.TOLERANCE) {
      return false
    }
    // check Top side
    if (y >= hpy + HotPixel.TOLERANCE) {
      return false
    }
    // check Bottom side
    if (y < hpy - HotPixel.TOLERANCE) {
      return false
    }
    return true
  }

  /**
   * Tests whether the line segment (p0-p1) intersects this hot pixel.
   *
   * @param p0
   *   the first coordinate of the line segment to test
   * @param p1
   *   the second coordinate of the line segment to test
   * @return
   *   true if the line segment intersects this hot pixel
   */
  def intersects(p0: Coordinate, p1: Coordinate): Boolean = {
    if (scaleFactor == 1.0) {
      return intersectsScaled(p0.x, p0.y, p1.x, p1.y)
    }
    val sp0x: Double = scale(p0.x)
    val sp0y: Double = scale(p0.y)
    val sp1x: Double = scale(p1.x)
    val sp1y: Double = scale(p1.y)
    return intersectsScaled(sp0x, sp0y, sp1x, sp1y)
  }

  private def intersectsScaled(p0x: Double, p0y: Double, p1x: Double, p1y: Double): Boolean = { // determine oriented segment pointing in positive X direction
    var px: Double = p0x
    var py: Double = p0y
    var qx: Double = p1x
    var qy: Double = p1y
    if (px > qx) {
      px = p1x
      py = p1y
      qx = p0x
      qy = p0y
    }

    /**
     * Report false if segment env does not intersect pixel env. This check reflects the fact that
     * the pixel Top and Right sides are open (not part of the pixel).
     */
    // check Right side
    val maxx            = hpx + HotPixel.TOLERANCE
    val segMinx: Double = Math.min(px, qx)
    if (segMinx >= maxx) {
      return false
    }
    val minx            = hpx - HotPixel.TOLERANCE
    val segMaxx: Double = Math.max(px, qx)
    if (segMaxx < minx) {
      return false
    }
    val maxy            = hpy + HotPixel.TOLERANCE
    val segMiny: Double = Math.min(py, qy)
    if (segMiny >= maxy) {
      return false
    }
    val miny            = hpy - HotPixel.TOLERANCE
    val segMaxy: Double = Math.max(py, qy)
    if (segMaxy < miny) {
      return false
    }

    /**
     * Vertical or horizontal segments must now intersect the segment interior or Left or Bottom
     * sides.
     */
    // ---- check vertical segment
    if (px == qx) {
      return true
    }
    // ---- check horizontal segment
    if (py == qy) {
      return true
    }

    /**
     * Now know segment is not horizontal or vertical.
     *
     * Compute orientation WRT each pixel corner. If corner orientation == 0, segment intersects the
     * corner. From the corner and whether segment is heading up or down, can determine intersection
     * or not.
     *
     * Otherwise, check whether segment crosses interior of pixel side This is the case if the
     * orientations for each corner of the side are different.
     */
    val orientUL: Int = CGAlgorithmsDD.orientationIndex(px, py, qx, qy, minx, maxy)
    if (orientUL == 0) {
      if (py < qy) {
        return false
      }
      return true
    }
    val orientUR: Int = CGAlgorithmsDD.orientationIndex(px, py, qx, qy, maxx, maxy)
    if (orientUR == 0) {
      if (py > qy) {
        return false
      }
      return true
    }
    // --- check crossing Top side
    if (orientUL != orientUR) {
      return true
    }
    val orientLL: Int = CGAlgorithmsDD.orientationIndex(px, py, qx, qy, minx, miny)
    if (orientUL == 0) { // LL corner is the only one in pixel interior
      return true
    }
    // --- check crossing Left side
    if (orientLL != orientUL) {
      return true
    }
    val orientLR: Int = CGAlgorithmsDD.orientationIndex(px, py, qx, qy, maxx, miny)
    if (orientLR == 0) {
      if (py < qy) {
        return false
      }
      return true
    }
    // --- check crossing Bottom side
    if (orientLL != orientLR) {
      return true
    }
    // --- check crossing Right side
    if (orientLR != orientUR) {
      return true
    }
    // segment does not intersect pixel
    return false
  }

  /**
   * Test whether a segment intersects the closure of this hot pixel. This is NOT the test used in
   * the standard snap-rounding algorithm, which uses the partially-open tolerance square instead.
   * This method is provided for testing purposes only.
   *
   * @param p0
   *   the start point of a line segment
   * @param p1
   *   the end point of a line segment
   * @return
   *   <code>true</code> if the segment intersects the closure of the pixel's tolerance square
   */
  private def intersectsPixelClosure(p0: Coordinate, p1: Coordinate): Boolean = {
    val maxx                      = hpx + HotPixel.TOLERANCE
    val minx                      = hpx - HotPixel.TOLERANCE
    val miny                      = hpy - HotPixel.TOLERANCE
    val maxy                      = hpy + HotPixel.TOLERANCE
    val corner: Array[Coordinate] = new Array[Coordinate](4)
    corner(HotPixel.UPPER_RIGHT) = new Coordinate(maxx, maxy)
    corner(HotPixel.UPPER_LEFT) = new Coordinate(minx, maxy)
    corner(HotPixel.LOWER_LEFT) = new Coordinate(minx, miny)
    corner(HotPixel.LOWER_RIGHT) = new Coordinate(maxx, miny)
    val li: LineIntersector       = new RobustLineIntersector
    li.computeIntersection(p0, p1, corner(0), corner(1))
    if (li.hasIntersection) {
      return true
    }
    li.computeIntersection(p0, p1, corner(1), corner(2))
    if (li.hasIntersection) {
      return true
    }
    li.computeIntersection(p0, p1, corner(2), corner(3))
    if (li.hasIntersection) {
      return true
    }
    li.computeIntersection(p0, p1, corner(3), corner(0))
    if (li.hasIntersection) {
      return true
    }
    return false
  }

  override def toString: String =
    return "HP(" /*+ WKTWriter.format(ptHot)*/ + ")"
}
