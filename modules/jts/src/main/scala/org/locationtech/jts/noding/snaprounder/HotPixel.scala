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
package org.locationtech.jts.noding.snaprounder

import org.locationtech.jts.algorithm.LineIntersector
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.noding.NodedSegmentString
import org.locationtech.jts.util.Assert

/**
 * Implements a "hot pixel" as used in the Snap Rounding algorithm. A hot pixel contains the
 * interior of the tolerance square and the boundary <b>minus</b> the top and right segments. <p>
 * The hot pixel operations are all computed in the integer domain to avoid rounding problems.
 *
 * @version 1.7
 */
object HotPixel {
  private val SAFE_ENV_EXPANSION_FACTOR = 0.75
}

class HotPixel(var originalPt: Coordinate, var scaleFactor: Double, var li: LineIntersector) {

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
  // tolerance = 0.5;
  private var p0Scaled: Coordinate = null
  private var p1Scaled: Coordinate = null
  if (scaleFactor <= 0) throw new IllegalArgumentException("Scale factor must be non-zero")
  if (scaleFactor != 1.0) {
    this.originalPt = new Coordinate(scale(originalPt.x), scale(originalPt.y))
    p0Scaled = new Coordinate
    p1Scaled = new Coordinate
  }
  initCorners(this.originalPt)
  private val pt                   = originalPt
//  private val ptScaled = null
  private var minx                 = .0
  private var maxx                 = .0
  private var miny                 = .0
  private var maxy                 = .0

  /**
   * The corners of the hot pixel, in the order: 10 23
   */
  private val corner            = new Array[Coordinate](4)
  private var safeEnv: Envelope = null

  /**
   * Gets the coordinate this hot pixel is based at.
   *
   * return the coordinate of the pixel
   */
  def getCoordinate = originalPt

  /**
   * Returns a "safe" envelope that is guaranteed to contain the hot pixel. The envelope returned
   * will be larger than the exact envelope of the pixel.
   *
   * return an envelope which contains the hot pixel
   */
  def getSafeEnvelope = {
    if (safeEnv == null) {
      val safeTolerance = HotPixel.SAFE_ENV_EXPANSION_FACTOR / scaleFactor
      safeEnv = new Envelope(originalPt.x - safeTolerance,
                             originalPt.x + safeTolerance,
                             originalPt.y - safeTolerance,
                             originalPt.y + safeTolerance
      )
    }
    safeEnv
  }

  private def initCorners(pt: Coordinate) = {
    val tolerance = 0.5
    minx = pt.x - tolerance
    maxx = pt.x + tolerance
    miny = pt.y - tolerance
    maxy = pt.y + tolerance
    corner(0) = new Coordinate(maxx, maxy)
    corner(1) = new Coordinate(minx, maxy)
    corner(2) = new Coordinate(minx, miny)
    corner(3) = new Coordinate(maxx, miny)
  }

  private def scale(`val`: Double) = `val` * scaleFactor.round.toDouble

  /**
   * Tests whether the line segment (p0-p1) intersects this hot pixel.
   *
   * @param p0
   *   the first coordinate of the line segment to test
   * @param p1
   *   the second coordinate of the line segment to test return true if the line segment intersects
   *   this hot pixel
   */
  def intersects(p0: Coordinate, p1: Coordinate): Boolean = {
    if (scaleFactor == 1.0) return intersectsScaled(p0, p1)
    copyScaled(p0, p0Scaled)
    copyScaled(p1, p1Scaled)
    intersectsScaled(p0Scaled, p1Scaled)
  }

  private def copyScaled(p: Coordinate, pScaled: Coordinate): Unit = {
    pScaled.x = scale(p.x)
    pScaled.y = scale(p.y)
  }

  private def intersectsScaled(p0: Coordinate, p1: Coordinate): Boolean = {
    val segMinx           = Math.min(p0.x, p1.x)
    val segMaxx           = Math.max(p0.x, p1.x)
    val segMiny           = Math.min(p0.y, p1.y)
    val segMaxy           = Math.max(p0.y, p1.y)
    val isOutsidePixelEnv = maxx < segMinx || minx > segMaxx || maxy < segMiny || miny > segMaxy
    if (isOutsidePixelEnv) return false
    val intersects        = intersectsToleranceSquare(p0, p1)
    //    boolean intersectsPixelClosure = intersectsPixelClosure(p0, p1);
    //    if (intersectsPixel != intersects) {
    //      Debug.println("Found hot pixel intersection mismatch at " + pt);
    //      Debug.println("Test segment: " + p0 + " " + p1);
    //    }
    /*
        if (scaleFactor != 1.0) {
          boolean intersectsScaled = intersectsScaledTest(p0, p1);
          if (intersectsScaled != intersects) {
            intersectsScaledTest(p0, p1);
    //        Debug.println("Found hot pixel scaled intersection mismatch at " + pt);
    //        Debug.println("Test segment: " + p0 + " " + p1);
          }
          return intersectsScaled;
        }
     */
    Assert.isTrue(!(isOutsidePixelEnv && intersects), "Found bad envelope test")
    //    if (isOutsideEnv && intersects) {
    //      Debug.println("Found bad envelope test");
    intersects
    // return intersectsPixelClosure;
  }

  /**
   * Tests whether the segment p0-p1 intersects the hot pixel tolerance square. Because the
   * tolerance square point set is partially open (along the top and right) the test needs to be
   * more sophisticated than simply checking for any intersection. However, it can take advantage of
   * the fact that the hot pixel edges do not lie on the coordinate grid. It is sufficient to check
   * if any of the following occur: <ul> <li>a proper intersection between the segment and any hot
   * pixel edge <li>an intersection between the segment and <b>both</b> the left and bottom hot
   * pixel edges (which detects the case where the segment intersects the bottom left hot pixel
   * corner) <li>an intersection between a segment endpoint and the hot pixel coordinate </ul>
   *
   * @param p0
   * @param p1
   *   return
   */
  private def intersectsToleranceSquare(p0: Coordinate, p1: Coordinate): Boolean = {
    var intersectsLeft   = false
    var intersectsBottom = false
    // System.out.println("Hot Pixel: " + WKTWriter.toLineString(corner));
    // System.out.println("Line: " + WKTWriter.toLineString(p0, p1));
    li.computeIntersection(p0, p1, corner(0), corner(1))
    if (li.isProperF) return true
    li.computeIntersection(p0, p1, corner(1), corner(2))
    if (li.isProperF) return true
    if (li.hasIntersection) intersectsLeft = true
    li.computeIntersection(p0, p1, corner(2), corner(3))
    if (li.isProperF) return true
    if (li.hasIntersection) intersectsBottom = true
    li.computeIntersection(p0, p1, corner(3), corner(0))
    if (li.isProperF) return true
    if (intersectsLeft && intersectsBottom) return true
    if (p0 == pt) return true
    if (p1 == pt) return true
    false
  }

  /**
   * Test whether the given segment intersects the closure of this hot pixel. This is NOT the test
   * used in the standard snap-rounding algorithm, which uses the partially closed tolerance square
   * instead. This routine is provided for testing purposes only.
   *
   * @param p0
   *   the start point of a line segment
   * @param p1
   *   the end point of a line segment return <code>true</code> if the segment intersects the
   *   closure of the pixel's tolerance square
   */
//  private def intersectsPixelClosure(p0: Coordinate, p1: Coordinate): Boolean = {
//    li.computeIntersection(p0, p1, corner(0), corner(1))
//    if (li.hasIntersection) return true
//    li.computeIntersection(p0, p1, corner(1), corner(2))
//    if (li.hasIntersection) return true
//    li.computeIntersection(p0, p1, corner(2), corner(3))
//    if (li.hasIntersection) return true
//    li.computeIntersection(p0, p1, corner(3), corner(0))
//    if (li.hasIntersection) return true
//    false
//  }

  /**
   * Adds a new node (equal to the snap pt) to the specified segment if the segment passes through
   * the hot pixel
   *
   * @param segStr
   * @param segIndex
   *   return true if a node was added to the segment
   */
  def addSnappedNode(segStr: NodedSegmentString, segIndex: Int): Boolean = {
    val p0 = segStr.getCoordinate(segIndex)
    val p1 = segStr.getCoordinate(segIndex + 1)
    if (intersects(p0, p1)) { // System.out.println("snapped: " + snapPt);
      // System.out.println("POINT (" + snapPt.x + " " + snapPt.y + ")");
      segStr.addIntersection(getCoordinate, segIndex)
      return true
    }
    false
  }
}
