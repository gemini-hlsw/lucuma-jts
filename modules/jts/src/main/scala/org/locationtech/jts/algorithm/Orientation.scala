// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

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
package org.locationtech.jts.algorithm

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateSequence

/**
 * Functions to compute the orientation of basic geometric structures including point triplets
 * (triangles) and rings. Orientation is a fundamental property of planar geometries (and more
 * generally geometry on two-dimensional manifolds). <p> Orientation is notoriously subject to
 * numerical precision errors in the case of collinear or nearly collinear points. JTS uses
 * extended-precision arithmetic to increase the robustness of the computation.
 *
 * @author
 *   Martin Davis
 */
object Orientation {

  /**
   * A value that indicates an orientation of clockwise, or a right turn.
   */
  val CLOCKWISE: Int = -1
  val RIGHT: Int     = CLOCKWISE

  /**
   * A value that indicates an orientation of counterclockwise, or a left turn.
   */
  val COUNTERCLOCKWISE = 1
  val LEFT: Int        = COUNTERCLOCKWISE

  /**
   * A value that indicates an orientation of collinear, or no turn (straight).
   */
  val COLLINEAR     = 0
  val STRAIGHT: Int = COLLINEAR

  /**
   * Returns the orientation index of the direction of the point <code>q</code> relative to a
   * directed infinite line specified by <code>p1-p2</code>. The index indicates whether the point
   * lies to the {link #LEFT} or {link #RIGHT} of the line, or lies on it {link #COLLINEAR}. The
   * index also indicates the orientation of the triangle formed by the three points ( {link
   * #COUNTERCLOCKWISE}, {link #CLOCKWISE}, or {link #STRAIGHT} )
   *
   * @param p1
   *   the origin point of the line vector
   * @param p2
   *   the final point of the line vector
   * @param q
   *   the point to compute the direction to return -1 ( { @link #CLOCKWISE} or { @link #RIGHT} ) if
   *   q is clockwise (right) from p1-p2; 1 ( { @link #COUNTERCLOCKWISE} or { @link #LEFT} ) if q is
   *   counter-clockwise (left) from p1-p2; 0 ( { @link #COLLINEAR} or { @link #STRAIGHT} ) if q is
   *   collinear with p1-p2
   */
  def index(p1: Coordinate, p2: Coordinate, q: Coordinate): Int =
    /*
     * MD - 9 Aug 2010 It seems that the basic algorithm is slightly orientation
     * dependent, when computing the orientation of a point very close to a
     * line. This is possibly due to the arithmetic in the translation to the
     * origin.
     *
     * For instance, the following situation produces identical results in spite
     * of the inverse orientation of the line segment:
     *
     * Coordinate p0 = new Coordinate(219.3649559090992, 140.84159161824724);
     * Coordinate p1 = new Coordinate(168.9018919682399, -5.713787599646864);
     *
     * Coordinate p = new Coordinate(186.80814046338352, 46.28973405831556); int
     * orient = orientationIndex(p0, p1, p); int orientInv =
     * orientationIndex(p1, p0, p);
     *
     * A way to force consistent results is to normalize the orientation of the
     * vector using the following code. However, this may make the results of
     * orientationIndex inconsistent through the triangle of points, so it's not
     * clear this is an appropriate patch.
     *
     */ CGAlgorithmsDD.orientationIndex(p1, p2, q)
  // testing only
  // return ShewchuksDeterminant.orientationIndex(p1, p2, q);
  // previous implementation - not quite fully robust
  // return RobustDeterminant.orientationIndex(p1, p2, q);

  /**
   * Computes whether a ring defined by an array of {link Coordinate}s is oriented
   * counter-clockwise. <ul> <li>The list of points is assumed to have the first and last points
   * equal. <li>This will handle coordinate lists which contain repeated points. </ul> This
   * algorithm is <b>only</b> guaranteed to work with valid rings. If the ring is invalid (e.g.
   * self-crosses or touches), the computed result may not be correct.
   *
   * @param ring
   *   an array of Coordinates forming a ring return true if the ring is oriented counter-clockwise.
   *   throws IllegalArgumentException if there are too few points to determine orientation (&lt; 4)
   */
  def isCCW(ring: Array[Coordinate]): Boolean = { // # of points without closing endpoint
    val nPts    = ring.length - 1
    // sanity check
    if (nPts < 3)
      throw new IllegalArgumentException(
        "Ring has fewer than 4 points, so orientation cannot be determined"
      )
    // find highest point
    var hiPt    = ring(0)
    var hiIndex = 0
    var i       = 1
    while (i <= nPts) {
      val p = ring(i)
      if (p.y > hiPt.y) {
        hiPt = p
        hiIndex = i
      }
      {
        i += 1; i - 1
      }
    }
    // find distinct point before highest point
    var iPrev   = hiIndex
    while ({
      {
        iPrev = iPrev - 1
        if (iPrev < 0) iPrev = nPts
      }; ring(iPrev).equals2D(hiPt) && iPrev != hiIndex
    }) ()
    // find distinct point after highest point
    var iNext   = hiIndex
    while ({ { iNext = (iNext + 1) % nPts }; (ring(iNext).equals2D(hiPt) && iNext != hiIndex) }) ()
    val prev    = ring(iPrev)
    val next    = ring(iNext)
    /*
     * This check catches cases where the ring contains an A-B-A configuration
     * of points. This can happen if the ring does not contain 3 distinct points
     * (including the case where the input array has fewer than 4 elements), or
     * it contains coincident line segments.
     */
    if (prev.equals2D(hiPt) || next.equals2D(hiPt) || prev.equals2D(next)) return false
    val disc    = Orientation.index(prev, hiPt, next)
    /*
     * If disc is exactly 0, lines are collinear. There are two possible cases:
     * (1) the lines lie along the x axis in opposite directions (2) the lines
     * lie on top of one another
     *
     * (1) is handled by checking if next is left of prev ==> CCW (2) will never
     * happen if the ring is valid, so don't check for it (Might want to assert
     * this)
     */
    var isCCW   = false
    if (disc == 0) { // poly is CCW if prev x is right of next x
      isCCW = prev.x > next.x
    } else { // if area is positive, points are ordered CCW
      isCCW = disc > 0
    }
    isCCW
  }

  /**
   * Computes whether a ring defined by an {link CoordinateSequence} is oriented counter-clockwise.
   * <ul> <li>The list of points is assumed to have the first and last points equal. <li>This will
   * handle coordinate lists which contain repeated points. </ul> This algorithm is <b>only</b>
   * guaranteed to work with valid rings. If the ring is invalid (e.g. self-crosses or touches), the
   * computed result may not be correct.
   *
   * @param ring
   *   a CoordinateSequence forming a ring return true if the ring is oriented counter-clockwise.
   *   throws IllegalArgumentException if there are too few points to determine orientation (&lt; 4)
   */
  def isCCW(ring: CoordinateSequence): Boolean = {
    val nPts             = ring.size - 1
    if (nPts < 3)
      throw new IllegalArgumentException(
        "Ring has fewer than 4 points, so orientation cannot be determined"
      )
    var hiPt             = ring.getCoordinate(0)
    var hiIndex          = 0
    var i                = 1
    while (i <= nPts) {
      val p = ring.getCoordinate(i)
      if (p.y > hiPt.y) {
        hiPt = p
        hiIndex = i
      }
      {
        i += 1; i - 1
      }
    }
    var prev: Coordinate = null
    var iPrev            = hiIndex
    while ({
      {
        iPrev = iPrev - 1
        if (iPrev < 0) iPrev = nPts
        prev = ring.getCoordinate(iPrev)
      }; prev.equals2D(hiPt) && iPrev != hiIndex
    }) ()
    var next: Coordinate = null
    var iNext            = hiIndex
    while ({
      {
        iNext = (iNext + 1) % nPts
        next = ring.getCoordinate(iNext)
      }; next.equals2D(hiPt) && iNext != hiIndex
    }) ()
    if (prev.equals2D(hiPt) || next.equals2D(hiPt) || prev.equals2D(next)) return false
    val disc             = Orientation.index(prev, hiPt, next)
    /*
     * If disc is exactly 0, lines are collinear. There are two possible cases:
     * (1) the lines lie along the x axis in opposite directions (2) the lines
     * lie on top of one another
     *
     * (1) is handled by checking if next is left of prev ==> CCW (2) will never
     * happen if the ring is valid, so don't check for it (Might want to assert
     * this)
     */
    var isCCW            = false
    if (disc == 0) isCCW = prev.x > next.x
    else isCCW = disc > 0
    isCCW
  }
}
