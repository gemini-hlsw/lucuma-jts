// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import org.locationtech.jts.geom.impl.CoordinateArraySequence

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
  def isCCW(ring: Array[Coordinate]): Boolean = // # of points without closing endpoint
    // wrap with an XY CoordinateSequence
    isCCW(new CoordinateArraySequence(ring, 2, 0))

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
  def isCCW(ring: CoordinateSequence): Boolean    = {
    val nPts = ring.size - 1
    if (nPts < 3)
      throw new IllegalArgumentException(
        "Ring has fewer than 4 points, so orientation cannot be determined"
      )

    /**
     * Find first highest point after a lower point, if one exists (e.g. a rising segment) If one
     * does not exist, hiIndex will remain 0 and the ring must be flat. Note this relies on the
     * convention that rings have the same start and end point.
     */
    var upHiPt              = ring.getCoordinate(0)
    var prevY               = upHiPt.y
    var upLowPt: Coordinate = null;
    var iUpHi               = 0
    for (i <- 1 to nPts) {
      val py = ring.getOrdinate(i, Coordinate.Y)

      /**
       * If segment is upwards and endpoint is higher, record it
       */
      if (py > prevY && py >= upHiPt.y) {
        upHiPt = ring.getCoordinate(i)
        iUpHi = i
        upLowPt = ring.getCoordinate(i - 1)
      }
      prevY = py
    }

    if (iUpHi == 0) return false

    /**
     * Find the next lower point after the high point (e.g. a falling segment). This must exist
     * since ring is not flat.
     */
    var iDownLow = iUpHi

    while ({
      iDownLow = (iDownLow + 1) % nPts
      iDownLow != iUpHi && ring.getOrdinate(iDownLow, Coordinate.Y) == upHiPt.y
    }) {}

    val downLowPt = ring.getCoordinate(iDownLow)
    val iDownHi   = if (iDownLow > 0) iDownLow - 1 else nPts - 1
    val downHiPt  = ring.getCoordinate(iDownHi)

    /**
     * Two cases can occur: 1) the hiPt and the downPrevPt are the same. This is the general
     * position case of a "pointed cap". The ring orientation is determined by the orientation of
     * the cap 2) The hiPt and the downPrevPt are different. In this case the top of the cap is
     * flat. The ring orientation is given by the direction of the flat segment
     */
    if (upHiPt.equals2D(downHiPt)) {

      /**
       * Check for the case where the cap has configuration A-B-A. This can happen if the ring does
       * not contain 3 distinct points (including the case where the input array has fewer than 4
       * elements), or it contains coincident line segments.
       */
      if (upLowPt.equals2D(upHiPt) || downLowPt.equals2D(upHiPt) || upLowPt.equals2D(downLowPt))
        return false

      /**
       * It can happen that the top segments are coincident. This is an invalid ring, which cannot
       * be computed correctly. In this case the orientation is 0, and the result is false.
       */
      val indexOf = index(upLowPt, upHiPt, downLowPt)
      indexOf == COUNTERCLOCKWISE
    } else {

      /**
       * Flat cap - direction of flat top determines orientation
       */
      val delX = downHiPt.x - upHiPt.x
      delX < 0
    }
  }
  def isCCWArea(ring: Array[Coordinate]): Boolean =
    Area.ofRingSigned(ring) < 0
}
