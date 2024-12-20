// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.operation.overlayng

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateList
import org.locationtech.jts.geom.Envelope

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
 * Clips rings of points to a rectangle. Uses a variant of Cohen-Sutherland clipping. <p> In general
 * the output is not topologically valid. In particular, the output may contain coincident non-noded
 * line segments along the clip rectangle sides. However, the output is sufficiently well-structured
 * that it can be used as input to the {@link OverlayNG} algorithm (which is able to process
 * coincident linework due to the need to handle topology collapse under precision reduction). <p>
 * Because of the likelihood of creating extraneous line segments along the clipping rectangle
 * sides, this class is not suitable for clipping linestrings. <p> The clipping envelope should be
 * generated using {@link RobustClipEnvelopeComputer} , to ensure that intersecting line segments
 * are not perturbed by clipping. This is required to ensure that the overlay of the clipped
 * geometry is robust and correct (i.e. the same as if clipping was not used).
 *
 * @see
 *   LineLimiter
 * @author
 *   Martin Davis
 */
object RingClipper {
  private val BOX_LEFT   = 3
  private val BOX_TOP    = 2
  private val BOX_RIGHT  = 1
  private val BOX_BOTTOM = 0
}

/**
 * Creates a new clipper for the given envelope.
 *
 * @param clipEnv
 *   the clipping envelope
 */
class RingClipper(var clipEnv: Envelope) {

  private val clipEnvMinY = clipEnv.getMinY
  private val clipEnvMaxY = clipEnv.getMaxY
  private val clipEnvMinX = clipEnv.getMinX
  private val clipEnvMaxX = clipEnv.getMaxX

  /**
   * Clips a list of points to the clipping rectangle box.
   *
   * @param ring
   * @param env
   * @return
   */
  def clip(pts: Array[Coordinate]): Array[Coordinate] = {
    var pts0 = pts
    for (edgeIndex <- 0 until 4) {
      val closeRing: Boolean = edgeIndex == 3
      pts0 = clipToBoxEdge(pts0, edgeIndex, closeRing)
      if (pts0.length == 0) {
        return pts0
      }
    }
    return pts0
  }

  /**
   * Clips line to the axis-parallel line defined by a single box edge.
   *
   * @param pts
   * @param edgeIndex
   * @param closeRing
   * @return
   */
  private def clipToBoxEdge(
    pts:       Array[Coordinate],
    edgeIndex: Int,
    closeRing: Boolean
  ): Array[Coordinate] = { // TODO: is it possible to avoid copying array 4 times?
    val ptsClip: CoordinateList = new CoordinateList
    var p0: Coordinate          = pts(pts.length - 1)
    for (i <- 0 until pts.length) {
      val p1: Coordinate = pts(i)
      if (isInsideEdge(p1, edgeIndex)) {
        if (!isInsideEdge(p0, edgeIndex)) {
          val intPt: Coordinate = intersection(p0, p1, edgeIndex)
          ptsClip.add(intPt, false)
        }
        // TODO: avoid copying so much?
        ptsClip.add(p1.copy, false)
      } else {
        if (isInsideEdge(p0, edgeIndex)) {
          val intPt: Coordinate = intersection(p0, p1, edgeIndex)
          ptsClip.add(intPt, false)
        }
      }
      // else p0-p1 is outside box, so it is dropped
      p0 = p1
    }
    // add closing point if required
    if (closeRing && ptsClip.size > 0) {
      val start: Coordinate = ptsClip.get(0)
      if (!start.equals2D(ptsClip.get(ptsClip.size - 1))) {
        ptsClip.add(start.copy)
      }
    }
    return ptsClip.toCoordinateArray
  }

  /**
   * Computes the intersection point of a segment with an edge of the clip box. The segment must be
   * known to intersect the edge.
   *
   * @param a
   *   first endpoint of the segment
   * @param b
   *   second endpoint of the segment
   * @param edgeIndex
   *   index of box edge
   * @return
   *   the intersection point with the box edge
   */
  private def intersection(a: Coordinate, b: Coordinate, edgeIndex: Int): Coordinate = {
    var intPt: Coordinate = null
    edgeIndex match {
      case RingClipper.BOX_BOTTOM =>
        intPt = new Coordinate(intersectionLineY(a, b, clipEnvMinY), clipEnvMinY)

      case RingClipper.BOX_RIGHT =>
        intPt = new Coordinate(clipEnvMaxX, intersectionLineX(a, b, clipEnvMaxX))

      case RingClipper.BOX_TOP =>
        intPt = new Coordinate(intersectionLineY(a, b, clipEnvMaxY), clipEnvMaxY)

      case _ =>
        intPt = new Coordinate(clipEnvMinX, intersectionLineX(a, b, clipEnvMinX))
    }
    return intPt
  }

  private def intersectionLineY(a: Coordinate, b: Coordinate, y: Double): Double = {
    val m: Double         = (b.x - a.x) / (b.y - a.y)
    val intercept: Double = (y - a.y) * m
    return a.x + intercept
  }

  private def intersectionLineX(a: Coordinate, b: Coordinate, x: Double): Double = {
    val m: Double         = (b.y - a.y) / (b.x - a.x)
    val intercept: Double = (x - a.x) * m
    return a.y + intercept
  }

  private def isInsideEdge(p: Coordinate, edgeIndex: Int): Boolean =
    edgeIndex match {
      case RingClipper.BOX_BOTTOM => // bottom
        p.y > clipEnvMinY

      case RingClipper.BOX_RIGHT => // right
        p.x < clipEnvMaxX

      case RingClipper.BOX_TOP => // top
        p.y < clipEnvMaxY

      case _ => // left
        p.x > clipEnvMinX
    }
}
