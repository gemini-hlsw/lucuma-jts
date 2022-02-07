// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2019 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */ // Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2019 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.index.strtree

import org.locationtech.jts.geom.Envelope

/**
 * Functions for computing distances between {link Envelope}s.
 *
 * @author
 *   mdavis
 */
object EnvelopeDistance {

  /**
   * Computes the maximum distance between the points defining two envelopes. It is equal to the
   * length of the diagonal of the envelope containing both input envelopes. This is a coarse upper
   * bound on the distance between geometries bounded by the envelopes.
   *
   * @param env1
   *   an envelope
   * @param env2
   *   an envelope return the maximum distance between the points defining the envelopes
   */
  def maximumDistance(env1: Envelope, env2: Envelope): Double = {
    val minx = Math.min(env1.getMinX, env2.getMinX)
    val miny = Math.min(env1.getMinY, env2.getMinY)
    val maxx = Math.max(env1.getMaxX, env2.getMaxX)
    val maxy = Math.max(env1.getMaxY, env2.getMaxY)
    distance(minx, miny, maxx, maxy)
  }

  private def distance(x1: Double, y1: Double, x2: Double, y2: Double) = {
    val dx = x2 - x1
    val dy = y2 - y1
    Math.sqrt(dx * dx + dy * dy)
  }

  /**
   * Computes the Min-Max Distance between two {link Envelope}s. It is equal to the minimum of the
   * maximum distances between all pairs of edge segments from the two envelopes. This is the tight
   * upper bound on the distance between geometric items bounded by the envelopes. <p> Theoretically
   * this bound can be used in the R-tree nearest-neighbour branch-and-bound search instead of {link
   * #maximumDistance(Envelope, Envelope)}. However, little performance improvement is observed in
   * practice.
   *
   * @param a
   *   an envelope
   * @param b
   *   an envelope return the min-max-distance between the envelopes
   */
  def minMaxDistance(a: Envelope, b: Envelope): Double = {
    val aminx = a.getMinX
    val aminy = a.getMinY
    val amaxx = a.getMaxX
    val amaxy = a.getMaxY
    val bminx = b.getMinX
    val bminy = b.getMinY
    val bmaxx = b.getMaxX
    val bmaxy = b.getMaxY
    var dist  = maxDistance(aminx, aminy, aminx, amaxy, bminx, bminy, bminx, bmaxy)
    dist = Math.min(dist, maxDistance(aminx, aminy, aminx, amaxy, bminx, bminy, bmaxx, bminy))
    dist = Math.min(dist, maxDistance(aminx, aminy, aminx, amaxy, bmaxx, bmaxy, bminx, bmaxy))
    dist = Math.min(dist, maxDistance(aminx, aminy, aminx, amaxy, bmaxx, bmaxy, bmaxx, bminy))
    dist = Math.min(dist, maxDistance(aminx, aminy, amaxx, aminy, bminx, bminy, bminx, bmaxy))
    dist = Math.min(dist, maxDistance(aminx, aminy, amaxx, aminy, bminx, bminy, bmaxx, bminy))
    dist = Math.min(dist, maxDistance(aminx, aminy, amaxx, aminy, bmaxx, bmaxy, bminx, bmaxy))
    dist = Math.min(dist, maxDistance(aminx, aminy, amaxx, aminy, bmaxx, bmaxy, bmaxx, bminy))
    dist = Math.min(dist, maxDistance(amaxx, amaxy, aminx, amaxy, bminx, bminy, bminx, bmaxy))
    dist = Math.min(dist, maxDistance(amaxx, amaxy, aminx, amaxy, bminx, bminy, bmaxx, bminy))
    dist = Math.min(dist, maxDistance(amaxx, amaxy, aminx, amaxy, bmaxx, bmaxy, bminx, bmaxy))
    dist = Math.min(dist, maxDistance(amaxx, amaxy, aminx, amaxy, bmaxx, bmaxy, bmaxx, bminy))
    dist = Math.min(dist, maxDistance(amaxx, amaxy, amaxx, aminy, bminx, bminy, bminx, bmaxy))
    dist = Math.min(dist, maxDistance(amaxx, amaxy, amaxx, aminy, bminx, bminy, bmaxx, bminy))
    dist = Math.min(dist, maxDistance(amaxx, amaxy, amaxx, aminy, bmaxx, bmaxy, bminx, bmaxy))
    dist = Math.min(dist, maxDistance(amaxx, amaxy, amaxx, aminy, bmaxx, bmaxy, bmaxx, bminy))
    dist
  }

  /**
   * Computes the maximum distance between two line segments.
   *
   * @param ax1
   *   x ordinate of first endpoint of segment 1
   * @param ay1
   *   y ordinate of first endpoint of segment 1
   * @param ax2
   *   x ordinate of second endpoint of segment 1
   * @param ay2
   *   y ordinate of second endpoint of segment 1
   * @param bx1
   *   x ordinate of first endpoint of segment 2
   * @param by1
   *   y ordinate of first endpoint of segment 2
   * @param bx2
   *   x ordinate of second endpoint of segment 2
   * @param by2
   *   y ordinate of second endpoint of segment 2 return maximum distance between the segments
   */
  private def maxDistance(
    ax1: Double,
    ay1: Double,
    ax2: Double,
    ay2: Double,
    bx1: Double,
    by1: Double,
    bx2: Double,
    by2: Double
  ) = {
    var dist = distance(ax1, ay1, bx1, by1)
    dist = Math.max(dist, distance(ax1, ay1, bx2, by2))
    dist = Math.max(dist, distance(ax2, ay2, bx1, by1))
    dist = Math.max(dist, distance(ax2, ay2, bx2, by2))
    dist
  }
}
