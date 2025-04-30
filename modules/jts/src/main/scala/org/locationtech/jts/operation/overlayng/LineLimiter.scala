// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

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
package org.locationtech.jts.operation.overlayng

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

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateList
import org.locationtech.jts.geom.Envelope

import java.util

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
 * Limits the segments in a list of segments to those which intersect an envelope. This creates zero
 * or more sections of the input segment sequences, containing only line segments which intersect
 * the limit envelope. Segments are not clipped, since that can move line segments enough to alter
 * topology, and it happens in the overlay in any case. This can substantially reduce the number of
 * vertices which need to be processed during overlay. <p> This optimization is only applicable to
 * Line geometries, since it does not maintain the closed topology of rings. Polygonal geometries
 * are optimized using the {@link RingClipper} .
 *
 * @author
 *   Martin Davis
 * @see
 *   RingClipper
 */
class LineLimiter(var limitEnv: Envelope) {

  /**
   * Creates a new limiter for a given envelope.
   *
   * @param env
   *   the envelope to limit to
   */
  private var ptList: CoordinateList                 = null
  private var lastOutside: Coordinate                = null
  private var sections: util.List[Array[Coordinate]] = null

  /**
   * Limits a list of segments.
   *
   * @param pts
   *   the segment sequence to limit
   * @return
   *   the sections which intersect the limit envelope
   */
  def limit(pts: Array[Coordinate]): util.List[Array[Coordinate]] = {
    lastOutside = null
    ptList = null
    sections = new util.ArrayList[Array[Coordinate]]
    for (i <- 0 until pts.length) {
      val p: Coordinate = pts(i)
      if (limitEnv.intersects(p)) {
        addPoint(p)
      } else {
        addOutside(p)
      }
    }
    // finish last section, if any
    finishSection()
    return sections
  }

  private def addPoint(p: Coordinate): Unit = {
    if (p == null) {
      return
    }
    startSection()
    ptList.add(p, false)
  }

  private def addOutside(p: Coordinate): Unit = {
    val segIntersects: Boolean = isLastSegmentIntersecting(p)
    if (!segIntersects) {
      finishSection()
    } else {
      addPoint(lastOutside)
      addPoint(p)
    }
    lastOutside = p
  }

  private def isLastSegmentIntersecting(p: Coordinate): Boolean = {
    if (lastOutside == null) { // last point must have been inside
      if (isSectionOpen) {
        return true
      }
      return false
    }
    return limitEnv.intersects(lastOutside, p)
  }

  private def isSectionOpen: Boolean =
    return ptList != null

  private def startSection(): Unit = {
    if (ptList == null) {
      ptList = new CoordinateList
    }
    if (lastOutside != null) {
      ptList.add(lastOutside, false)
    }
    lastOutside = null
  }

  private def finishSection(): Unit = {
    if (ptList == null) {
      return
    }
    // finish off this section
    if (lastOutside != null) {
      ptList.add(lastOutside, false)
      lastOutside = null
    }
    val section: Array[Coordinate] = ptList.toCoordinateArray
    sections.add(section)
    ptList = null
  }
}
