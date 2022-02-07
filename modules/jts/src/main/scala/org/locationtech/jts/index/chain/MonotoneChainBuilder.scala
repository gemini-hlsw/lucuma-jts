// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
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
 */ /*
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
package org.locationtech.jts.index.chain

import java.util
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geomgraph.Quadrant

/**
 * Constructs {link MonotoneChain}s for sequences of {link Coordinate}s.
 *
 * @version 1.7
 */
object MonotoneChainBuilder {

  /**
   * Computes a list of the {link MonotoneChain}s for a list of coordinates.
   *
   * @param pts
   *   the list of points to compute chains for return a list of the monotone chains for the points
   */
  def getChains(pts: Array[Coordinate]): util.ArrayList[MonotoneChain] = getChains(pts, null)

  /**
   * Computes a list of the {link MonotoneChain}s for a list of coordinates, attaching a context
   * data object to each.
   *
   * @param pts
   *   the list of points to compute chains for
   * @param context
   *   a data object to attach to each chain return a list of the monotone chains for the points
   */
  def getChains(pts: Array[Coordinate], context: Any): util.ArrayList[MonotoneChain] = {
    val mcList     = new util.ArrayList[MonotoneChain]
    var chainStart = 0
    while ({
      {
        val chainEnd = findChainEnd(pts, chainStart)
        val mc       = new MonotoneChain(pts, chainStart, chainEnd, context)
        mcList.add(mc)
        chainStart = chainEnd
      }; (chainStart < pts.length - 1)
    }) ()
    mcList
  }

  /**
   * Finds the index of the last point in a monotone chain starting at a given point. Repeated
   * points (0-length segments) are included in the monotone chain returned.
   *
   * @param pts
   *   the points to scan
   * @param start
   *   the index of the start of this chain return the index of the last point in the monotone chain
   *   starting at <code>start</code>.
   */
  private def findChainEnd(pts: Array[Coordinate], start: Int): Int = {
    var safeStart = start
    // skip any zero-length segments at the start of the sequence
    // (since they cannot be used to establish a quadrant)
    while (safeStart < pts.length - 1 && pts(safeStart).equals2D(pts(safeStart + 1))) safeStart += 1
    // check if there are NO non-zero-length segments
    if (safeStart >= pts.length - 1) return pts.length - 1
    // determine overall quadrant for chain (which is the starting quadrant)
    val chainQuad = Quadrant.quadrant(pts(safeStart), pts(safeStart + 1))
    var last      = start + 1
    while (last < pts.length) { // skip zero-length segments, but include them in the chain
      if (!pts(last - 1).equals2D(pts(last))) { // compute quadrant for next possible segment in chain
        val quad = Quadrant.quadrant(pts(last - 1), pts(last))
        if (quad != chainQuad) {
//            last = pts.length
          return last - 1
        } // break
      }
      last += 1
    }
    last - 1
  }
}
