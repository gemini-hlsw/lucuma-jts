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
 */
package org.locationtech.jts.geomgraph.index

import org.locationtech.jts.geomgraph.Edge

/**
 * @version 1.7
 */
class SweepLineSegment(var edge: Edge, var ptIndex: Int) {
  private[index] var pts = edge.getCoordinates

  def getMinX: Double = {
    val x1 = pts(ptIndex).x
    val x2 = pts(ptIndex + 1).x
    if (x1 < x2) x1
    else x2
  }

  def getMaxX: Double = {
    val x1 = pts(ptIndex).x
    val x2 = pts(ptIndex + 1).x
    if (x1 > x2) x1
    else x2
  }

  def computeIntersections(ss: SweepLineSegment, si: SegmentIntersector): Unit =
    si.addIntersections(edge, ptIndex, ss.edge, ss.ptIndex)
}
