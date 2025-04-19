// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
package org.locationtech.jts.index.chain

import org.locationtech.jts.geom.LineSegment

/**
 * The action for the internal iterator for performing overlap queries on a MonotoneChain
 *
 * @version 1.7
 */
class MonotoneChainOverlapAction {
  protected var overlapSeg1 = new LineSegment
  protected var overlapSeg2 = new LineSegment

  /**
   * This function can be overridden if the original chains are needed
   *
   * @param start1
   *   the index of the start of the overlapping segment from mc1
   * @param start2
   *   the index of the start of the overlapping segment from mc2
   */
  def overlap(mc1: MonotoneChain, start1: Int, mc2: MonotoneChain, start2: Int): Unit = {
    mc1.getLineSegment(start1, overlapSeg1)
    mc2.getLineSegment(start2, overlapSeg2)
    overlap(overlapSeg1, overlapSeg2)
  }

  /**
   * This is a convenience function which can be overridden to obtain the actual line segments which
   * overlap
   *
   * @param seg1
   * @param seg2
   */
  def overlap(seg1: LineSegment, seg2: LineSegment): Unit = {}
}
