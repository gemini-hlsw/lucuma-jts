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
package org.locationtech.jts.noding

import org.locationtech.jts.geom.Coordinate

/**
 * An interface for classes which support adding nodes to a segment string.
 *
 * @author
 *   Martin Davis
 */
trait NodableSegmentString extends SegmentString {

  /**
   * Adds an intersection node for a given point and segment to this segment string.
   *
   * @param intPt
   *   the location of the intersection
   * @param segmentIndex
   *   the index of the segment containing the intersection
   */
  def addIntersection(intPt: Coordinate, segmentIndex: Int): Unit
}
