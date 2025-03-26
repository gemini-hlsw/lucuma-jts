// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.operation.overlayng

import org.locationtech.jts.geom.Dimension

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
 * Records topological information about an edge representing a piece of linework (lineString or
 * polygon ring) from a single source geometry. This information is carried through the noding
 * process (which may result in many noded edges sharing the same information object). It is then
 * used to populate the topology info fields in {@link Edge} s (possibly via merging). That
 * information is used to construct the topology graph {@link OverlayLabel} s.
 *
 * @author
 *   mdavis
 */
class EdgeSourceInfo(index: Int, depthDelta: Int, isHole0: Boolean, dim: Int = Dimension.A) {

  def this(index: Int) =
    this(index, 0, false, Dimension.L)

  def getIndex: Int = index

  def getDimension: Int = dim

  def getDepthDelta: Int = depthDelta

  def isHole: Boolean = isHole0

  override def toString: String = Edge.infoString(index, dim, isHole0, depthDelta)
}
