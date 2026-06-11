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
package org.locationtech.jts.index.hprtree

import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.shape.fractal.HilbertCode

class HilbertEncoder(private val level: Int, extent: Envelope) {
  private val hside   = Math.pow(2, level).toInt - 1
  private val minx    = extent.getMinX
  private val strideX = extent.getWidth / hside
  private val miny    = extent.getMinY
  private val strideY = extent.getHeight / hside

  def encode(env: Envelope): Int = {
    val midx = env.getWidth / 2 + env.getMinX
    val x    = ((midx - minx) / strideX).toInt

    val midy = env.getHeight / 2 + env.getMinY
    val y    = ((midy - miny) / strideY).toInt

    HilbertCode.encode(level, x, y)
  }
}
