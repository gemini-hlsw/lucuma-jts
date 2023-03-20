// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.operation.overlayng

import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.PrecisionModel
import org.locationtech.jts.operation.overlayng.OverlayNG.UNION
import org.locationtech.jts.operation.union.UnaryUnionOp
import org.locationtech.jts.operation.union.UnionStrategy

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
 * Unions a collection of geometries in an efficient way, using {@link OverlayNG} to ensure robust
 * computation.
 *
 * @author
 *   Martin Davis
 */
object UnaryUnionNG {

  /**
   * Unions a collection of geometries using a given precision model.
   *
   * @param geom
   *   the geometry to union
   * @param pm
   *   the precision model to use
   * @return
   *   the union of the geometries
   */
  def union(geom: Geometry, pm: PrecisionModel): Geometry = {
    val unionSRFun = new UnionStrategy() {
      override def union(g0: Geometry, g1: Geometry): Geometry =
        OverlayNG.overlay(g0, g1, UNION, pm)

      override def isFloatingPrecision(): Boolean = OverlayUtil.isFloating(pm)
    }
    val op         = new UnaryUnionOp(geom)
    op.setUnionFunction(unionSRFun)
    op.union
  }
}

class UnaryUnionNG private () // no instantiation for now
{}
