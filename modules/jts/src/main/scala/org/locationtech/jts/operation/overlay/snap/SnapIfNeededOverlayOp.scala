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
package org.locationtech.jts.operation.overlay.snap

import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.operation.overlay.OverlayOp

/**
 * Performs an overlay operation using snapping and enhanced precision to improve the robustness of
 * the result. This class only uses snapping if an error is detected when running the standard JTS
 * overlay code. Errors detected include thrown exceptions (in particular, {link TopologyException})
 * and invalid overlay computations.
 *
 * @author
 *   Martin Davis
 * @version 1.7
 */
object SnapIfNeededOverlayOp {
  def overlayOp(g0: Geometry, g1: Geometry, opCode: Int): Geometry = {
    val op = new SnapIfNeededOverlayOp(g0, g1)
    op.getResultGeometry(opCode)
  }

  def intersection(g0: Geometry, g1: Geometry): Geometry = overlayOp(g0, g1, OverlayOp.INTERSECTION)

  def union(g0: Geometry, g1: Geometry): Geometry = overlayOp(g0, g1, OverlayOp.UNION)

  def difference(g0: Geometry, g1: Geometry): Geometry = overlayOp(g0, g1, OverlayOp.DIFFERENCE)

  def symDifference(g0: Geometry, g1: Geometry): Geometry =
    overlayOp(g0, g1, OverlayOp.SYMDIFFERENCE)
}

class SnapIfNeededOverlayOp(val g1: Geometry, val g2: Geometry) {
  private val geom = Array[Geometry](g1, g2)

  def getResultGeometry(opCode: Int): Geometry = {
    var result: Geometry                 = null
    var isSuccess                        = false
    var savedException: RuntimeException = null
    try { // try basic operation with input geometries
      result = OverlayOp.overlayOp(geom(0), geom(1), opCode)
      val isValid = true
      // not needed if noding validation is used
      //      boolean isValid = OverlayResultValidator.isValid(geom[0], geom[1], OverlayOp.INTERSECTION, result);
      if (isValid) isSuccess = true
    } catch {
      case ex: RuntimeException =>
        savedException = ex
      // ignore this exception, since the operation will be rerun
      //    	System.out.println(ex.getMessage());
      //    	ex.printStackTrace();
      // System.out.println(ex.getMessage());
      // System.out.println("Geom 0: " + geom[0]);
      // System.out.println("Geom 1: " + geom[1]);
    }
    if (!isSuccess) { // this may still throw an exception
      // if so, throw the original exception since it has the input coordinates
      try result = SnapOverlayOp.overlayOp(geom(0), geom(1), opCode)
      catch {
        case _: RuntimeException =>
          throw savedException
      }
    }
    result
  }
}
