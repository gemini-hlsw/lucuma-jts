// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.operation.overlayng

import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.PrecisionModel
import org.locationtech.jts.precision.GeometryPrecisionReducer

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
 * Functions to reduce the precision of a geometry by rounding it to a given precision model. <p>
 * This class handles only polygonal and linear inputs. For full functionality see {@link
 * GeometryPrecisionReducer}.
 *
 * @see
 *   GeometryPrecisionReducer
 * @author
 *   Martin Davis
 */
object PrecisionReducer {

  /**
   * Reduces the precision of a geometry by rounding and snapping it to the supplied {@link
   * PrecisionModel}. The input geometry must be polygonal or linear. <p> The output is always a
   * valid geometry. This implies that input components may be merged if they are closer than the
   * grid precision. if merging is not desired, then the individual geometry components should be
   * processed separately. <p> The output is fully noded (i.e. coincident lines are merged and
   * noded). This provides an effective way to node / snap-round a collection of {@link
   * LineString}s.
   *
   * @param geom
   *   the geometry to reduce
   * @param pm
   *   the precision model to use
   * @return
   *   the precision-reduced geometry
   */
  def reducePrecision(geom: Geometry, pm: PrecisionModel): Geometry = {
    val ov = new OverlayNG(geom, pm)

    /**
     * Ensure reducing a area only produces polygonal result. (I.e. collapse lines are not output)
     */
    if (geom.getDimension == 2) ov.setAreaResultOnly(true)
    val reduced = ov.getResult
    reduced
  }
}

class PrecisionReducer private () // no instantiation for now
{}
