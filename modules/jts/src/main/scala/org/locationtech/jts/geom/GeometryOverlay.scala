// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.geom

import org.locationtech.jts.geom.util.GeometryCollectionMapper
import org.locationtech.jts.geom.util.GeometryMapper
import org.locationtech.jts.operation.overlay.OverlayOp
import org.locationtech.jts.operation.overlay.snap.SnapIfNeededOverlayOp
import org.locationtech.jts.operation.overlayng.OverlayNGRobust
import org.locationtech.jts.operation.union.UnaryUnionOp

/*
 * Copyright (c) 2020 Martin Davis.
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
 * Internal class which encapsulates the runtime switch to use OverlayNG, and some additional
 * extensions for optimization and GeometryCollection handling. <p> This class allows the {@link
 * Geometry} overlay methods to be switched between the original algorithm and the modern OverlayNG
 * codebase via a system property <code>jts.overlay</code>. <ul> <li><code>jts.overlay=old</code> -
 * (default) use original overlay algorithm <li><code>jts.overlay=ng</code> - use OverlayNG </ul>
 *
 * @author
 *   mdavis
 */
object GeometryOverlay {
  var OVERLAY_PROPERTY_NAME      = "jts.overlay"
  var OVERLAY_PROPERTY_VALUE_NG  = "ng"
  var OVERLAY_PROPERTY_VALUE_OLD = "old"

  /**
   * Currently the original JTS overlay implementation is the default
   */
  var OVERLAY_NG_DEFAULT  = false
  private var isOverlayNG = OVERLAY_NG_DEFAULT

  /**
   * This function is provided primarily for unit testing. It is not recommended to use it
   * dynamically, since that may result in inconsistent overlay behaviour.
   *
   * @param overlayImplCode
   *   the code for the overlay method (may be null)
   */
  private[geom] def setOverlayImpl(overlayImplCode: String): Unit = {
    if (overlayImplCode == null) return
    // set flag explicitly since current value may not be default
    isOverlayNG = OVERLAY_NG_DEFAULT
    if (OVERLAY_PROPERTY_VALUE_NG.equalsIgnoreCase(overlayImplCode)) isOverlayNG = true
  }

  private def overlay(a: Geometry, b: Geometry, opCode: Int) = if (isOverlayNG)
    OverlayNGRobust.overlay(a, b, opCode)
  else SnapIfNeededOverlayOp.overlayOp(a, b, opCode)

  private[geom] def difference(a: Geometry, b: Geometry): Geometry = { // special case: if A.isEmpty ==> empty; if B.isEmpty ==> A
    if (a.isEmpty) return OverlayOp.createEmptyResult(OverlayOp.DIFFERENCE, a, b, a.getFactory)
    if (b.isEmpty) return a.copy
    Geometry.checkNotGeometryCollection(a)
    Geometry.checkNotGeometryCollection(b)
    overlay(a, b, OverlayOp.DIFFERENCE)
  }

  private[geom] def intersection(a: Geometry, b: Geometry): Geometry = {

    /**
     * TODO: MD - add optimization for P-A case using Point-In-Polygon
     */
    // special case: if one input is empty ==> empty
    if (a.isEmpty || b.isEmpty)
      return OverlayOp.createEmptyResult(OverlayOp.INTERSECTION, a, b, a.getFactory)
    // compute for GCs
    // (An inefficient algorithm, but will work)
    // TODO: improve efficiency of computation for GCs
    if (a.isGeometryCollection) {
      val g2 = b
      return GeometryCollectionMapper.map(a.asInstanceOf[GeometryCollection],
                                          new GeometryMapper.MapOp() {
                                            override def map(g: Geometry): Geometry =
                                              g.intersection(g2)
                                          }
      )
    }
    // No longer needed since GCs are handled by previous code
    // checkNotGeometryCollection(this);
    // checkNotGeometryCollection(other);
    overlay(a, b, OverlayOp.INTERSECTION)
  }

  private[geom] def symDifference(a: Geometry, b: Geometry): Geometry = { // handle empty geometry cases
    if (a.isEmpty || b.isEmpty) { // both empty - check dimensions
      if (a.isEmpty && b.isEmpty)
        return OverlayOp.createEmptyResult(OverlayOp.SYMDIFFERENCE, a, b, a.getFactory)
      // special case: if either input is empty ==> result = other arg
      if (a.isEmpty) return b.copy
      if (b.isEmpty) return a.copy
    }
    Geometry.checkNotGeometryCollection(a)
    Geometry.checkNotGeometryCollection(b)
    overlay(a, b, OverlayOp.SYMDIFFERENCE)
  }

  private[geom] def union(a: Geometry, b: Geometry): Geometry = {
    if (a.isEmpty || a.isEmpty) {
      if (b.isEmpty && b.isEmpty)
        return OverlayOp.createEmptyResult(OverlayOp.UNION, a, b, a.getFactory)
      // special case: if either input is empty ==> other input
      if (a.isEmpty) return b.copy
      if (b.isEmpty) return a.copy
    }
    // TODO: optimize if envelopes of geometries do not intersect
    Geometry.checkNotGeometryCollection(a)
    Geometry.checkNotGeometryCollection(b)
    overlay(a, b, OverlayOp.UNION)
  }

  private[geom] def union(a: Geometry) = if (isOverlayNG) OverlayNGRobust.union(a)
  else UnaryUnionOp.union(a)

  setOverlayImpl(System.getProperty(OVERLAY_PROPERTY_NAME))

}
