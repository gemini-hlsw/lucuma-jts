// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.operation.overlayng

import org.locationtech.jts.algorithm.PointLocator
import org.locationtech.jts.algorithm.locate.PointOnGeometryLocator
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Geometry

/**
 * Locates points on a linear geometry, using a spatial index to provide good performance.
 *
 * @author
 *   mdavis
 */
class IndexedPointOnLineLocator(var inputGeom: Geometry) extends PointOnGeometryLocator {
  override def locate(p: Coordinate): Int = { // TODO: optimize this with a segment index
    val locator = new PointLocator
    locator.locate(p, inputGeom)
  }
}
