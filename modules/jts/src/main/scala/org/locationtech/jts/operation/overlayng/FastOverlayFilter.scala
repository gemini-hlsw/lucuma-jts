// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.operation.overlayng

import org.locationtech.jts.geom.Geometry

class FastOverlayFilter( // superceded by overlap clipping?
  // TODO: perhaps change this to RectangleClipping, with fast/looser semantics?
  var targetGeom: Geometry
) {
  isTargetRectangle = targetGeom.isRectangle
  private var isTargetRectangle = false

  /**
   * Computes the overlay operation on the input geometries, if it can be determined that the result
   * is either empty or equal to one of the input values. Otherwise <code>null</code> is returned,
   * indicating that a full overlay operation must be performed.
   *
   * @param geom
   * @param overlayOpCode
   * @return
   */
  def overlay(geom: Geometry, overlayOpCode: Int): Geometry = { // for now only INTERSECTION is handled
    if (overlayOpCode != OverlayNG.INTERSECTION) return null
    intersection(geom)
  }

  private def intersection(geom: Geometry): Geometry = { // handle rectangle case
    val resultForRect = intersectionRectangle(geom)
    if (resultForRect != null) return resultForRect
    // handle general case
    if (!isEnvelopeIntersects(targetGeom, geom)) return createEmpty(geom)
    null
  }

  private def createEmpty(geom: Geometry) = // empty result has dimension of non-rectangle input
    OverlayUtil.createEmptyResult(geom.getDimension, geom.getFactory)

  private def intersectionRectangle(geom: Geometry): Geometry = {
    if (!isTargetRectangle) return null
    if (isEnvelopeCovers(targetGeom, geom)) return geom.copy
    if (!isEnvelopeIntersects(targetGeom, geom)) return createEmpty(geom)
    null
  }

  private def isEnvelopeIntersects(a: Geometry, b: Geometry) =
    a.getEnvelopeInternal.intersects(b.getEnvelopeInternal)

  private def isEnvelopeCovers(a: Geometry, b: Geometry) =
    a.getEnvelopeInternal.covers(b.getEnvelopeInternal)
}
