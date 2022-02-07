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

import java.util
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateSequence
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.Polygonal
import org.locationtech.jts.geom.PrecisionModel
import org.locationtech.jts.geom.util.GeometryTransformer

/**
 * Snaps the vertices and segments of a {link Geometry} to another Geometry's vertices. A snap
 * distance tolerance is used to control where snapping is performed. Snapping one geometry to
 * another can improve robustness for overlay operations by eliminating nearly-coincident edges
 * (which cause problems during noding and intersection calculation). It can also be used to
 * eliminate artifacts such as narrow slivers, spikes and gores. <p> Too much snapping can result in
 * invalid topology being created, so the number and location of snapped vertices is decided using
 * heuristics to determine when it is safe to snap. This can result in some potential snaps being
 * omitted, however.
 *
 * @author
 *   Martin Davis
 * @version 1.7
 */
object GeometrySnapper {
  private val SNAP_PRECISION_FACTOR = 1e-9

  /**
   * Estimates the snap tolerance for a Geometry, taking into account its precision model.
   *
   * @param g
   *   a Geometry return the estimated snap tolerance
   */
  def computeOverlaySnapTolerance(g: Geometry): Double = {
    var snapTolerance = computeSizeBasedSnapTolerance(g)

    /**
     * Overlay is carried out in the precision model of the two inputs. If this precision model is
     * of type FIXED, then the snap tolerance must reflect the precision grid size. Specifically,
     * the snap tolerance should be at least the distance from a corner of a precision grid cell to
     * the centre point of the cell.
     */
    val pm = g.getPrecisionModel
    if (pm.getType eq PrecisionModel.FIXED) {
      val fixedSnapTol = (1 / pm.getScale) * 2 / 1.415
      if (fixedSnapTol > snapTolerance) snapTolerance = fixedSnapTol
    }
    snapTolerance
  }

  def computeSizeBasedSnapTolerance(g: Geometry): Double = {
    val env          = g.getEnvelopeInternal
    val minDimension = Math.min(env.getHeight, env.getWidth)
    val snapTol      = minDimension * SNAP_PRECISION_FACTOR
    snapTol
  }

  def computeOverlaySnapTolerance(g0: Geometry, g1: Geometry): Double =
    Math.min(computeOverlaySnapTolerance(g0), computeOverlaySnapTolerance(g1))

  /**
   * Snaps two geometries together with a given tolerance.
   *
   * @param g0
   *   a geometry to snap
   * @param g1
   *   a geometry to snap
   * @param snapTolerance
   *   the tolerance to use return the snapped geometries
   */
  def snap(g0: Geometry, g1: Geometry, snapTolerance: Double): Array[Geometry] = {
    val snapGeom = new Array[Geometry](2)
    val snapper0 = new GeometrySnapper(g0)
    snapGeom(0) = snapper0.snapTo(g1, snapTolerance)

    /**
     * Snap the second geometry to the snapped first geometry (this strategy minimizes the number of
     * possible different points in the result)
     */
    val snapper1 = new GeometrySnapper(g1)
    snapGeom(1) = snapper1.snapTo(snapGeom(0), snapTolerance)
    //    System.out.println(snap[0]);
    //    System.out.println(snap[1]);
    snapGeom
  }

  /**
   * Snaps a geometry to itself. Allows optionally cleaning the result to ensure it is topologically
   * valid (which fixes issues such as topology collapses in polygonal inputs). <p> Snapping a
   * geometry to itself can remove artifacts such as very narrow slivers, gores and spikes.
   *
   * @param geom
   *   the geometry to snap
   * @param snapTolerance
   *   the snapping tolerance
   * @param cleanResult
   *   whether the result should be made valid return a new snapped Geometry
   */
  def snapToSelf(geom: Geometry, snapTolerance: Double, cleanResult: Boolean): Geometry = {
    val snapper0 = new GeometrySnapper(geom)
    snapper0.snapToSelf(snapTolerance, cleanResult)
  }
}

class GeometrySnapper(var srcGeom: Geometry) {

  /**
   * Creates a new snapper acting on the given geometry
   *
   * @param srcGeom
   *   the geometry to snap
   */
  // /**
  //  * Snaps the vertices in the component {link LineString}s
  //  * of the source geometry
  //  * to the vertices of the given snap geometry.
  //  *
  //  * @param snapGeom a geometry to snap the source to
  //  * return a new snapped Geometry
  //  */
  def snapTo(snapGeom: Geometry, snapTolerance: Double): Geometry = {
    val snapPts   = extractTargetCoordinates(snapGeom)
    val snapTrans = new SnapTransformer(snapTolerance, snapPts)
    snapTrans.transform(srcGeom)
  }

  // /**
  //  * Snaps the vertices in the component {link LineString}s
  //  * of the source geometry
  //  * to the vertices of the same geometry.
  //  * Allows optionally cleaning the result to ensure it is
  //  * topologically valid
  //  * (which fixes issues such as topology collapses in polygonal inputs).
  //  *
  //  * @param snapTolerance the snapping tolerance
  //  * @param cleanResult   whether the result should be made valid
  //  * return a new snapped Geometry
  //  */
  def snapToSelf(snapTolerance: Double, cleanResult: Boolean): Geometry = {
    val snapPts     = extractTargetCoordinates(srcGeom)
    val snapTrans   = new SnapTransformer(snapTolerance, snapPts, true)
    val snappedGeom = snapTrans.transform(srcGeom)
    var result      = snappedGeom
    if (cleanResult && result.isInstanceOf[Polygonal]) { // TODO: use better cleaning approach
      result = snappedGeom.buffer(0)
    }
    result
  }

  private def extractTargetCoordinates(g: Geometry) = { // TODO: should do this more efficiently.  Use CoordSeq filter to get points, KDTree for uniqueness & queries
    val ptSet = new util.TreeSet[Coordinate]
    val pts   = g.getCoordinates
    var i     = 0
    while (i < pts.length) {
      ptSet.add(pts(i))
      i += 1
    }
    ptSet.toArray(new Array[Coordinate](0))
  }

  // /**
  //  * Computes the snap tolerance based on the input geometries.
  //  *
  //  * @param ringPts
  //  * return
  //  */
//  private def computeSnapTolerance(ringPts: Array[Coordinate]) = {
//    val minSegLen = computeMinimumSegmentLength(ringPts)
//    // use a small percentage of this to be safe
//    val snapTol = minSegLen / 10
//    snapTol
//  }

//  private def computeMinimumSegmentLength(pts: Array[Coordinate]) = {
//    var minSegLen = Double.MaxValue
//    var i = 0
//    while ( {
//      i < pts.length - 1
//    }) {
//      val segLen = pts(i).distance(pts(i + 1))
//      if (segLen < minSegLen) minSegLen = segLen
//      i += 1
//    }
//    minSegLen
//  }
}

class SnapTransformer(snapTolerance: Double, snapPts: Array[Coordinate], isSelfSnap: Boolean)
    extends GeometryTransformer {
//  private var snapTolerance = .0
//  private var snapPts: Array[Coordinate] = null
//  private var isSelfSnap = false

  def this(snapTolerance: Double, snapPts: Array[Coordinate]) =
    this(snapTolerance, snapPts, false)
//    this.snapTolerance = snapTolerance
//    this.snapPts = snapPts

//  def this {
//    this()
//    this.snapTolerance = snapTolerance
//    this.snapPts = snapPts
//    this.isSelfSnap = isSelfSnap
//  }

  override protected def transformCoordinates(
    coords: CoordinateSequence,
    parent: Geometry
  ): CoordinateSequence = {
    val srcPts = coords.toCoordinateArray
    val newPts = snapLine(srcPts, snapPts)
    factory.getCoordinateSequenceFactory.create(newPts)
  }

  private def snapLine(srcPts: Array[Coordinate], snapPts: Array[Coordinate]) = {
    val snapper = new LineStringSnapper(srcPts, snapTolerance)
    snapper.setAllowSnappingToSourceVertices(isSelfSnap)
    snapper.snapTo(snapPts)
  }
}
