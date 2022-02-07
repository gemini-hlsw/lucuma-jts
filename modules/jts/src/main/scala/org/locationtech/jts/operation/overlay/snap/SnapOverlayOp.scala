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
package org.locationtech.jts.operation.overlay.snap

import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.operation.overlay.OverlayOp
import org.locationtech.jts.precision.CommonBitsRemover

/**
 * Performs an overlay operation using snapping and enhanced precision to improve the robustness of
 * the result. This class <i>always</i> uses snapping. This is less performant than the standard JTS
 * overlay code, and may even introduce errors which were not present in the original data. For this
 * reason, this class should only be used if the standard overlay code fails to produce a correct
 * result.
 *
 * @author
 *   Martin Davis
 * @version 1.7
 */
object SnapOverlayOp {
  def overlayOp(g0: Geometry, g1: Geometry, opCode: Int): Geometry = {
    val op = new SnapOverlayOp(g0, g1)
    op.getResultGeometry(opCode)
  }

  def intersection(g0: Geometry, g1: Geometry): Geometry = overlayOp(g0, g1, OverlayOp.INTERSECTION)

  def union(g0: Geometry, g1: Geometry): Geometry = overlayOp(g0, g1, OverlayOp.UNION)

  def difference(g0: Geometry, g1: Geometry): Geometry = overlayOp(g0, g1, OverlayOp.DIFFERENCE)

  def symDifference(g0: Geometry, g1: Geometry): Geometry =
    overlayOp(g0, g1, OverlayOp.SYMDIFFERENCE)
}

class SnapOverlayOp(val g1: Geometry, val g2: Geometry) {
  computeSnapTolerance()
  private val geom          = new Array[Geometry](2)
  geom(0) = g1
  geom(1) = g2
  private var snapTolerance = .0

  private def computeSnapTolerance(): Unit =
    snapTolerance = GeometrySnapper.computeOverlaySnapTolerance(geom(0), geom(1))
  // System.out.println("Snap tol = " + snapTolerance);

  def getResultGeometry(opCode: Int): Geometry = { //  	Geometry[] selfSnapGeom = new Geometry[] { selfSnap(geom[0]), selfSnap(geom[1])};
    val prepGeom = snap(geom)
    val result   = OverlayOp.overlayOp(prepGeom(0), prepGeom(1), opCode)
    prepareResult(result)
  }

//  private def selfSnap(geom: Geometry) = {
//    val snapper0 = new GeometrySnapper(geom)
//    val snapGeom = snapper0.snapTo(geom, snapTolerance)
//    //System.out.println("Self-snapped: " + snapGeom);
//    //System.out.println();
//    snapGeom
//  }

  private def snap(geom: Array[Geometry]) = {
    val remGeom  = removeCommonBits(geom)
    // MD - testing only
    //  	Geometry[] remGeom = geom;
    val snapGeom = GeometrySnapper.snap(remGeom(0), remGeom(1), snapTolerance)
    // MD - may want to do this at some point, but it adds cycles
    //    checkValid(snapGeom[0]);
    //    checkValid(snapGeom[1]);
    /*
        System.out.println("Snapped geoms: ");
        System.out.println(snapGeom[0]);
        System.out.println(snapGeom[1]);
     */
    snapGeom
  }

  private def prepareResult(geom: Geometry) = {
    cbr.addCommonBits(geom)
    geom
  }

  private var cbr: CommonBitsRemover = null

  private def removeCommonBits(geom: Array[Geometry]) = {
    cbr = new CommonBitsRemover
    cbr.add(geom(0))
    cbr.add(geom(1))
    val remGeom = new Array[Geometry](2)
    remGeom(0) = cbr.removeCommonBits(geom(0).copy)
    remGeom(1) = cbr.removeCommonBits(geom(1).copy)
    remGeom
  }

//  private def checkValid(g: Geometry): Unit = if (!g.isValid) System.out.println("Snapped geometry is invalid")
}
