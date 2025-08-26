// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.operation.overlayng

import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.PrecisionModel
import org.locationtech.jts.geom.TopologyException
import org.locationtech.jts.noding.snap.SnappingNoder
import org.locationtech.jts.operation.union.UnaryUnionOp
import org.locationtech.jts.operation.union.UnionStrategy

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
 * Performs an overlay operation using {@link OverlayNG} , providing full robustness by using a
 * series of increasingly robust (but slower) noding strategies. <p> The noding strategies used are:
 * <ol> <li>A simple, fast noder using FLOATING precision. <li>A {@link SnappingNoder} using an
 * automatically-determined snap tolerance <li>First snapping each geometry to itself, and then
 * overlaying them using a <code>SnappingNoder</code>. <li>The above two strategies are repeated
 * with increasing snap tolerance, up to a limit. <li>Finally a {@link SnapRoundngNoder} is used
 * with a automatically-determined scale factor intended to preserve input precision while still
 * preventing robustness problems. </ol> If all of the above attempts fail to compute a valid
 * overlay, the original {@link TopologyException} is thrown. In practice this is extremely unlikely
 * to occur. <p> This algorithm relies on each overlay operation execution throwing a {@link
 * TopologyException} if it is unable to compute the overlay correctly. Generally this occurs
 * because the noding phase does not produce a valid noding. This requires the use of a {@link
 * ValidatingNoder} in order to check the results of using a floating noder.
 *
 * @author
 *   Martin Davis
 * @see
 *   OverlayNG
 */
object OverlayNGRobust {
  def union(a: Geometry): Geometry = {
    val unionSRFun = new UnionStrategy() {
      override def union(g0: Geometry, g1: Geometry): Geometry = overlay(g0, g1, OverlayNG.UNION)

      override def isFloatingPrecision() = true
    }
    val op         = new UnaryUnionOp(a)
    op.setUnionFunction(unionSRFun)
    op.union
  }

  /**
   * Overlay two geometries, using heuristics to ensure computation completes correctly. In practice
   * the heuristics are observed to be fully correct.
   *
   * @param geom0
   *   a geometry
   * @param geom1
   *   a geometry
   * @param opCode
   *   the overlay operation code (from {@link OverlayNG}
   * @return
   *   the overlay result geometry
   * @see
   *   OverlayNG
   */
  def overlay(geom0: Geometry, geom1: Geometry, opCode: Int): Geometry = {
    var result: Geometry             = null
    var exOriginal: RuntimeException = null

    /**
     * First try overlay with a FLOAT noder, which is fast and causes least change to geometry
     * coordinates By default the noder is validated, which is required in order to detect certain
     * invalid noding situations which otherwise cause incorrect overlay output.
     */
    try {
      result = OverlayNG.overlay(geom0, geom1, opCode)
      return result
    } catch {
      case ex: RuntimeException =>
        /**
         * Capture original exception, so it can be rethrown if the remaining strategies all fail.
         */
        exOriginal = ex
    }

    /**
     * On failure retry using snapping noding with a "safe" tolerance. if this throws an exception
     * just let it go, since it is something that is not a TopologyException
     */
    result = overlaySnapTries(geom0, geom1, opCode)
    if (result != null) return result

    /**
     * On failure retry using snap-rounding with a heuristic scale factor (grid size).
     */
    result = overlaySR(geom0, geom1, opCode)
    if (result != null) return result

    /**
     * Just can't get overlay to work, so throw original error.
     */
    throw exOriginal
  }

  private val NUM_SNAP_TRIES = 5

  /**
   * Attempt overlay using snapping with repeated tries with increasing snap tolerances.
   *
   * @param geom0
   * @param geom1
   * @param opCode
   * @return
   *   the computed overlay result, or null if the overlay fails
   */
  private def overlaySnapTries(geom0: Geometry, geom1: Geometry, opCode: Int): Geometry = {
    var result: Geometry = null
    var snapTol          = snapTolerance(geom0, geom1)
    for (i <- 0 until NUM_SNAP_TRIES) {
      result = overlaySnapping(geom0, geom1, opCode, snapTol)
      if (result != null) return result

      /**
       * Now try snapping each input individually, and then doing the overlay.
       */
      result = overlaySnapBoth(geom0, geom1, opCode, snapTol)
      if (result != null) return result
      // increase the snap tolerance and try again
      snapTol = snapTol * 10
    }
    // failed to compute overlay
    null
  }

  /**
   * Attempt overlay using a {@link SnappingNoder} .
   *
   * @param geom0
   * @param geom1
   * @param opCode
   * @param snapTol
   * @return
   *   the computed overlay result, or null if the overlay fails
   */
  private def overlaySnapping(
    geom0:   Geometry,
    geom1:   Geometry,
    opCode:  Int,
    snapTol: Double
  ): Geometry = {
    try return overlaySnapTol(geom0, geom1, opCode, snapTol)
    catch {
      case _: TopologyException =>

      // ---- ignore exception, return null result to indicate failure
      // System.out.println("Snapping with " + snapTol + " - FAILED");
      // log("Snapping with " + snapTol + " - FAILED", geom0, geom1);
    }
    null
  }

  /**
   * Attempt overlay with first snapping each geometry individually.
   *
   * @param geom0
   * @param geom1
   * @param opCode
   * @param snapTol
   * @return
   *   the computed overlay result, or null if the overlay fails
   */
  private def overlaySnapBoth(
    geom0:   Geometry,
    geom1:   Geometry,
    opCode:  Int,
    snapTol: Double
  ): Geometry = {
    try {
      val snap0 = snapSelf(geom0, snapTol)
      val snap1 = snapSelf(geom1, snapTol)
      // log("Snapping BOTH with " + snapTol, geom0, geom1);
      return overlaySnapTol(snap0, snap1, opCode, snapTol)
    } catch {
      case _: TopologyException =>

    }
    null
  }

  /**
   * Self-snaps a geometry by running a union operation with it as the only input. This helps to
   * remove narrow spike/gore artifacts to simplify the geometry, which improves robustness.
   * Collapsed artifacts are removed from the result to allow using it in further overlay
   * operations.
   *
   * @param geom
   *   geometry to self-snap
   * @param snapTol
   *   snap tolerance
   * @return
   *   the snapped geometry (homogeneous)
   */
  def snapSelf(geom: Geometry, snapTol: Double): Geometry = {
    val ov        = new OverlayNG(geom, null)
    val snapNoder = new SnappingNoder(snapTol)
    ov.setNoder(snapNoder)

    /**
     * Ensure the result is not mixed-dimension, since it will be used in further overlay
     * computation. It may however be lower dimension, if it collapses completely due to snapping.
     */
    ov.setStrictMode(true)
    ov.getResult
  }

  private def overlaySnapTol(geom0: Geometry, geom1: Geometry, opCode: Int, snapTol: Double) = {
    val snapNoder = new SnappingNoder(snapTol)
    OverlayNG.overlay(geom0, geom1, opCode, snapNoder)
  }

  /**
   * A factor for a snapping tolerance distance which should allow noding to be computed robustly.
   */
  private val SNAP_TOL_FACTOR = 1e12

  /**
   * Computes a heuristic snap tolerance distance for overlaying a pair of geometries using a {@link
   * SnappingNoder}.
   *
   * @param geom0
   * @param geom1
   * @return
   *   the snap tolerance
   */
  private def snapTolerance(geom0: Geometry, geom1: Geometry): Double = {
    val tol0    = snapTolerance(geom0)
    val tol1    = snapTolerance(geom1)
    val snapTol = Math.max(tol0, tol1)
    snapTol
  }

  private def snapTolerance(geom: Geometry) = {
    val magnitude = ordinateMagnitude(geom)
    magnitude / SNAP_TOL_FACTOR
  }

  /**
   * Computes the largest magnitude of the ordinates of a geometry, based on the geometry envelope.
   *
   * @param geom
   *   a geometry
   * @return
   *   the magnitude of the largest ordinate
   */
  private def ordinateMagnitude(geom: Geometry): Double = {
    if (geom == null) return 0
    val env    = geom.getEnvelopeInternal
    val magMax = Math.max(Math.abs(env.getMaxX), Math.abs(env.getMaxY))
    val magMin = Math.max(Math.abs(env.getMinX), Math.abs(env.getMinY))
    Math.max(magMax, magMin)
  }

  /**
   * Attempt Overlay using Snap-Rounding with an automatically-determined scale factor.
   *
   * @param geom0
   * @param geom1
   * @param opCode
   * @return
   *   the computed overlay result, or null if the overlay fails
   */
  private def overlaySR(geom0: Geometry, geom1: Geometry, opCode: Int): Geometry = {
    var result: Geometry = null
    try { // System.out.println("OverlaySnapIfNeeded: trying snap-rounding");
      val scaleSafe = PrecisionUtil.safeScale(geom0, geom1)
      val pmSafe    = new PrecisionModel(scaleSafe)
      result = OverlayNG.overlay(geom0, geom1, opCode, pmSafe)
      return result
    } catch {
      case _: TopologyException =>

    }
    null
  }
}
