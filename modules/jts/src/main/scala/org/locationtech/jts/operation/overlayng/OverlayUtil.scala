// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

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
package org.locationtech.jts.operation.overlayng

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

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.Point
import org.locationtech.jts.geom.Polygon
import org.locationtech.jts.geom.PrecisionModel

import java.util
import scala.jdk.CollectionConverters._

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
 * Utility methods for overlay processing.
 *
 * @author
 *   mdavis
 */
object OverlayUtil {

  /**
   * A null-handling wrapper for {@link PrecisionModel# isFloating ( )}
   *
   * @param pm
   * @return
   */
  private[overlayng] def isFloating(pm: PrecisionModel): Boolean = {
    if (pm == null) return true
    pm.isFloating
  }

  /**
   * Computes a clipping envelope for overlay input geometries. The clipping envelope encloses all
   * geometry line segments which might participate in the overlay, with a buffer to account for
   * numerical precision (in particular, rounding due to a precision model. The clipping envelope is
   * used in both the {@link RingClipper} and in the {@link LineLimiter}. <p> Some overlay
   * operations (i.e. {@link OverlayNG# UNION and OverlayNG#SYMDIFFERENCE} cannot use clipping as an
   * optimization, since the result envelope is the full extent of the two input geometries. In this
   * case the returned envelope is <code>null</code> to indicate this.
   *
   * @param opCode
   *   the overlay op code
   * @param inputGeom
   *   the input geometries
   * @param pm
   *   the precision model being used
   * @return
   *   an envelope for clipping and line limiting, or null if no clipping is performed
   */
  private[overlayng] def clippingEnvelope(
    opCode:    Int,
    inputGeom: InputGeometry,
    pm:        PrecisionModel
  ): Envelope = {
    val resultEnv = resultEnvelope(opCode, inputGeom, pm)
    if (resultEnv == null) return null
    val clipEnv   = RobustClipEnvelopeComputer.getEnvelope(inputGeom.getGeometry(0),
                                                         inputGeom.getGeometry(1),
                                                         resultEnv
    )
    safeEnv(clipEnv, pm)
  }

  /**
   * Computes an envelope which covers the extent of the result of a given overlay operation for
   * given inputs. The operations which have a result envelope smaller than the extent of the inputs
   * are: <ul> <li>{@link OverlayNG# INTERSECTION}: result envelope is the intersection of the input
   * envelopes <li>{@link OverlayNG# DIFERENCE}: result envelope is the envelope of the A input
   * geometry </ul> Otherwise, <code>null</code> is returned to indicate full extent.
   *
   * @param opCode
   * @param inputGeom
   * @param pm
   * @return
   *   the result envelope, or null if the full extent
   */
  private def resultEnvelope(opCode: Int, inputGeom: InputGeometry, pm: PrecisionModel): Envelope =
    opCode match {
      case OverlayNG.INTERSECTION =>
        // use safe envelopes for intersection to ensure they contain rounded coordinates
        val envA = safeEnv(inputGeom.getEnvelope(0), pm)
        val envB = safeEnv(inputGeom.getEnvelope(1), pm)
        envA.intersection(envB)

      case OverlayNG.DIFFERENCE =>
        safeEnv(inputGeom.getEnvelope(0), pm)
      // return null for UNION and SYMDIFFERENCE to indicate no clipping
      case _                    => null

    }

  /**
   * Determines a safe geometry envelope for clipping, taking into account the precision model being
   * used.
   *
   * @param env
   *   a geometry envelope
   * @param pm
   *   the precision model
   * @return
   *   a safe envelope to use for clipping
   */
  private def safeEnv(env: Envelope, pm: PrecisionModel): Envelope = {
    val envExpandDist = safeExpandDistance(env, pm)
    val safeEnv       = env.copy
    safeEnv.expandBy(envExpandDist)
    safeEnv
  }

  private val SAFE_ENV_BUFFER_FACTOR = 0.1
  private val SAFE_ENV_GRID_FACTOR   = 3

  private def safeExpandDistance(env: Envelope, pm: PrecisionModel) = {
    var envExpandDist = .0
    if (isFloating(pm)) { // if PM is FLOAT then there is no scale factor, so add 10%
      val minSize = Math.min(env.getHeight, env.getWidth)
      envExpandDist = SAFE_ENV_BUFFER_FACTOR * minSize
    } else { // if PM is fixed, add a small multiple of the grid size
      val gridSize = 1.0 / pm.getScale
      envExpandDist = SAFE_ENV_GRID_FACTOR * gridSize
    }
    envExpandDist
  }

  /**
   * Tests if the result can be determined to be empty based on simple properties of the input
   * geometries (such as whether one or both are empty, or their envelopes are disjoint).
   *
   * @param opCode
   *   the overlay operation
   * @param inputGeom
   *   the input geometries
   * @return
   *   true if the overlay result is determined to be empty
   */
  private[overlayng] def isEmptyResult(
    opCode: Int,
    a:      Geometry,
    b:      Geometry,
    pm:     PrecisionModel
  ): Boolean = {
    opCode match {
      case OverlayNG.INTERSECTION =>
        if (isEnvDisjoint(a, b, pm)) return true

      case OverlayNG.DIFFERENCE =>
        if (isEmpty(a)) return true

      case OverlayNG.UNION | OverlayNG.SYMDIFFERENCE =>
        if (isEmpty(a) && isEmpty(b)) return true

    }
    false
  }

  private def isEmpty(geom: Geometry) = geom == null || geom.isEmpty

  /**
   * Tests if the geometry envelopes are disjoint, or empty. The disjoint test must take into
   * account the precision model being used, since geometry coordinates may shift under rounding.
   *
   * @param a
   *   a geometry
   * @param b
   *   a geometry
   * @param pm
   *   the precision model being used
   * @return
   *   true if the geometry envelopes are disjoint or empty
   */
  private[overlayng] def isEnvDisjoint(a: Geometry, b: Geometry, pm: PrecisionModel): Boolean = {
    if (isEmpty(a) || isEmpty(b)) return true
    if (isFloating(pm)) return a.getEnvelopeInternal.disjoint(b.getEnvelopeInternal)
    isDisjoint(a.getEnvelopeInternal, b.getEnvelopeInternal, pm)
  }

  /**
   * Tests for disjoint envelopes adjusting for rounding caused by a fixed precision model. Assumes
   * envelopes are non-empty.
   *
   * @param envA
   *   an envelope
   * @param envB
   *   an envelope
   * @param pm
   *   the precision model
   * @return
   *   true if the envelopes are disjoint
   */
  private def isDisjoint(envA: Envelope, envB: Envelope, pm: PrecisionModel): Boolean = {
    if (pm.makePrecise(envB.getMinX) > pm.makePrecise(envA.getMaxX)) return true
    if (pm.makePrecise(envB.getMaxX) < pm.makePrecise(envA.getMinX)) return true
    if (pm.makePrecise(envB.getMinY) > pm.makePrecise(envA.getMaxY)) return true
    if (pm.makePrecise(envB.getMaxY) < pm.makePrecise(envA.getMinY)) return true
    false
  }

  /**
   * Creates an empty result geometry of the appropriate dimension, based on the given overlay
   * operation and the dimensions of the inputs. The created geometry is an atomic geometry, not a
   * collection (unless the dimension is -1, in which case a <code>GEOMETRYCOLLECTION EMPTY</code>
   * is created.)
   *
   * @param dim
   *   the dimension of the empty geometry to create
   * @param geomFact
   *   the geometry factory being used for the operation
   * @return
   *   an empty atomic geometry of the appropriate dimension
   */
  private[overlayng] def createEmptyResult(dim: Int, geomFact: GeometryFactory): Geometry =
    dim match {
      case 0 =>
        geomFact.createPoint

      case 1 =>
        geomFact.createLineString

      case 2 =>
        geomFact.createPolygon

      case -1 =>
        geomFact.createGeometryCollection

      case _ =>
        throw new RuntimeException("Unable to determine overlay result geometry dimension")
    }

  /**
   * Computes the dimension of the result of applying the given operation to inputs with the given
   * dimensions. This assumes that complete collapse does not occur. <p> The result dimension is
   * computed according to the following rules: <ul> <li>{@link OverlayNG# INTERSECTION} - result
   * has the dimension of the lowest input dimension <li>{@link OverlayNG# UNION} - result has the
   * dimension of the highest input dimension <li>{@link OverlayNG# DIFFERENCE} - result has the
   * dimension of the left-hand input <li>{@link OverlayNG# SYMDIFFERENCE} - result has the
   * dimension of the highest input dimension (since the Symmetric Difference is the Union of the
   * Differences). </ul>
   *
   * @param opCode
   *   the overlay operation
   * @param dim0
   *   dimension of the LH input
   * @param dim1
   *   dimension of the RH input
   * @return
   *   the dimension of the result
   */
  def resultDimension(opCode: Int, dim0: Int, dim1: Int): Int = {
    var resultDimension = -1
    opCode match {
      case OverlayNG.INTERSECTION =>
        resultDimension = Math.min(dim0, dim1)

      case OverlayNG.UNION =>
        resultDimension = Math.max(dim0, dim1)

      case OverlayNG.DIFFERENCE =>
        resultDimension = dim0

      case OverlayNG.SYMDIFFERENCE =>
        /**
         * This result is chosen because <pre> SymDiff = Union( Diff(A, B), Diff(B, A) ) </pre> and
         * Union has the dimension of the highest-dimension argument.
         */
        resultDimension = Math.max(dim0, dim1)

    }
    resultDimension
  }

  /**
   * Creates an overlay result geometry for homogeneous or mixed components.
   *
   * @param resultPolyList
   *   the list of result polygons (may be empty or null)
   * @param resultLineList
   *   the list of result lines (may be empty or null)
   * @param resultPointList
   *   the list of result points (may be empty or null)
   * @param geometryFactory
   *   the geometry factory to use
   * @return
   *   a geometry structured according to the overlay result semantics
   */
  private[overlayng] def createResultGeometry(
    resultPolyList:  util.List[Polygon],
    resultLineList:  util.List[LineString],
    resultPointList: util.List[Point],
    geometryFactory: GeometryFactory
  ) = {
    val geomList = new util.ArrayList[Geometry]
    // TODO: for mixed dimension, return collection of Multigeom for each dimension (breaking change)
    // element geometries of the result are always in the order A,L,P
    if (resultPolyList != null) geomList.addAll(resultPolyList)
    if (resultLineList != null) geomList.addAll(resultLineList)
    if (resultPointList != null) geomList.addAll(resultPointList)
    // build the most specific geometry possible
    // TODO: perhaps do this internally to give more control?
    geometryFactory.buildGeometry(geomList)
  }

  private[overlayng] def toLines(
    graph:         OverlayGraph,
    isOutputEdges: Boolean,
    geomFact:      GeometryFactory
  ) = {
    val lines = new util.ArrayList[Geometry]
    for (edge <- graph.getEdges.asScala) {
      val includeEdge = isOutputEdges || edge.isInResultArea
      if (includeEdge) {
        // Coordinate[] pts = getCoords(nss);
        val pts  = edge.getCoordinatesOriented
        val line = geomFact.createLineString(pts)
        line.setUserData(labelForResult(edge))
        lines.add(line)
      }
    }
    geomFact.buildGeometry(lines)
  }

  private def labelForResult(edge: OverlayEdge) =
    edge.getLabel.toString(edge.isForward) + (if (edge.isInResultArea) " Res"
                                              else "")

  /**
   * Round the key point if precision model is fixed. Note: return value is only copied if rounding
   * is performed.
   *
   * @param pt
   *   the Point to round
   * @return
   *   the rounded point coordinate, or null if empty
   */
  def round(pt: Point, pm: PrecisionModel): Coordinate = {
    if (pt.isEmpty) return null
    val p = pt.getCoordinate.copy
    if (!isFloating(pm)) pm.makePrecise(p)
    p
  }
  /*
    private void checkSanity(Geometry result) {
      // for Union, area should be greater than largest of inputs
      double areaA = inputGeom.getGeometry(0).getArea();
      double areaB = inputGeom.getGeometry(1).getArea();
      double area = result.getArea();

      // if result is empty probably had a complete collapse, so can't use this check
      if (area == 0) return;

      if (opCode == UNION) {
        double minAreaLimit = 0.5 * Math.max(areaA, areaB);
        if (area < minAreaLimit ) {
          throw new TopologyException("Result area sanity issue");
        }
      }
    }
   */
}
