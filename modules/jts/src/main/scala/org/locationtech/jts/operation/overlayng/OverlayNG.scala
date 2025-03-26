// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.operation.overlayng

import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.Location
import org.locationtech.jts.geom.Point
import org.locationtech.jts.geom.Polygon
import org.locationtech.jts.geom.PrecisionModel
import org.locationtech.jts.noding.Noder
import org.locationtech.jts.noding.SegmentString
import org.locationtech.jts.operation.overlay.OverlayOp

import java.util
import scala.jdk.CollectionConverters._

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
 * Computes the geometric overlay of two {@link Geometry} s, using an explicit precision model to
 * allow robust computation. The overlay can be used to determine any of the following set-theoretic
 * operations (boolean combinations) of the geometries: <ul> <li>{@link INTERSECTION} - all points
 * which lie in both geometries <li>{@link UNION} - all points which lie in at least one geometry
 * <li>{@link DIFFERENCE} - all points which lie in the first geometry but not the second <li>{@link
 * SYMDIFFERENCE} - all points which lie in one geometry but not both </ul> Input geometries may
 * have different dimension. Collections must be homogeneous (all elements must have the same
 * dimension). <p> The precision model used for the computation can be supplied independent of the
 * precision model of the input geometry. The main use for this is to allow using a fixed precision
 * for geometry with a floating precision model. This does two things: ensures robust computation;
 * and forces the output to be validly rounded to the precision model. <p> For fixed precision
 * models noding is performed using a {@link SnapRoundingNoder} . This provides robust computation
 * (as long as precision is limited to around 13 decimal digits). <p> For floating precision an
 * {@link MCIndexNoder} is used. This is not fully robust, so can sometimes result in {@link
 * TopologyException}s being thrown. For robust full-precision overlay see {@link OverlayNGRobust} .
 * <p> A custom {@link Noder} can be supplied. This allows using a more performant noding strategy
 * in specific cases, for instance in {@link CoverageUnion} . <p> <b>Note:</b If a {@link
 * SnappingNoder} is used it is best to specify a fairly small snap tolerance, since the
 * intersection clipping optimization can interact with the snapping to alter the result. <p>
 * Optionally the overlay computation can process using strict mode (via {@link # setStrictMode (
 * boolean )}. In strict mode result semantics are: <ul> <li>Result geometries are homogeneous (all
 * components are of same dimension), except for some cases of symmetricDifference. <li>Lines and
 * Points resulting from topology collapses are not included in the result </ul> Strict mode has the
 * following benefits: <ul> <li>Results are simpler <li>Overlay operations are easily chainable
 * </ul> The original JTS overlay semantics corresponds to non-strict mode.
 *
 * @author
 *   mdavis
 * @see
 *   OverlayNGRobust
 */
object OverlayNG {

  /**
   * The code for the Intersection overlay operation.
   */
  val INTERSECTION: Int = OverlayOp.INTERSECTION

  /**
   * The code for the Union overlay operation.
   */
  val UNION: Int = OverlayOp.UNION

  /**
   * The code for the Difference overlay operation.
   */
  val DIFFERENCE: Int = OverlayOp.DIFFERENCE

  /**
   * The code for the Symmetric Difference overlay operation.
   */
  val SYMDIFFERENCE: Int = OverlayOp.SYMDIFFERENCE

  /**
   * The default setting for Strict Mode.
   *
   * The original JTS overlay semantics used non-strict result semantics, including;
   *   - An Intersection result can be mixed-dimension, due to inclusion of intersection components
   *     of all dimensions
   *   - Results can include lines caused by Area topology collapse
   */
  private[overlayng] val STRICT_MODE_DEFAULT = false

  /**
   * Tests whether a point with a given topological {@link Label} relative to two geometries is
   * contained in the result of overlaying the geometries using a given overlay operation. <p> The
   * method handles arguments of {@link Location# NONE} correctly
   *
   * @param label
   *   the topological label of the point
   * @param opCode
   *   the code for the overlay operation to test
   * @return
   *   true if the label locations correspond to the overlayOpCode
   */
  private[overlayng] def isResultOfOpPoint(label: OverlayLabel, opCode: Int) = {
    val loc0 = label.getLocation(0)
    val loc1 = label.getLocation(1)
    isResultOfOp(opCode, loc0, loc1)
  }

  /**
   * Tests whether a point with given {@link Location} s relative to two geometries would be
   * contained in the result of overlaying the geometries using a given overlay operation. This is
   * used to determine whether components computed during the overlay process should be included in
   * the result geometry. <p> The method handles arguments of {@link Location# NONE} correctly.
   *
   * @param overlayOpCode
   *   the code for the overlay operation to test
   * @param loc0
   *   the code for the location in the first geometry
   * @param loc1
   *   the code for the location in the second geometry
   * @return
   *   true if a point with given locations is in the result of the overlay operation
   */
  private[overlayng] def isResultOfOp(overlayOpCode: Int, loc0: Int, loc1: Int): Boolean = {
    val _loc0 = if (loc0 == Location.BOUNDARY) Location.INTERIOR else loc0
    val _loc1 = if (loc1 == Location.BOUNDARY) Location.INTERIOR else loc1
    overlayOpCode match {
      case INTERSECTION  =>
        return _loc0 == Location.INTERIOR && _loc1 == Location.INTERIOR
      case UNION         =>
        return _loc0 == Location.INTERIOR || _loc1 == Location.INTERIOR
      case DIFFERENCE    =>
        return _loc0 == Location.INTERIOR && _loc1 != Location.INTERIOR
      case SYMDIFFERENCE =>
        return (_loc0 == Location.INTERIOR && _loc1 != Location.INTERIOR) || (_loc0 != Location.INTERIOR && _loc1 == Location.INTERIOR)
    }
    false
  }

  /**
   * Computes an overlay operation for the given geometry operands, with the noding strategy
   * determined by the precision model.
   *
   * @param geom0
   *   the first geometry argument
   * @param geom1
   *   the second geometry argument
   * @param opCode
   *   the code for the desired overlay operation
   * @param pm
   *   the precision model to use
   * @return
   *   the result of the overlay operation
   */
  def overlay(geom0: Geometry, geom1: Geometry, opCode: Int, pm: PrecisionModel): Geometry = {
    val ov     = new OverlayNG(geom0, geom1, pm, opCode)
    val geomOv = ov.getResult
    geomOv
  }

  /**
   * Computes an overlay operation on the given geometry operands, using a supplied {@link Noder} .
   *
   * @param geom0
   *   the first geometry argument
   * @param geom1
   *   the second geometry argument
   * @param opCode
   *   the code for the desired overlay operation
   * @param pm
   *   the precision model to use (which may be null if the noder does not use one)
   * @param noder
   *   the noder to use
   * @return
   *   the result of the overlay operation
   */
  def overlay(
    geom0:  Geometry,
    geom1:  Geometry,
    opCode: Int,
    pm:     PrecisionModel,
    noder:  Noder[SegmentString]
  ): Geometry = {
    val ov     = new OverlayNG(geom0, geom1, pm, opCode)
    ov.setNoder(noder)
    val geomOv = ov.getResult
    geomOv
  }

  /**
   * Computes an overlay operation on the given geometry operands, using a supplied {@link Noder} .
   *
   * @param geom0
   *   the first geometry argument
   * @param geom1
   *   the second geometry argument
   * @param opCode
   *   the code for the desired overlay operation
   * @param noder
   *   the noder to use
   * @return
   *   the result of the overlay operation
   */
  def overlay(
    geom0:  Geometry,
    geom1:  Geometry,
    opCode: Int,
    noder:  Noder[SegmentString]
  ): Geometry = {
    val ov     = new OverlayNG(geom0, geom1, null, opCode)
    ov.setNoder(noder)
    val geomOv = ov.getResult
    geomOv
  }

  /**
   * Computes an overlay operation on the given geometry operands, using the precision model of the
   * geometry. and an appropriate noder. <p> The noder is chosen according to the precision model
   * specified. <ul> <li>For {@link PrecisionModel# FIXED} a snap-rounding noder is used, and the
   * computation is robust. <li>For {@link PrecisionModel# FLOATING} a non-snapping noder is used,
   * and this computation may not be robust. If errors occur a {@link TopologyException} is thrown.
   * </ul>
   *
   * @param geom0
   *   the first argument geometry
   * @param geom1
   *   the second argument geometry
   * @param opCode
   *   the code for the desired overlay operation
   * @return
   *   the result of the overlay operation
   */
  def overlay(geom0: Geometry, geom1: Geometry, opCode: Int): Geometry = {
    val ov = new OverlayNG(geom0, geom1, opCode)
    ov.getResult
  }

  /**
   * Computes a union operation on the given geometry, with the supplied precision model. The
   * primary use for this is to perform precision reduction (round the geometry to the supplied
   * precision). <p> The input must be a valid geometry. Collections must be homogeneous. <p> To
   * union an overlapping set of polygons in a more performant way use {@link UnaryUnionNG} . To
   * union a polyonal coverage or linear network in a more performant way, use {@link
   * CoverageUnion}.
   *
   * @param geom0
   *   the geometry
   * @param pm
   *   the precision model to use
   * @return
   *   the result of the union operation
   * @see
   *   OverlayMixedPoints
   * @see
   *   PrecisionReducer
   * @see
   *   UnaryUnionNG
   * @see
   *   CoverageUnion
   */
  private[overlayng] def union(geom: Geometry, pm: PrecisionModel) = {
    val ov     = new OverlayNG(geom, pm)
    val geomOv = ov.getResult
    geomOv
  }

  /**
   * Computes a union of a single geometry using a custom noder. <p> The primary use of this is to
   * support coverage union.
   *
   * @param geom
   *   the geometry to union
   * @param pm
   *   the precision model to use (maybe be null)
   * @param noder
   *   the noder to use
   * @return
   *   the result geometry
   * @see
   *   CoverageUnion
   */
  private[overlayng] def union(
    geom:  Geometry,
    pm:    PrecisionModel,
    noder: Noder[SegmentString]
  ) = {
    val ov     = new OverlayNG(geom, pm)
    ov.setNoder(noder)
    ov.setStrictMode(true)
    val geomOv = ov.getResult
    geomOv
  }

  private def isEmpty(list: util.List[_]) = list == null || list.size == 0
}

/**
 * Creates an overlay operation on the given geometries, with a defined precision model. The noding
 * strategy is determined by the precision model.
 *
 * @param geom0
 *   the A operand geometry
 * @param geom1
 *   the B operand geometry (may be null)
 * @param pm
 *   the precision model to use
 * @param opCode
 *   the overlay opcode
 */
class OverlayNG(val geom0: Geometry, val geom1: Geometry, var pm: PrecisionModel, var opCode: Int) {

  private val inputGeom: InputGeometry     = new InputGeometry(geom0, geom1)
  private val geomFact: GeometryFactory    = geom0.getFactory
  private var noder: Noder[SegmentString]  = null
  private var isStrictMode: Boolean        = OverlayNG.STRICT_MODE_DEFAULT
  private var isOptimized: Boolean         = true
  private var isAreaResultOnly: Boolean    = false
  private var isOutputEdges: Boolean       = false
  private var isOutputResultEdges: Boolean = false
  private var isOutputNodedEdges: Boolean  = false

  /**
   * Creates an overlay operation on the given geometries using the precision model of the
   * geometries. <p> The noder is chosen according to the precision model specified. <ul> <li>For
   * {@link PrecisionModel# FIXED} a snap-rounding noder is used, and the computation is robust.
   * <li>For {@link PrecisionModel# FLOATING} a non-snapping noder is used, and this computation may
   * not be robust. If errors occur a {@link TopologyException} is thrown. </ul>
   *
   * @param geom0
   *   the A operand geometry
   * @param geom1
   *   the B operand geometry (may be null)
   * @param opCode
   *   the overlay opcode
   */
  def this(geom0: Geometry, geom1: Geometry, opCode: Int) =
    this(geom0, geom1, geom0.getFactory.getPrecisionModel, opCode)

  /**
   * Creates a union of a single geometry with a given precision model.
   *
   * @param geom
   *   the geometry
   * @param pm
   *   the precision model to use
   */
  def this(geom: Geometry, pm: PrecisionModel) =
    this(geom, null, pm, OverlayNG.UNION)

  /**
   * Sets whether the overlay results are computed according to strict mode semantics. <ul>
   * <li>Result geometry is always homogeneous (except for some SymmetricDifference cases) <li>Lines
   * resulting from topology collapse are not included </ul>
   *
   * @param isStrictMode
   *   true if strict mode is to be used
   */
  def setStrictMode(isStrictMode: Boolean): Unit =
    this.isStrictMode = isStrictMode

  /**
   * Sets whether overlay processing optimizations are enabled. It may be useful to disable
   * optimizations for testing purposes. Default is TRUE (optimization enabled).
   *
   * @param isOptimized
   *   whether to optimize processing
   */
  def setOptimized(isOptimized: Boolean): Unit =
    this.isOptimized = isOptimized

  /**
   * Sets whether the result can contain only {@link Polygon} components. This is used if it is
   * known that the result must be an (possibly empty) area.
   *
   * @param isAreaResultOnly
   *   true if the result should contain only area components
   */
  private[overlayng] def setAreaResultOnly(isAreaResultOnly: Boolean): Unit =
    this.isAreaResultOnly = isAreaResultOnly

  /**
   * @param isOutputEdges
   */
  def setOutputEdges(isOutputEdges: Boolean): Unit =
    this.isOutputEdges = isOutputEdges

  def setOutputNodedEdges(isOutputNodedEdges: Boolean): Unit = {
    this.isOutputEdges = true
    this.isOutputNodedEdges = isOutputNodedEdges
  }

  def setOutputResultEdges(isOutputResultEdges: Boolean): Unit =
    this.isOutputResultEdges = isOutputResultEdges

  def setNoder(noder: Noder[SegmentString]): Unit =
    this.noder = noder

  /**
   * Gets the result of the overlay operation.
   *
   * @return
   *   the result of the overlay operation.
   */
  def getResult: Geometry = {
    if (OverlayUtil.isEmptyResult(opCode, inputGeom.getGeometry(0), inputGeom.getGeometry(1), pm)) {
      return createEmptyResult
    }
    // special logic for Point-Point inputs
    if (inputGeom.isAllPoints) {
      return OverlayPoints.overlay(opCode, inputGeom.getGeometry(0), inputGeom.getGeometry(1), pm)
    }
    // special logic for Point-nonPoint inputs
    if (!inputGeom.isSingle && inputGeom.hasPoints) {
      return OverlayMixedPoints.overlay(opCode,
                                        inputGeom.getGeometry(0),
                                        inputGeom.getGeometry(1),
                                        pm
      )
    }
    val result: Geometry = computeEdgeOverlay
    return result
  }

  private def computeEdgeOverlay: Geometry = {
    val edges: util.List[Edge] = nodeEdges
    val graph: OverlayGraph    = buildGraph(edges)
    if (isOutputNodedEdges) {
      return OverlayUtil.toLines(graph, isOutputEdges, geomFact)
    }
    labelGraph(graph)
    // for (OverlayEdge e : graph.getEdges()) {  Debug.println(e);  }
    if (isOutputEdges || isOutputResultEdges) {
      return OverlayUtil.toLines(graph, isOutputEdges, geomFact)
    }
    return extractResult(opCode, graph)
  }

  private def nodeEdges: util.List[Edge] = {

    /**
     * Node the edges, using whatever noder is being used
     */
    val nodingBuilder: EdgeNodingBuilder = new EdgeNodingBuilder(pm, noder)

    /**
     * Optimize Intersection and Difference by clipping to the result extent, if enabled.
     */
    if (isOptimized) {
      val clipEnv: Envelope = OverlayUtil.clippingEnvelope(opCode, inputGeom, pm)
      if (clipEnv != null) {
        nodingBuilder.setClipEnvelope(clipEnv)
      }
    }
    val mergedEdges: util.List[Edge] =
      nodingBuilder.build(inputGeom.getGeometry(0), inputGeom.getGeometry(1))

    /**
     * Record if an input geometry has collapsed. This is used to avoid trying to locate
     * disconnected edges against a geometry which has collapsed completely.
     */
    inputGeom.setCollapsed(0, !nodingBuilder.hasEdgesFor(0))
    inputGeom.setCollapsed(1, !nodingBuilder.hasEdgesFor(1))
    return mergedEdges
  }

  private def buildGraph(edges: util.Collection[Edge]): OverlayGraph = {
    val graph: OverlayGraph = new OverlayGraph
    for (e <- edges.asScala)
      graph.addEdge(e.getCoordinates, e.createLabel)
    return graph
  }

  private def labelGraph(graph: OverlayGraph): Unit = {
    val labeller: OverlayLabeller = new OverlayLabeller(graph, inputGeom)
    labeller.computeLabelling()
    labeller.markResultAreaEdges(opCode)
    labeller.unmarkDuplicateEdgesFromResultArea()
  }

  /**
   * Extracts the result geometry components from the fully labelled topology graph. <p> This method
   * implements the semantic that the result of an intersection operation is homogeneous with
   * highest dimension. In other words, if an intersection has components of a given dimension no
   * lower-dimension components are output. For example, if two polygons intersect in an area, no
   * linestrings or points are included in the result, even if portions of the input do meet in
   * lines or points. This semantic choice makes more sense for typical usage, in which only the
   * highest dimension components are of interest.
   *
   * @param opCode
   *   the overlay operation
   * @param graph
   *   the topology graph
   * @return
   *   the result geometry
   */
  private def extractResult(opCode: Int, graph: OverlayGraph): Geometry = {
    val isAllowMixedIntResult: Boolean          = !isStrictMode
    // --- Build polygons
    val resultAreaEdges: util.List[OverlayEdge] = graph.getResultAreaEdges
    val polyBuilder: PolygonBuilder             = new PolygonBuilder(resultAreaEdges, geomFact)
    val resultPolyList: util.List[Polygon]      = polyBuilder.getPolygons
    val hasResultAreaComponents: Boolean        = resultPolyList.size > 0
    var resultLineList: util.List[LineString]   = null
    var resultPointList: util.List[Point]       = null
    if (!isAreaResultOnly) { // --- Build lines
      val allowResultLines: Boolean =
        !hasResultAreaComponents || isAllowMixedIntResult || opCode == OverlayNG.SYMDIFFERENCE || opCode == OverlayNG.UNION
      if (allowResultLines) {
        val lineBuilder: LineBuilder =
          new LineBuilder(inputGeom, graph, hasResultAreaComponents, opCode, geomFact)
        lineBuilder.setStrictMode(isStrictMode)
        resultLineList = lineBuilder.getLines
      }

      /**
       * Operations with point inputs are handled elsewhere. Only an Intersection op can produce
       * point results from non-point inputs.
       */
      val hasResultComponents: Boolean = hasResultAreaComponents || resultLineList.size > 0
      val allowResultPoints: Boolean   = !hasResultComponents || isAllowMixedIntResult
      if (opCode == OverlayNG.INTERSECTION && allowResultPoints) {
        val pointBuilder: IntersectionPointBuilder = new IntersectionPointBuilder(graph, geomFact)
        pointBuilder.setStrictMode(isStrictMode)
        resultPointList = pointBuilder.getPoints
      }
    }
    if (
      OverlayNG.isEmpty(resultPolyList) && OverlayNG
        .isEmpty(resultLineList) && OverlayNG.isEmpty(resultPointList)
    ) {
      return createEmptyResult
    }
    val resultGeom: Geometry                    =
      OverlayUtil.createResultGeometry(resultPolyList, resultLineList, resultPointList, geomFact)
    return resultGeom
  }

  private def createEmptyResult: Geometry =
    return OverlayUtil.createEmptyResult(
      OverlayUtil.resultDimension(opCode, inputGeom.getDimension(0), inputGeom.getDimension(1)),
      geomFact
    )
}
