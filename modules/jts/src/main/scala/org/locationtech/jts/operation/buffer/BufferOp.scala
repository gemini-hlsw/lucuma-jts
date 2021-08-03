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
package org.locationtech.jts.operation.buffer

/**
 * @version 1.7
 */

import org.locationtech.jts.geom.{ Geometry, PrecisionModel, TopologyException }
import org.locationtech.jts.math.MathUtil
import org.locationtech.jts.noding.ScaledNoder
import org.locationtech.jts.noding.snaprounder.MCIndexSnapRounder
//import debug.*;
// /**
//  * Computes the buffer of a geometry, for both positive and negative buffer distances.
//  * <p>
//  * In GIS, the positive (or negative) buffer of a geometry is defined as
//  * the Minkowski sum (or difference) of the geometry
//  * with a circle of radius equal to the absolute value of the buffer distance.
//  * In the CAD/CAM world buffers are known as <i>offset curves</i>.
//  * In morphological analysis the
//  * operation of positive and negative buffering
//  * is referred to as <i>erosion</i> and <i>dilation</i>
//  * <p>
//  * The buffer operation always returns a polygonal result.
//  * The negative or zero-distance buffer of lines and points is always an empty {link Polygon}.
//  * <p>
//  * Since true buffer curves may contain circular arcs,
//  * computed buffer polygons are only approximations to the true geometry.
//  * The user can control the accuracy of the approximation by specifying
//  * the number of linear segments used to approximate arcs.
//  * This is specified via {link BufferParameters#setQuadrantSegments(int)} or {link #setQuadrantSegments(int)}.
//  * <p>
//  * The <b>end cap style</b> of a linear buffer may be {link BufferParameters#setEndCapStyle(int) specified}. The
//  * following end cap styles are supported:
//  * <ul>
//  * <li>{link BufferParameters#CAP_ROUND} - the usual round end caps
//  * <li>{link BufferParameters#CAP_FLAT} - end caps are truncated flat at the line ends
//  * <li>{link BufferParameters#CAP_SQUARE} - end caps are squared off at the buffer distance beyond the line ends
//  * </ul>
//  * <p>
//  * The <b>join style</b> of the corners in a buffer may be {link BufferParameters#setJoinStyle(int) specified}. The
//  * following join styles are supported:
//  * <ul>
//  * <li>{link BufferParameters#JOIN_ROUND} - the usual round join
//  * <li>{link BufferParameters#JOIN_MITRE} - corners are "sharp" (up to a {link BufferParameters#getMitreLimit() distance limit})
//  * <li>{link BufferParameters#JOIN_BEVEL} - corners are beveled (clipped off).
//  * </ul>
//  * <p>
//  * The buffer algorithm can perform simplification on the input to increase performance.
//  * The simplification is performed a way that always increases the buffer area
//  * (so that the simplified input covers the original input).
//  * The degree of simplification can be {link BufferParameters#setSimplifyFactor(double) specified},
//  * with a {link BufferParameters#DEFAULT_SIMPLIFY_FACTOR default} used otherwise.
//  * Note that if the buffer distance is zero then so is the computed simplify tolerance,
//  * no matter what the simplify factor.
//  *
//  * @version 1.7
//  */
object BufferOp {

  /**
   * Specifies a round line buffer end cap style.
   *
   * @deprecated use BufferParameters
   */
  val CAP_ROUND: Int = BufferParameters.CAP_ROUND

  /**
   * Specifies a butt (or flat) line buffer end cap style.
   *
   * @deprecated use BufferParameters
   */
  val CAP_BUTT: Int = BufferParameters.CAP_FLAT
  val CAP_FLAT: Int = BufferParameters.CAP_FLAT

  /**
   * Specifies a square line buffer end cap style.
   *
   * @deprecated use BufferParameters
   */
  val CAP_SQUARE: Int = BufferParameters.CAP_SQUARE

  /**
   * A number of digits of precision which leaves some computational "headroom"
   * for floating point operations.
   *
   * This value should be less than the decimal precision of double-precision values (16).
   */
  private val MAX_PRECISION_DIGITS = 12

  /**
   * Compute a scale factor to limit the precision of
   * a given combination of Geometry and buffer distance.
   * The scale factor is determined by
   * the number of digits of precision in the (geometry + buffer distance),
   * limited by the supplied <code>maxPrecisionDigits</code> value.
   * <p>
   * The scale factor is based on the absolute magnitude of the (geometry + buffer distance).
   * since this determines the number of digits of precision which must be handled.
   *
   * @param g                  the Geometry being buffered
   * @param distance           the buffer distance
   * @param maxPrecisionDigits the max # of digits that should be allowed by
   *                           the precision determined by the computed scale factor
   * return a scale factor for the buffer computation
   */
  private def precisionScaleFactor(g: Geometry, distance: Double, maxPrecisionDigits: Int) = {
    val env                   = g.getEnvelopeInternal
    val envMax                = MathUtil.max(Math.abs(env.getMaxX),
                              Math.abs(env.getMaxY),
                              Math.abs(env.getMinX),
                              Math.abs(env.getMinY)
    )
    val expandByDistance      =
      if (distance > 0.0) distance
      else 0.0
    val bufEnvMax             = envMax + 2 * expandByDistance
    // the smallest power of 10 greater than the buffer envelope
    val bufEnvPrecisionDigits = (Math.log(bufEnvMax) / Math.log(10) + 1.0).toInt
    val minUnitLog10          = maxPrecisionDigits - bufEnvPrecisionDigits
    val scaleFactor           = Math.pow(10.0, minUnitLog10.toDouble)
    scaleFactor
  }

  /**
   * Computes the buffer of a geometry for a given buffer distance.
   *
   * @param g        the geometry to buffer
   * @param distance the buffer distance
   * return the buffer of the input geometry
   */
  def bufferOp(g: Geometry, distance: Double): Geometry = {
    val gBuf    = new BufferOp(g)
    val geomBuf = gBuf.getResultGeometry(distance)
    //BufferDebug.saveBuffer(geomBuf);
    //BufferDebug.runCount++;
    geomBuf
  }

  /**
   * Computes the buffer for a geometry for a given buffer distance
   * and accuracy of approximation.
   *
   * @param g        the geometry to buffer
   * @param distance the buffer distance
   * @param params   the buffer parameters to use
   * return the buffer of the input geometry
   */
  def bufferOp(g: Geometry, distance: Double, params: BufferParameters): Geometry = {
    val bufOp   = new BufferOp(g, params)
    val geomBuf = bufOp.getResultGeometry(distance)
    geomBuf
  }

  /**
   * Computes the buffer for a geometry for a given buffer distance
   * and accuracy of approximation.
   *
   * @param g                the geometry to buffer
   * @param distance         the buffer distance
   * @param quadrantSegments the number of segments used to approximate a quarter circle
   * return the buffer of the input geometry
   */
  def bufferOp(g: Geometry, distance: Double, quadrantSegments: Int): Geometry = {
    val bufOp   = new BufferOp(g)
    bufOp.setQuadrantSegments(quadrantSegments)
    val geomBuf = bufOp.getResultGeometry(distance)
    geomBuf
  }

  /**
   * Computes the buffer for a geometry for a given buffer distance
   * and accuracy of approximation.
   *
   * @param g                the geometry to buffer
   * @param distance         the buffer distance
   * @param quadrantSegments the number of segments used to approximate a quarter circle
   * @param endCapStyle      the end cap style to use
   * return the buffer of the input geometry
   */
  def bufferOp(g: Geometry, distance: Double, quadrantSegments: Int, endCapStyle: Int): Geometry = {
    val bufOp   = new BufferOp(g)
    bufOp.setQuadrantSegments(quadrantSegments)
    bufOp.setEndCapStyle(endCapStyle)
    val geomBuf = bufOp.getResultGeometry(distance)
    geomBuf
  }
}

class BufferOp(g: Geometry, bufParams: BufferParameters = new BufferParameters()) {
  private val argGeom: Geometry               = g
  private var distance                        = .0
  private var resultGeometry: Geometry        = null
  private var saveException: RuntimeException = null // debugging only
  /**
   * Initializes a buffer computation for the given geometry
   *
   * @param g the geometry to buffer
   */
//  def this(g: Geometry) {
//    this()
//    argGeom = g
//  }

  /**
   * Initializes a buffer computation for the given geometry
   * with the given set of parameters
   *
   * @param g         the geometry to buffer
   * @param bufParams the buffer parameters to use
   */
//  def this {
//    this()
//    argGeom = g
//    this.bufParams = bufParams
//  }

  /**
   * Specifies the end cap style of the generated buffer.
   * The styles supported are {link BufferParameters#CAP_ROUND}, {link BufferParameters#CAP_FLAT}, and {link BufferParameters#CAP_SQUARE}.
   * The default is CAP_ROUND.
   *
   * @param endCapStyle the end cap style to specify
   */
  def setEndCapStyle(endCapStyle: Int): Unit = bufParams.setEndCapStyle(endCapStyle)

  /**
   * Sets the number of segments used to approximate a angle fillet
   *
   * @param quadrantSegments the number of segments in a fillet for a quadrant
   */
  def setQuadrantSegments(quadrantSegments: Int): Unit =
    bufParams.setQuadrantSegments(quadrantSegments)

  /**
   * Returns the buffer computed for a geometry for a given buffer distance.
   *
   * @param distance the buffer distance
   * return the buffer of the input geometry
   */
  def getResultGeometry(distance: Double): Geometry = {
    this.distance = distance
    computeGeometry()
    resultGeometry
  }

  private def computeGeometry(): Unit = {
    bufferOriginalPrecision()
    if (resultGeometry != null) return
    val argPM = argGeom.getFactory.getPrecisionModel
    if (argPM.getType eq PrecisionModel.FIXED) bufferFixedPrecision(argPM)
    else bufferReducedPrecision()
  }

  private def bufferReducedPrecision(): Unit = { // try and compute with decreasing precision
    var precDigits = BufferOp.MAX_PRECISION_DIGITS
    while (precDigits >= 0) {
      try bufferReducedPrecision(precDigits)
      catch {
        case ex: TopologyException =>
          // update the saved exception to reflect the new input geometry
          saveException = ex
        // don't propagate the exception - it will be detected by fact that resultGeometry is null
      }
      if (resultGeometry != null) return
      {
        precDigits -= 1; precDigits + 1
      }
    }
    // tried everything - have to bail
    throw saveException
  }

  private def bufferOriginalPrecision(): Unit = try { // use fast noding by default
    val bufBuilder = new BufferBuilder(bufParams)
    resultGeometry = bufBuilder.buffer(argGeom, distance)
  } catch {
    case ex: RuntimeException =>
      saveException = ex
    // testing ONLY - propagate exception
    //throw ex;
  }

  private def bufferReducedPrecision(precisionDigits: Int): Unit = {
    val sizeBasedScaleFactor = BufferOp.precisionScaleFactor(argGeom, distance, precisionDigits)
    //    System.out.println("recomputing with precision scale factor = " + sizeBasedScaleFactor);
    val fixedPM              = new PrecisionModel(sizeBasedScaleFactor)
    bufferFixedPrecision(fixedPM)
  }

  private def bufferFixedPrecision(fixedPM: PrecisionModel): Unit = {
    val noder      = new ScaledNoder(new MCIndexSnapRounder(new PrecisionModel(1.0)), fixedPM.getScale)
    val bufBuilder = new BufferBuilder(bufParams)
    bufBuilder.setWorkingPrecisionModel(fixedPM)
    bufBuilder.setNoder(noder)
    // this may throw an exception, if robustness errors are encountered
    resultGeometry = bufBuilder.buffer(argGeom, distance)
  }
}
