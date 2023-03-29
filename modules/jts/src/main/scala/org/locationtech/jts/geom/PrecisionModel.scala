// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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
 */
package org.locationtech.jts.geom

import java.io.Serializable

/**
 * Specifies the precision model of the {link Coordinate}s in a {link Geometry}. In other words,
 * specifies the grid of allowable points for all <code>Geometry</code>s. <p> The {link
 * #makePrecise(Coordinate)} method allows rounding a coordinate to a "precise" value; that is, one
 * whose precision is known exactly. <p> Coordinates are assumed to be precise in geometries. That
 * is, the coordinates are assumed to be rounded to the precision model given for the geometry. JTS
 * input routines automatically round coordinates to the precision model before creating Geometries.
 * All internal operations assume that coordinates are rounded to the precision model. Constructive
 * methods (such as boolean operations) always round computed coordinates to the appropriate
 * precision model. <p> Currently three types of precision model are supported: <ul> <li>FLOATING -
 * represents full double precision floating point. This is the default precision model used in JTS
 * <li>FLOATING_SINGLE - represents single precision floating point. <li>FIXED - represents a model
 * with a fixed number of decimal places. A Fixed Precision Model is specified by a scale factor.
 * The scale factor specifies the size of the grid which numbers are rounded to. Input coordinates
 * are mapped to fixed coordinates according to the following equations: <UL> <LI> jtsPt.x = round(
 * (inputPt.x * scale ) / scale <LI> jtsPt.y = round( (inputPt.y * scale ) / scale </UL> </ul> For
 * example, to specify 3 decimal places of precision, use a scale factor of 1000. To specify -3
 * decimal places of precision (i.e. rounding to the nearest 1000), use a scale factor of 0.001. <p>
 * Coordinates are represented internally as Java double-precision values. Since Java uses the
 * IEEE-394 floating point standard, this provides 53 bits of precision. (Thus the maximum precisely
 * representable <i>integer</i> is 9,007,199,254,740,992 - or almost 16 decimal digits of
 * precision). <p> JTS binary methods currently do not handle inputs which have different precision
 * models. The precision model of any constructed geometric value is undefined.
 *
 * @version 1.7
 */
@SerialVersionUID(7777263578777803835L)
object PrecisionModel {

  /**
   * Determines which of two {link PrecisionModel}s is the most precise (allows the greatest number
   * of significant digits).
   *
   * @param pm1
   *   a PrecisionModel
   * @param pm2
   *   a PrecisionModel return the PrecisionModel which is most precise
   */
  def mostPrecise(pm1: PrecisionModel, pm2: PrecisionModel): PrecisionModel = {
    if (pm1.compareTo(pm2) >= 0) return pm1
    pm2
  }

  /**
   * The types of Precision Model which JTS supports.
   */
  @SerialVersionUID(-5528602631731589822L)
  object Type {
    private val nameToTypeMap = new java.util.HashMap[String, Type]
  }

  @SerialVersionUID(-5528602631731589822L)
  class Type(var name: String) extends Serializable {
    Type.nameToTypeMap.put(name, this)

    override def toString: String = name

    /*
     * Ssee http://www.javaworld.com/javaworld/javatips/jw-javatip122.html
     */
    private def readResolve = Type.nameToTypeMap.get(name)
  }

  /**
   * Fixed Precision indicates that coordinates have a fixed number of decimal places. The number of
   * decimal places is determined by the log10 of the scale factor.
   */
  val FIXED = new PrecisionModel.Type("FIXED")

  /**
   * Floating precision corresponds to the standard Java double-precision floating-point
   * representation, which is based on the IEEE-754 standard
   */
  val FLOATING = new PrecisionModel.Type("FLOATING")

  /**
   * Floating single precision corresponds to the standard Java single-precision floating-point
   * representation, which is based on the IEEE-754 standard
   */
  val FLOATING_SINGLE = new PrecisionModel.Type("FLOATING SINGLE")

  /**
   * The maximum precise value representable in a double. Since IEE754 double-precision numbers
   * allow 53 bits of mantissa, the value is equal to 2^53 - 1. This provides <i>almost</i> 16
   * decimal digits of precision.
   */
  val maximumPreciseValue = 9007199254740992.0
}

@SerialVersionUID(7777263578777803835L)
class PrecisionModel() extends Serializable with Comparable[PrecisionModel] { // default is floating precision
  /**
   * The type of PrecisionModel this represents.
   */
  private var modelType = PrecisionModel.FLOATING

  /**
   * The scale factor which determines the number of decimal places in fixed precision.
   */
  private var scale = .0

  /**
   * Creates a <code>PrecisionModel</code> that specifies an explicit precision model type. If the
   * model type is FIXED the scale factor will default to 1.
   *
   * @param modelType
   *   the type of the precision model
   */
  def this(modelType: PrecisionModel.Type) = {
    this()
    this.modelType = modelType
    if (modelType eq PrecisionModel.FIXED) setScale(1.0)
  }

  /**
   * Creates a <code>PrecisionModel</code> that specifies Fixed precision. Fixed-precision
   * coordinates are represented as precise internal coordinates, which are rounded to the grid
   * defined by the scale factor.
   *
   * @param scale
   *   amount by which to multiply a coordinate after subtracting the offset, to obtain a precise
   *   coordinate
   * @param offsetX
   *   not used.
   * @param offsetY
   *   not used.
   * @deprecated
   *   offsets are no longer supported, since internal representation is rounded floating point
   */
  def this(scale: Double, offsetX: Double, offsetY: Double) = {
    this()
    modelType = PrecisionModel.FIXED
    setScale(scale)
  }

  /**
   * Creates a <code>PrecisionModel</code> that specifies Fixed precision. Fixed-precision
   * coordinates are represented as precise internal coordinates, which are rounded to the grid
   * defined by the scale factor.
   *
   * @param scale
   *   amount by which to multiply a coordinate after subtracting the offset, to obtain a precise
   *   coordinate
   */
  def this(scale: Double) = {
    this()
    modelType = PrecisionModel.FIXED
    setScale(scale)
  }

  /**
   * Copy constructor to create a new <code>PrecisionModel</code> from an existing one.
   */
  def this(pm: PrecisionModel) = {
    this()
    modelType = pm.modelType
    scale = pm.scale
  }

  /**
   * Tests whether the precision model supports floating point
   *
   * return <code>true</code> if the precision model supports floating point
   */
  def isFloating: Boolean =
    (modelType eq PrecisionModel.FLOATING) || (modelType eq PrecisionModel.FLOATING_SINGLE)

  /**
   * Returns the maximum number of significant digits provided by this precision model. Intended for
   * use by routines which need to print out decimal representations of precise values (such as
   * {link WKTWriter}). <p> This method would be more correctly called
   * <tt>getMinimumDecimalPlaces</tt>, since it actually computes the number of decimal places that
   * is required to correctly display the full precision of an ordinate value. <p> Since it is
   * difficult to compute the required number of decimal places for scale factors which are not
   * powers of 10, the algorithm uses a very rough approximation in this case. This has the side
   * effect that for scale factors which are powers of 10 the value returned is 1 greater than the
   * true value.
   *
   * return the maximum number of decimal places provided by this precision model
   */
  def getMaximumSignificantDigits: Int = {
    var maxSigDigits = 16
    if (modelType eq PrecisionModel.FLOATING) maxSigDigits = 16
    else if (modelType eq PrecisionModel.FLOATING_SINGLE) maxSigDigits = 6
    else if (modelType eq PrecisionModel.FIXED)
      maxSigDigits = 1 + Math.ceil(Math.log(getScale) / Math.log(10)).toInt
    maxSigDigits
  }

  /**
   * Returns the scale factor used to specify a fixed precision model. The number of decimal places
   * of precision is equal to the base-10 logarithm of the scale factor. Non-integral and negative
   * scale factors are supported. Negative scale factors indicate that the places of precision is to
   * the left of the decimal point.
   *
   * return the scale factor for the fixed precision model
   */
  def getScale: Double = scale

  /**
   * Gets the type of this precision model
   *
   * return the type of this precision model
   * @see
   *   Type
   */
  def getType: PrecisionModel.Type = modelType

  /**
   * Sets the multiplying factor used to obtain a precise coordinate. This method is private because
   * PrecisionModel is an immutable (value) type.
   */
  private def setScale(scale: Double): Unit = this.scale = Math.abs(scale)

  /**
   * Returns the x-offset used to obtain a precise coordinate.
   *
   * return the amount by which to subtract the x-coordinate before multiplying by the scale
   * @deprecated
   *   Offsets are no longer used
   */
  def getOffsetX: Int = // We actually don't use offsetX and offsetY anymore ... [Jon Aquino]
    0

  /**
   * Returns the y-offset used to obtain a precise coordinate.
   *
   * return the amount by which to subtract the y-coordinate before multiplying by the scale
   * @deprecated
   *   Offsets are no longer used
   */
  def getOffsetY = 0

  /**
   * Sets <code>internal</code> to the precise representation of <code>external</code>.
   *
   * @param external
   *   the original coordinate
   * @param internal
   *   the coordinate whose values will be changed to the precise representation of
   *   <code>external</code>
   * @deprecated
   *   use makePrecise instead
   */
  def toInternal(external: Coordinate, internal: Coordinate) = {
    if (isFloating) {
      internal.x = external.x
      internal.y = external.y
    } else {
      internal.x = makePrecise(external.x)
      internal.y = makePrecise(external.y)
    }
    internal.setZ(external.getZ)
  }

  /**
   * Returns the precise representation of <code>external</code>.
   *
   * @param external
   *   the original coordinate return the coordinate whose values will be changed to the precise
   *   representation of <code>external</code>
   * @deprecated
   *   use makePrecise instead
   */
  def toInternal(external: Coordinate) = {
    val internal = new Coordinate(external)
    makePrecise(internal)
    internal
  }

  /**
   * Returns the external representation of <code>internal</code>.
   *
   * @param internal
   *   the original coordinate return the coordinate whose values will be changed to the external
   *   representation of <code>internal</code>
   * @deprecated
   *   no longer needed, since internal representation is same as external representation
   */
  def toExternal(internal: Coordinate) = {
    val external = new Coordinate(internal)
    external
  }

  /**
   * Sets <code>external</code> to the external representation of <code>internal</code>.
   *
   * @param internal
   *   the original coordinate
   * @param external
   *   the coordinate whose values will be changed to the external representation of
   *   <code>internal</code>
   * @deprecated
   *   no longer needed, since internal representation is same as external representation
   */
  def toExternal(internal: Coordinate, external: Coordinate): Unit = {
    external.x = internal.x
    external.y = internal.y
  }

  /**
   * Rounds a numeric value to the PrecisionModel grid. Asymmetric Arithmetic Rounding is used, to
   * provide uniform rounding behaviour no matter where the number is on the number line. <p> This
   * method has no effect on NaN values. <p> <b>Note:</b> Java's <code>Math#rint</code> uses the
   * "Banker's Rounding" algorithm, which is not suitable for precision operations elsewhere in JTS.
   */
  def makePrecise(value: Double): Double = { // don't change NaN values
    if (java.lang.Double.isNaN(value)) return value
    if (modelType == PrecisionModel.FLOATING_SINGLE) {
      val floatSingleVal = value.toFloat
      return floatSingleVal.toDouble
    }
    if (modelType == PrecisionModel.FIXED) {
      return (value * scale).round / scale
    }
    value
  }

  /**
   * Rounds a Coordinate to the PrecisionModel grid.
   */
  def makePrecise(coord: Coordinate): Unit = { // optimization for full precision
    if (modelType eq PrecisionModel.FLOATING) return
    coord.x = makePrecise(coord.x)
    coord.y = makePrecise(coord.y)
    // MD says it's OK that we're not makePrecise'ing the z [Jon Aquino]
  }

  override def toString: String = {
    var description = "UNKNOWN"
    if (modelType eq PrecisionModel.FLOATING) description = "Floating"
    else if (modelType eq PrecisionModel.FLOATING_SINGLE) description = "Floating-Single"
    else if (modelType eq PrecisionModel.FIXED) description = "Fixed (Scale=" + getScale + ")"
    description
  }

  override def equals(other: Any): Boolean = {
    if (!other.isInstanceOf[PrecisionModel]) return false
    val otherPrecisionModel = other.asInstanceOf[PrecisionModel]
    (modelType eq otherPrecisionModel.modelType) && scale == otherPrecisionModel.scale
  }

  /**
   * Compares this {link PrecisionModel} object with the specified object for order. A
   * PrecisionModel is greater than another if it provides greater precision. The comparison is
   * based on the value returned by the {link #getMaximumSignificantDigits} method. This comparison
   * is not strictly accurate when comparing floating precision models to fixed models; however, it
   * is correct when both models are either floating or fixed.
   *
   * @param o
   *   the <code>PrecisionModel</code> with which this <code>PrecisionModel</code> is being compared
   *   return a negative integer, zero, or a positive integer as this <code>PrecisionModel</code> is
   *   less than, equal to, or greater than the specified <code>PrecisionModel</code>
   */
  override def compareTo(o: PrecisionModel): Int = {
    val other          = o
    val sigDigits      = getMaximumSignificantDigits
    val otherSigDigits = other.getMaximumSignificantDigits
    Integer.compare(sigDigits, otherSigDigits)
    //    if (sigDigits > otherSigDigits)
    //      return 1;
    //    else if
    //    if (modelType == FLOATING && other.modelType == FLOATING) return 0;
    //    if (modelType == FLOATING && other.modelType != FLOATING) return 1;
    //    if (modelType != FLOATING && other.modelType == FLOATING) return -1;
    //    if (modelType == FIXED && other.modelType == FIXED) {
    //      if (scale > other.scale)
    //        return 1;
    //      else if (scale < other.scale)
    //        return -1;
    //      else
    //        return 0;
    //    }
    //    Assert.shouldNeverReachHere("Unknown Precision Model type encountered");
    //    return 0;
  }
}
