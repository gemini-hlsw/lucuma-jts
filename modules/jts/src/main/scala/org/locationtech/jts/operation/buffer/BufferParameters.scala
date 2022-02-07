// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
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
package org.locationtech.jts.operation.buffer

/**
 * A value class containing the parameters which specify how a buffer should be constructed. <p> The
 * parameters allow control over: <ul> <li>Quadrant segments (accuracy of approximation for circular
 * arcs) <li>End Cap style <li>Join style <li>Mitre limit <li>whether the buffer is single-sided
 * </ul>
 *
 * @author
 *   Martin Davis
 */
object BufferParameters {

  /**
   * Specifies a round line buffer end cap style.
   */
  val CAP_ROUND = 1

  /**
   * Specifies a flat line buffer end cap style.
   */
  val CAP_FLAT = 2

  /**
   * Specifies a square line buffer end cap style.
   */
  val CAP_SQUARE = 3

  /**
   * Specifies a round join style.
   */
  val JOIN_ROUND = 1

  /**
   * Specifies a mitre join style.
   */
  val JOIN_MITRE = 2

  /**
   * Specifies a bevel join style.
   */
  val JOIN_BEVEL = 3

  /**
   * The default number of facets into which to divide a fillet of 90 degrees. A value of 8 gives
   * less than 2% max error in the buffer distance. For a max error of &lt; 1%, use QS = 12. For a
   * max error of &lt; 0.1%, use QS = 18.
   */
  val DEFAULT_QUADRANT_SEGMENTS = 8

  /**
   * The default mitre limit Allows fairly pointy mitres.
   */
  val DEFAULT_MITRE_LIMIT = 5.0

  /**
   * The default simplify factor Provides an accuracy of about 1%, which matches the accuracy of the
   * default Quadrant Segments parameter.
   */
  val DEFAULT_SIMPLIFY_FACTOR = 0.01

  /**
   * Computes the maximum distance error due to a given level of approximation to a true arc.
   *
   * @param quadSegs
   *   the number of segments used to approximate a quarter-circle return the error of approximation
   */
  def bufferDistanceError(quadSegs: Int): Double = {
    val alpha = Math.PI / 2.0 / quadSegs
    1 - Math.cos(alpha / 2.0)
  }
}

class BufferParameters() {

  /**
   * Creates a default set of parameters
   */
  private var quadrantSegments = BufferParameters.DEFAULT_QUADRANT_SEGMENTS
  private var endCapStyle      = BufferParameters.CAP_ROUND
  private var joinStyle        = BufferParameters.JOIN_ROUND
  private var mitreLimit       = BufferParameters.DEFAULT_MITRE_LIMIT
  private var visSingleSided   = false
  private var simplifyFactor   = BufferParameters.DEFAULT_SIMPLIFY_FACTOR

  /**
   * Creates a set of parameters with the given quadrantSegments value.
   *
   * @param quadrantSegments
   *   the number of quadrant segments to use
   */
  def this(quadrantSegments: Int) = {
    this()
    setQuadrantSegments(quadrantSegments)
  }

  /**
   * Creates a set of parameters with the given quadrantSegments and endCapStyle values.
   *
   * @param quadrantSegments
   *   the number of quadrant segments to use
   * @param endCapStyle
   *   the end cap style to use
   */
  def this(quadrantSegments: Int, endCapStyle: Int) = {
    this()
    setQuadrantSegments(quadrantSegments)
    setEndCapStyle(endCapStyle)
  }

  /**
   * Creates a set of parameters with the given parameter values.
   *
   * @param quadrantSegments
   *   the number of quadrant segments to use
   * @param endCapStyle
   *   the end cap style to use
   * @param joinStyle
   *   the join style to use
   * @param mitreLimit
   *   the mitre limit to use
   */
  def this(quadrantSegments: Int, endCapStyle: Int, joinStyle: Int, mitreLimit: Double) = {
    this()
    setQuadrantSegments(quadrantSegments)
    setEndCapStyle(endCapStyle)
    setJoinStyle(joinStyle)
    setMitreLimit(mitreLimit)
  }

  /**
   * Gets the number of quadrant segments which will be used
   *
   * return the number of quadrant segments
   */
  def getQuadrantSegments: Int = quadrantSegments

  /**
   * Sets the number of line segments used to approximate an angle fillet. <ul> <li>If
   * <tt>quadSegs</tt> &gt;= 1, joins are round, and <tt>quadSegs</tt> indicates the number of
   * segments to use to approximate a quarter-circle. <li>If <tt>quadSegs</tt> = 0, joins are
   * bevelled (flat) <li>If <tt>quadSegs</tt> &lt; 0, joins are mitred, and the value of qs
   * indicates the mitre ration limit as <pre> mitreLimit = |<tt>quadSegs</tt>| </pre> </ul> For
   * round joins, <tt>quadSegs</tt> determines the maximum error in the approximation to the true
   * buffer curve. The default value of 8 gives less than 2% max error in the buffer distance. For a
   * max error of &lt; 1%, use QS = 12. For a max error of &lt; 0.1%, use QS = 18. The error is
   * always less than the buffer distance (in other words, the computed buffer curve is always
   * inside the true curve).
   *
   * @param quadSegs
   *   the number of segments in a fillet for a quadrant
   */
  def setQuadrantSegments(quadSegs: Int): Unit = {
    quadrantSegments = quadSegs

    /**
     * Indicates how to construct fillets. If qs >= 1, fillet is round, and qs indicates number of
     * segments to use to approximate a quarter-circle. If qs = 0, fillet is bevelled flat (i.e. no
     * filleting is performed) If qs < 0, fillet is mitred, and absolute value of qs indicates
     * maximum length of mitre according to
     *
     * mitreLimit = |qs|
     */
    if (quadrantSegments == 0) joinStyle = BufferParameters.JOIN_BEVEL
    if (quadrantSegments < 0) {
      joinStyle = BufferParameters.JOIN_MITRE
      mitreLimit = Math.abs(quadrantSegments).toDouble
    }
    if (quadSegs <= 0) quadrantSegments = 1

    /**
     * If join style was set by the quadSegs value, use the default for the actual quadrantSegments
     * value.
     */
    if (joinStyle != BufferParameters.JOIN_ROUND)
      quadrantSegments = BufferParameters.DEFAULT_QUADRANT_SEGMENTS
  }

  /**
   * Gets the end cap style.
   *
   * return the end cap style
   */
  def getEndCapStyle: Int = endCapStyle

  // /**
  //  * Specifies the end cap style of the generated buffer.
  //  * The styles supported are {link #CAP_ROUND}, {link #CAP_FLAT}, and {link #CAP_SQUARE}.
  //  * The default is CAP_ROUND.
  //  *
  //  * @param endCapStyle the end cap style to specify
  //  */
  def setEndCapStyle(endCapStyle: Int): Unit = this.endCapStyle = endCapStyle

  /**
   * Gets the join style
   *
   * return the join style code
   */
  def getJoinStyle: Int = joinStyle

  // /**
  //  * Sets the join style for outside (reflex) corners between line segments.
  //  * Allowable values are {link #JOIN_ROUND} (which is the default),
  //  * {link #JOIN_MITRE} and {link JOIN_BEVEL}.
  //  *
  //  * @param joinStyle the code for the join style
  //  */
  def setJoinStyle(joinStyle: Int): Unit = this.joinStyle = joinStyle

  /**
   * Gets the mitre ratio limit.
   *
   * return the limit value
   */
  def getMitreLimit: Double = mitreLimit

  /**
   * Sets the limit on the mitre ratio used for very sharp corners. The mitre ratio is the ratio of
   * the distance from the corner to the end of the mitred offset corner. When two line segments
   * meet at a sharp angle, a miter join will extend far beyond the original geometry. (and in the
   * extreme case will be infinitely far.) To prevent unreasonable geometry, the mitre limit allows
   * controlling the maximum length of the join corner. Corners with a ratio which exceed the limit
   * will be beveled.
   *
   * @param mitreLimit
   *   the mitre ratio limit
   */
  def setMitreLimit(mitreLimit: Double): Unit = this.mitreLimit = mitreLimit

  /**
   * Sets whether the computed buffer should be single-sided. A single-sided buffer is constructed
   * on only one side of each input line. <p> The side used is determined by the sign of the buffer
   * distance: <ul> <li>a positive distance indicates the left-hand side <li>a negative distance
   * indicates the right-hand side </ul> The single-sided buffer of point geometries is the same as
   * the regular buffer. <p> The End Cap Style for single-sided buffers is always ignored, and
   * forced to the equivalent of <tt>CAP_FLAT</tt>.
   *
   * @param isSingleSided
   *   true if a single-sided buffer should be constructed
   */
  def setSingleSided(isSingleSided: Boolean): Unit = this.visSingleSided = isSingleSided

  /**
   * Tests whether the buffer is to be generated on a single side only.
   *
   * return true if the generated buffer is to be single-sided
   */
  def isSingleSided: Boolean = visSingleSided

  /**
   * Gets the simplify factor.
   *
   * return the simplify factor
   */
  def getSimplifyFactor: Double = simplifyFactor

  /**
   * Sets the factor used to determine the simplify distance tolerance for input simplification.
   * Simplifying can increase the performance of computing buffers. Generally the simplify factor
   * should be greater than 0. Values between 0.01 and .1 produce relatively good accuracy for the
   * generate buffer. Larger values sacrifice accuracy in return for performance.
   *
   * @param simplifyFactor
   *   a value greater than or equal to zero.
   */
  def setSimplifyFactor(simplifyFactor: Double): Unit = this.simplifyFactor =
    if (simplifyFactor < 0) 0
    else simplifyFactor
}
