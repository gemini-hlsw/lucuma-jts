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
package org.locationtech.jts.operation.buffer.validate

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.MultiPolygon
import org.locationtech.jts.geom.Polygon

/**
 * Validates that the result of a buffer operation is geometrically correct, within a computed
 * tolerance. <p> This is a heuristic test, and may return false positive results (I.e. it may fail
 * to detect an invalid result.) It should never return a false negative result, however (I.e. it
 * should never report a valid result as invalid.) <p> This test may be (much) more expensive than
 * the original buffer computation.
 *
 * @author
 *   Martin Davis
 */
object BufferResultValidator {
  private val VERBOSE = false

  /**
   * Maximum allowable fraction of buffer distance the actual distance can differ by. 1% sometimes
   * causes an error - 1.2% should be safe.
   */
  private val MAX_ENV_DIFF_FRAC = .012

  def isValid(g: Geometry, distance: Double, result: Geometry): Boolean = {
    val validator = new BufferResultValidator(g, distance, result)
    if (validator.isValid) return true
    false
  }

  /**
   * Checks whether the geometry buffer is valid, and returns an error message if not.
   *
   * @param g
   * @param distance
   * @param result
   *   return an appropriate error message or null if the buffer is valid
   */
  def isValidMsg(g: Geometry, distance: Double, result: Geometry): String = {
    val validator = new BufferResultValidator(g, distance, result)
    if (!validator.isValid) return validator.getErrorMessage
    null
  }
}

class BufferResultValidator(var input: Geometry, var distance: Double, var result: Geometry) {
  private var visValid                  = true
  private var errorMsg: String          = null
  private var errorLocation: Coordinate = null
  private var errorIndicator: Geometry  = null

  def isValid: Boolean = {
    checkPolygonal()
    if (!visValid) return visValid
    checkExpectedEmpty()
    if (!visValid) return visValid
    checkEnvelope()
    if (!visValid) return visValid
    checkArea()
    if (!visValid) return visValid
    checkDistance()
    visValid
  }

  def getErrorMessage: String = errorMsg

  def getErrorLocation: Coordinate = errorLocation

  /**
   * Gets a geometry which indicates the location and nature of a validation failure. <p> If the
   * failure is due to the buffer curve being too far or too close to the input, the indicator is a
   * line segment showing the location and size of the discrepancy.
   *
   * return a geometric error indicator or null if no error was found
   */
  def getErrorIndicator: Geometry = errorIndicator

  private def report(checkName: String): Unit = {
    if (!BufferResultValidator.VERBOSE) return
    System.out.println(
      "Check " + checkName + ": " + (if (visValid) "passed"
                                     else "FAILED")
    )
  }

  private def checkPolygonal(): Unit = {
    if (!(result.isInstanceOf[Polygon] || result.isInstanceOf[MultiPolygon])) visValid = false
    errorMsg = "Result is not polygonal"
    errorIndicator = result
    report("Polygonal")
  }

  private def checkExpectedEmpty(): Unit = { // can't check areal features
    if (input.getDimension >= 2) return
    // can't check positive distances
    if (distance > 0.0) return
    // at this point can expect an empty result
    if (!result.isEmpty) {
      visValid = false
      errorMsg = "Result is non-empty"
      errorIndicator = result
    }
    report("ExpectedEmpty")
  }

  private def checkEnvelope(): Unit = {
    if (distance < 0.0) return
    var padding     = distance * BufferResultValidator.MAX_ENV_DIFF_FRAC
    if (padding == 0.0) padding = 0.001
    val expectedEnv = new Envelope(input.getEnvelopeInternal)
    expectedEnv.expandBy(distance)
    val bufEnv      = new Envelope(result.getEnvelopeInternal)
    bufEnv.expandBy(padding)
    if (!bufEnv.contains(expectedEnv)) {
      visValid = false
      errorMsg = "Buffer envelope is incorrect"
      errorIndicator = input.getFactory.toGeometry(bufEnv)
    }
    report("Envelope")
  }

  private def checkArea(): Unit = {
    val inputArea  = input.getArea
    val resultArea = result.getArea
    if (distance > 0.0 && inputArea > resultArea) {
      visValid = false
      errorMsg = "Area of positive buffer is smaller than input"
      errorIndicator = result
    }
    if (distance < 0.0 && inputArea < resultArea) {
      visValid = false
      errorMsg = "Area of negative buffer is larger than input"
      errorIndicator = result
    }
    report("Area")
  }

  private def checkDistance(): Unit = {
    val distValid = new BufferDistanceValidator(input, distance, result)
    if (!distValid.isValid) {
      visValid = false
      errorMsg = distValid.getErrorMessage
      errorLocation = distValid.getErrorLocation
      errorIndicator = distValid.getErrorIndicator
    }
    report("Distance")
  }
}
