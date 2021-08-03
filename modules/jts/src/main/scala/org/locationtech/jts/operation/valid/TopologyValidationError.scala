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
package org.locationtech.jts.operation.valid

import org.locationtech.jts.geom.Coordinate

/**
 * Contains information about the nature and location of a {link Geometry}
 * validation error
 *
 * @version 1.7
 */
object TopologyValidationError {

  /**
   * Not used
   *
   * @deprecated
   */
  val ERROR = 0

  /**
   * No longer used - repeated points are considered valid as per the SFS
   *
   * @deprecated
   */
  val REPEATED_POINT = 1

  /**
   * Indicates that a hole of a polygon lies partially or completely in the exterior of the shell
   */
  val HOLE_OUTSIDE_SHELL = 2

  /**
   * Indicates that a hole lies in the interior of another hole in the same polygon
   */
  val NESTED_HOLES = 3

  /**
   * Indicates that the interior of a polygon is disjoint
   * (often caused by set of contiguous holes splitting the polygon into two parts)
   */
  val DISCONNECTED_INTERIOR = 4

  /**
   * Indicates that two rings of a polygonal geometry intersect
   */
  val SELF_INTERSECTION = 5

  /**
   * Indicates that a ring self-intersects
   */
  val RING_SELF_INTERSECTION = 6

  /**
   * Indicates that a polygon component of a MultiPolygon lies inside another polygonal component
   */
  val NESTED_SHELLS = 7

  /**
   * Indicates that a polygonal geometry contains two rings which are identical
   */
  val DUPLICATE_RINGS = 8

  /**
   * Indicates that either
   * <ul>
   * <li>a LineString contains a single point
   * <li>a LinearRing contains 2 or 3 points
   * </ul>
   */
  val TOO_FEW_POINTS     = 9
  // /**
  //  * Indicates that the <code>X</code> or <code>Y</code> ordinate of
  //  * a Coordinate is not a valid numeric value (e.g. {link Double#NaN} )
  //  */
  val INVALID_COORDINATE = 10

  /**
   * Indicates that a ring is not correctly closed
   * (the first and the last coordinate are different)
   */
  val RING_NOT_CLOSED = 11

  /**
   * Messages corresponding to error codes
   */
  val errMsg = Array(
    "Topology Validation Error",
    "Repeated Point",
    "Hole lies outside shell",
    "Holes are nested",
    "Interior is disconnected",
    "Self-intersection",
    "Ring Self-intersection",
    "Nested shells",
    "Duplicate Rings",
    "Too few distinct points in geometry component",
    "Invalid Coordinate",
    "Ring is not closed"
  )
}

class TopologyValidationError(var errorType: Int, var pt: Coordinate) {

  /**
   * Creates a validation error with the given type and location
   *
   * @param errorType the type of the error
   * @param pt        the location of the error
   */
  if (pt != null) this.pt = pt.copy

  /**
   * Creates a validation error of the given type with a null location
   *
   * @param errorType the type of the error
   */
  def this(errorType: Int) = {
    this(errorType, null)
  }

  // /**
  //  * Returns the location of this error (on the {link Geometry} containing the error).
  //  *
  //  * return a { @link Coordinate} on the input geometry
  //  */
  def getCoordinate: Coordinate = pt

  /**
   * Gets the type of this error.
   *
   * return the error type
   */
  def getErrorType: Int = errorType

  /**
   * Gets an error message describing this error.
   * The error message does not describe the location of the error.
   *
   * return the error message
   */
  def getMessage: String = TopologyValidationError.errMsg(errorType)

  /**
   * Gets a message describing the type and location of this error.
   *
   * return the error message
   */
  override def toString: String = {
    var locStr = ""
    if (pt != null) locStr = " at or near point " + pt
    getMessage + locStr
  }
}
