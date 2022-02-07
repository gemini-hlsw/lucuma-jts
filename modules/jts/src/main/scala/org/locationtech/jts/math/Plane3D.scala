// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2016 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */ /*
 * Copyright (c) 2016 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.math

import org.locationtech.jts.geom.Coordinate

/**
 * Models a plane in 3-dimensional Cartesian space.
 *
 * @author
 *   mdavis
 */
object Plane3D {

  /**
   * Enums for the 3 coordinate planes
   */
  val XY_PLANE = 1
  val YZ_PLANE = 2
  val XZ_PLANE = 3
}

class Plane3D(var normal: Vector3D, var basePt: Coordinate) {

  /**
   * Computes the oriented distance from a point to the plane. The distance is: <ul>
   * <li><b>positive</b> if the point lies above the plane (relative to the plane normal)
   * <li><b>zero</b> if the point is on the plane <li><b>negative</b> if the point lies below the
   * plane (relative to the plane normal) </ul>
   *
   * @param p
   *   the point to compute the distance for return the oriented distance to the plane
   */
  def orientedDistance(p: Coordinate): Double = {
    val pb           = new Vector3D(p, basePt)
    val pbdDotNormal = pb.dot(normal)
    if (java.lang.Double.isNaN(pbdDotNormal))
      throw new IllegalArgumentException("3D Coordinate has NaN ordinate")
    val d            = pbdDotNormal / normal.length
    d
  }

  /**
   * Computes the axis plane that this plane lies closest to. <p> Geometries lying in this plane
   * undergo least distortion (and have maximum area) when projected to the closest axis plane. This
   * provides optimal conditioning for computing a Point-in-Polygon test.
   *
   * return the index of the closest axis plane.
   */
  def closestAxisPlane: Int = {
    val xmag = Math.abs(normal.getX)
    val ymag = Math.abs(normal.getY)
    val zmag = Math.abs(normal.getZ)
    if (xmag > ymag)
      if (xmag > zmag) return Plane3D.YZ_PLANE
      else return Plane3D.XY_PLANE
    else { // y >= x
      if (zmag > ymag) return Plane3D.XY_PLANE
    }
    // y >= z
    Plane3D.XZ_PLANE
  }
}
