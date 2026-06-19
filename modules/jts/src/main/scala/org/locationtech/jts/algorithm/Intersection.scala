// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2019 martin Davis
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.algorithm

import org.locationtech.jts.geom.Coordinate

/**
 * Contains functions to compute intersections between lines.
 *
 * @author
 *   Martin Davis
 */
object Intersection {

  /**
   * Computes the intersection point of two lines. If the lines are parallel or collinear this case
   * is detected and <code>null</code> is returned.
   *
   * Delegates to the double-double precision computation, matching upstream JTS 1.20: the earlier
   * floating-point version caused spatial predicate failures in some cases.
   *
   * @param p1
   *   an endpoint of line 1
   * @param p2
   *   an endpoint of line 1
   * @param q1
   *   an endpoint of line 2
   * @param q2
   *   an endpoint of line 2 return the intersection point between the lines, if there is one, or
   *   null if the lines are parallel or collinear
   * @see
   *   CGAlgorithmsDD#intersection(Coordinate, Coordinate, Coordinate, Coordinate)
   */
  def intersection(p1: Coordinate, p2: Coordinate, q1: Coordinate, q2: Coordinate): Coordinate =
    CGAlgorithmsDD.intersection(p1, p2, q1, q2)
}
