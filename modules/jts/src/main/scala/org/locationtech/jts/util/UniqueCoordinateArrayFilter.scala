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
package org.locationtech.jts.util

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateFilter

import java.util

/**
 * A {link CoordinateFilter} that extracts a unique array of <code>Coordinate</code>s. The array of
 * coordinates contains no duplicate points. It preserves the order of the input points.
 *
 * @version 1.7
 */
object UniqueCoordinateArrayFilter {

  /**
   * Convenience method which allows running the filter over an array of {link Coordinate}s.
   *
   * @param coords
   *   an array of coordinates return an array of the unique coordinates
   */
  def filterCoordinates(coords: Array[Coordinate]): Array[Coordinate] = {
    val filter = new UniqueCoordinateArrayFilter
    var i      = 0
    while (i < coords.length) {
      filter.filter(coords(i))
      i += 1
    }
    filter.getCoordinates
  }
}

class UniqueCoordinateArrayFilter() extends CoordinateFilter {
  private val coordSet = new util.HashSet[Coordinate]
  // Use an auxiliary list as well in order to preserve coordinate order
  private val list     = new util.ArrayList[Coordinate]

  /**
   * Returns the gathered <code>Coordinate</code>s.
   *
   * return the <code>Coordinate</code>s collected by this <code>CoordinateArrayFilter</code>
   */
  def getCoordinates: Array[Coordinate] = {
    val coordinates = new Array[Coordinate](list.size)
    list.toArray(coordinates).asInstanceOf[Array[Coordinate]]
  }

  /**
   * @see
   *   CoordinateFilter#filter(Coordinate)
   */
  def filter(coord: Coordinate): Unit = {
    if (coordSet.add(coord)) list.add(coord)
    ()
  }
}
