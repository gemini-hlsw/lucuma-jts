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
package org.locationtech.jts.geom.impl

import java.io.Serializable
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateSequence
import org.locationtech.jts.geom.CoordinateSequenceFactory

/**
 * Creates {link CoordinateSequence}s represented as an array of {link Coordinate}s.
 *
 * @version 1.7
 */
@SerialVersionUID(-4099577099607551657L)
object CoordinateArraySequenceFactory {
  private val instanceObject = new CoordinateArraySequenceFactory

  /**
   * Returns the singleton instance of {link CoordinateArraySequenceFactory}
   */
  def instance: CoordinateArraySequenceFactory = instanceObject
}

@SerialVersionUID(-4099577099607551657L)
final class CoordinateArraySequenceFactory private ()
    extends CoordinateSequenceFactory
    with Serializable {
  private def readResolve = // http://www.javaworld.com/javaworld/javatips/jw-javatip122.html
    CoordinateArraySequenceFactory.instance

  /**
   * Returns a {link CoordinateArraySequence} based on the given array (the array is
   * not copied).
   *
   * @param coordinates
   * the coordinates, which may not be null nor contain null
   * elements
   */
  def create(coordinates: Array[Coordinate]): CoordinateSequence = new CoordinateArraySequence(
    coordinates
  )

  /**
   * @see org.locationtech.jts.geom.CoordinateSequenceFactory#create(org.locationtech.jts.geom.CoordinateSequence)
   */
  def create(coordSeq: CoordinateSequence): CoordinateSequence = new CoordinateArraySequence(
    coordSeq
  )

  /**
   * The created sequence dimension is clamped to be &lt;= 3.
   *
   * @see org.locationtech.jts.geom.CoordinateSequenceFactory#create(int, int)
   */
  def create(size: Int, dimension: Int): CoordinateSequence = {
    val dim =
      if (dimension > 3) 3
      //throw new IllegalArgumentException("dimension must be <= 3");
      // handle bogus dimension
      else if (dimension < 2) 2
      else dimension
    new CoordinateArraySequence(size, dim)
  }

  override def create(size: Int, dimension: Int, measures: Int): CoordinateSequence = {
    var spatial = dimension - measures
    val meas    = if (measures > 1) {
      1 // clip measures
      //throw new IllegalArgumentException("measures must be <= 1");
    } else measures
    if (spatial > 3) {
      spatial = 3 // clip spatial dimension
      //throw new IllegalArgumentException("spatial dimension must be <= 3");
    }
    if (spatial < 2) spatial = 2 // handle bogus spatial dimension
    new CoordinateArraySequence(size, spatial + meas, meas)
  }
}
