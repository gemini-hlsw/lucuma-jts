// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2018 Contributors to the Eclipse Foundation
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */ // Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2018 Contributors to the Eclipse Foundation
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

/**
 * Useful utility functions for handling Coordinate objects.
 */
object Coordinates {

  /**
   * Factory method providing access to common Coordinate implementations.
   *
   * @param dimension
   *   return created coordinate
   */
  def create(dimension: Int): Coordinate = create(dimension, 0)

  /**
   * Factory method providing access to common Coordinate implementations.
   *
   * @param dimension
   * @param measures
   *   return created coordinate
   */
  def create(dimension: Int, measures: Int): Coordinate = {
    if (dimension == 2) return new CoordinateXY(0.0, 0.0)
    else if (dimension == 3 && measures == 0) return new Coordinate()
    else if (dimension == 3 && measures == 1) return new CoordinateXYM(0.0, 0.0, 0.0)
    else if (dimension == 4 && measures == 1) return new CoordinateXYZM(0.0, 0.0, 0.0, 0.0)
    new Coordinate
  }

  /**
   * Determine dimension based on subclass of {link Coordinate}.
   *
   * @param coordinate
   *   supplied coordinate return number of ordinates recorded
   */
  def dimension(coordinate: Coordinate): Int = {
    if (coordinate.isInstanceOf[CoordinateXY]) return 2
    else if (coordinate.isInstanceOf[CoordinateXYM]) return 3
    else if (coordinate.isInstanceOf[CoordinateXYZM]) return 4
    else if (coordinate.isInstanceOf[Coordinate]) return 3
    3
  }

  /**
   * Determine number of measures based on subclass of {link Coordinate}.
   *
   * @param coordinate
   *   supplied coordinate return number of measures recorded
   */
  def measures(coordinate: Coordinate): Int = {
    if (coordinate.isInstanceOf[CoordinateXY]) return 0
    else if (coordinate.isInstanceOf[CoordinateXYM]) return 1
    else if (coordinate.isInstanceOf[CoordinateXYZM]) return 1
    else if (coordinate.isInstanceOf[Coordinate]) return 0
    0
  }
}
