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
package org.locationtech.jts.geom.util

import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryCollection

/**
 * A visitor to {link Geometry} components, which allows short-circuiting when a defined condition
 * holds.
 *
 * @version 1.7
 */
abstract class ShortCircuitedGeometryVisitor() {
  private var visDone = false

  def applyTo(geom: Geometry): Unit = {
    var i = 0
    while (i < geom.getNumGeometries && !visDone) {
      val element = geom.getGeometryN(i)
      if (!element.isInstanceOf[GeometryCollection]) {
        visit(element)
        if (visDone) {
          visDone = true
          return
        }
      } else applyTo(element)
      i += 1
    }
  }

  protected def visit(element: Geometry): Unit

  /**
   * Reports whether visiting components can be terminated. Once this method returns <tt>true</tt>,
   * it must continue to return <tt>true</tt> on every subsequent call.
   *
   * return true if visiting can be terminated.
   */
  protected def isDone: Boolean
}
