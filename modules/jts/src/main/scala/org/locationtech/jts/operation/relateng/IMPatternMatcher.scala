// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2023 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v20.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.operation.relateng

import org.locationtech.jts.geom.Dimension
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.IntersectionMatrix
import org.locationtech.jts.geom.Location

object IMPatternMatcher {

  private def requireInteraction(im: IntersectionMatrix): Boolean = {
    val requiresInteraction =
      isInteraction(im.get(Location.INTERIOR, Location.INTERIOR))
        || isInteraction(im.get(Location.INTERIOR, Location.BOUNDARY))
        || isInteraction(im.get(Location.BOUNDARY, Location.INTERIOR))
        || isInteraction(im.get(Location.BOUNDARY, Location.BOUNDARY))
    requiresInteraction
  }

  private def isInteraction(imDim: Int): Boolean =
    imDim == Dimension.TRUE || imDim >= Dimension.P
}

/**
 * A predicate that matches a DE-9IM pattern.
 *
 * <h3>FUTURE WORK</h3> Extend the expressiveness of the DE-9IM pattern language to allow: <ul>
 * <li>Combining patterns via disjunction using "|". <li>Limiting patterns via geometry dimension.
 * A dimension limit specifies the allowable dimensions for both or individual geometries as [d] or
 * [ab] or [ab;cd] </ul>
 *
 * @author
 *   Martin Davis
 */
class IMPatternMatcher(private val imPattern: String) extends IMPredicate {

  private val patternMatrix: IntersectionMatrix = new IntersectionMatrix(imPattern)

  override def name: String = "IMPattern"

  // TODO: implement requiresExteriorCheck by inspecting matrix entries for E

  override def init(envA: Envelope, envB: Envelope): Unit = {
    super.init(dimA, dimB)
    // -- if pattern specifies any non-E/non-E interaction, envelopes must not be disjoint
    val requiresInteraction = IMPatternMatcher.requireInteraction(patternMatrix)
    val isDisjoint          = envA.disjoint(envB)
    setValueIf(false, requiresInteraction && isDisjoint)
  }

  override def requireInteraction: Boolean =
    IMPatternMatcher.requireInteraction(patternMatrix)

  override def isDetermined: Boolean = {

    /**
     * Matrix entries only increase in dimension as topology is computed. The predicate can be
     * short-circuited (as false) if any computed entry is greater than the mask value.
     */
    var i = 0
    while (i < 3) {
      var j = 0
      while (j < 3) {
        val patternEntry = patternMatrix.get(i, j)

        if (patternEntry != Dimension.DONTCARE) {
          val matrixVal = getDimension(i, j)

          // -- mask entry TRUE requires a known matrix entry
          if (patternEntry == Dimension.TRUE) {
            if (matrixVal < 0)
              return false
          }
          // -- result is known (false) if matrix entry has exceeded mask
          else if (matrixVal > patternEntry)
            return true
        }
        j += 1
      }
      i += 1
    }
    false
  }

  override def valueIM: Boolean = {
    val v = intMatrix.matches(imPattern)
    v
  }

  override def toString: String =
    name + "(" + imPattern + ")"
}
