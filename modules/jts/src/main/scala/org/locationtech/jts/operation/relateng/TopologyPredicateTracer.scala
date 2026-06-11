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

import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.Location

/**
 * Traces the evaluation of a {link TopologyPredicate}.
 *
 * @author
 *   mdavis
 */
object TopologyPredicateTracer {

  /**
   * Creates a new predicate tracing the evaluation of a given predicate.
   *
   * @param pred
   *   the predicate to trace return the traceable predicate
   */
  def trace(pred: TopologyPredicate): TopologyPredicate =
    new PredicateTracer(pred)

  private class PredicateTracer(private val pred: TopologyPredicate) extends TopologyPredicate {

    override def name: String = pred.name

    override def requireSelfNoding: Boolean =
      pred.requireSelfNoding

    override def requireInteraction: Boolean =
      pred.requireInteraction

    override def requireCovers(isSourceA: Boolean): Boolean =
      pred.requireCovers(isSourceA)

    override def requireExteriorCheck(isSourceA: Boolean): Boolean =
      pred.requireExteriorCheck(isSourceA)

    override def init(dimA: Int, dimB: Int): Unit = {
      pred.init(dimA, dimB)
      checkValue("dimensions")
    }

    override def init(envA: Envelope, envB: Envelope): Unit = {
      pred.init(envA, envB)
      checkValue("envelopes")
    }

    override def updateDimension(locA: Int, locB: Int, dimension: Int): Unit = {
      val desc      = "A:" + Location.toLocationSymbol(locA) +
        "/B:" + Location.toLocationSymbol(locB) +
        " -> " + dimension
      var ind       = ""
      val isChanged = isDimChanged(locA, locB, dimension)
      if (isChanged)
        ind = " <<< "
      System.out.println(desc + ind)
      pred.updateDimension(locA, locB, dimension)
      if (isChanged)
        checkValue("IM entry")
    }

    private def isDimChanged(locA: Int, locB: Int, dimension: Int): Boolean = {
      if (pred.isInstanceOf[IMPredicate])
        return pred.asInstanceOf[IMPredicate].isDimChanged(locA, locB, dimension)
      false
    }

    private def checkValue(source: String): Unit =
      if (pred.isKnown)
        System.out.println(name + " = " + pred.value + " based on " + source)

    override def finish(): Unit =
      pred.finish()

    override def isKnown: Boolean =
      pred.isKnown

    override def value: Boolean =
      pred.value

    override def toString: String =
      pred.toString
  }
}
