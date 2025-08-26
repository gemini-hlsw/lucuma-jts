// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
package org.locationtech.jts.geomgraph

import org.locationtech.jts.noding.BasicSegmentString
import org.locationtech.jts.noding.FastNodingValidator
import org.locationtech.jts.noding.SegmentString

import java.util

/**
 * Validates that a collection of {link Edge}s is correctly noded. Throws an appropriate exception
 * if an noding error is found. Uses {link FastNodingValidator} to perform the validation.
 *
 * @version 1.7
 * @see
 *   FastNodingValidator
 */
object EdgeNodingValidator {

  /**
   * Checks whether the supplied {link Edge}s are correctly noded. Throws a {link TopologyException}
   * if they are not.
   *
   * @param edges
   *   a collection of Edges. throws TopologyException if the SegmentStrings are not correctly noded
   */
  def checkValid(edges: util.Collection[Edge]): Unit = {
    val validator = new EdgeNodingValidator(edges)
    validator.checkValid()
  }

  def toSegmentStrings(edges: util.Collection[Edge]): util.ArrayList[SegmentString] = { // convert Edges to SegmentStrings
    val segStrings = new util.ArrayList[SegmentString]
    val i          = edges.iterator
    while (i.hasNext) {
      val e = i.next
      segStrings.add(new BasicSegmentString(e.getCoordinates, e))
    }
    segStrings
  }
}

class EdgeNodingValidator(val edges: util.Collection[Edge]) {

  /**
   * Creates a new validator for the given collection of {link Edge}s.
   *
   * @param edges
   *   a collection of Edges.
   */
  private val nv = new FastNodingValidator(EdgeNodingValidator.toSegmentStrings(edges))

  /**
   * Checks whether the supplied edges are correctly noded. Throws an exception if they are not.
   *
   * throws TopologyException if the SegmentStrings are not correctly noded
   */
  def checkValid(): Unit = nv.checkValid()
}
