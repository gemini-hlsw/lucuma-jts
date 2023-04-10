// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.noding

import java.util

/**
 * A wrapper for {@link Noder}s which validates the output arrangement is correctly noded. An
 * arrangement of line segments is fully noded if there is no line segment which has another segment
 * intersecting its interior. If the noding is not correct, a {@link TopologyException} is thrown
 * with details of the first invalid location found.
 *
 * @author
 *   mdavis
 * @see
 *   FastNodingValidator
 */
class ValidatingNoder[A <: SegmentString](var noder: Noder[A])

/**
 * Creates a noding validator wrapping the given Noder
 *
 * @param noder
 *   the Noder to validate
 */
    extends Noder[A] {
  private var nodedSS: util.Collection[A] = null

  /**
   * Checks whether the output of the wrapped noder is fully noded. Throws an exception if it is
   * not.
   *
   * @throws TopologyException
   */
  override def computeNodes(
    segStrings: util.Collection[A]
  ): Unit = {
    noder.computeNodes(segStrings)
    nodedSS = noder.getNodedSubstrings
    validate()
  }

  private def validate(): Unit = {
    val nv = new FastNodingValidator(nodedSS.asInstanceOf[util.Collection[SegmentString]])
    nv.checkValid()
  }

  override def getNodedSubstrings: util.Collection[A] = nodedSS
}
