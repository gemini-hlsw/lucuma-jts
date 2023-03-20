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
class ValidatingNoder(var noder: Noder[SegmentString])

/**
 * Creates a noding validator wrapping the given Noder
 *
 * @param noder
 *   the Noder to validate
 */
    extends Noder[SegmentString] {
  private var nodedSS: util.Collection[SegmentString] = null

  /**
   * Checks whether the output of the wrapped noder is fully noded. Throws an exception if it is
   * not.
   *
   * @throws TopologyException
   */
  @SuppressWarnings(Array("unchecked"))
  override def computeNodes(
    @SuppressWarnings(Array("rawtypes")) segStrings: util.Collection[SegmentString]
  ): Unit = {
    noder.computeNodes(segStrings)
    nodedSS = noder.getNodedSubstrings
    validate()
  }

  private def validate(): Unit = {
    val nv = new FastNodingValidator(nodedSS)
    nv.checkValid()
  }

  @SuppressWarnings(Array("rawtypes"))
  override def getNodedSubstrings: util.Collection[SegmentString] = nodedSS
}
