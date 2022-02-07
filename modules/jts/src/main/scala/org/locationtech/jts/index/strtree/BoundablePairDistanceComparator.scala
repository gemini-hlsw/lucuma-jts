// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2017 Jia Yu.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.index.strtree

import java.io.Serializable
import java.util.Comparator

/**
 * The Class BoundablePairDistanceComparator. It implements Java comparator and is used as a
 * parameter to sort the BoundablePair list.
 */
class BoundablePairDistanceComparator(
  /** The normal order. */
  var normalOrder: Boolean
) extends Comparator[BoundablePair]
    with Serializable {
  /* (non-Javadoc)
   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
   */
  override def compare(p1: BoundablePair, p2: BoundablePair): Int = {
    val distance1 = p1.getDistance
    val distance2 = p2.getDistance
    if (this.normalOrder) {
      if (distance1 > distance2) return 1
      else if (distance1 == distance2) return 0
      -1
    } else {
      if (distance1 > distance2) return -1
      else if (distance1 == distance2) return 0
      1
    }
  }
}
