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

import org.locationtech.jts.index.chain.MonotoneChain
import org.locationtech.jts.index.chain.MonotoneChainOverlapAction
import org.locationtech.jts.noding.SegmentIntersector
import org.locationtech.jts.noding.SegmentString

class EdgeSegmentOverlapAction(si: SegmentIntersector) extends MonotoneChainOverlapAction {

  override def overlap(mc1: MonotoneChain, start1: Int, mc2: MonotoneChain, start2: Int): Unit = {
    val ss1 = mc1.getContext.asInstanceOf[SegmentString]
    val ss2 = mc2.getContext.asInstanceOf[SegmentString]
    si.processIntersections(ss1, start1, ss2, start2)
  }

}
