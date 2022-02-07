// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
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
package org.locationtech.jts.index.intervaltree

import org.locationtech.jts.index.ItemVisitor

class IntervalRTreeLeafNode(val minArg: Double, val maxArg: Double, var item: Any)
    extends IntervalRTreeNode(minArg, maxArg) {
//  this.min = min
//  this.max = max

  override def query(queryMin: Double, queryMax: Double, visitor: ItemVisitor): Unit = {
    if (!intersects(queryMin, queryMax)) return
    visitor.visitItem(item)
  }
}
