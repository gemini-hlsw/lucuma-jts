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
package org.locationtech.jts.index.strtree

import java.io.Serializable

/**
 * Boundable wrapper for a non-Boundable spatial object. Used internally by AbstractSTRtree.
 *
 * @version 1.7
 */
class ItemBoundable(var bounds: Any, var item: Any) extends Boundable with Serializable {
  override def getBounds: Any = bounds

  def getItem: Any = item
}
