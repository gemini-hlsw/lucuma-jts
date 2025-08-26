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
package org.locationtech.jts.index

import java.util

/**
 * Builds an array of all visited items.
 *
 * @version 1.7
 */
class ArrayListVisitor()

/**
 * Creates a new instance.
 */
    extends ItemVisitor {
  private val items = new util.ArrayList[Any]

  /**
   * Visits an item.
   *
   * @param item
   *   the item to visit
   */
  override def visitItem(item: Any): Unit = {
    items.add(item)
    ()
  }

  /**
   * Gets the array of visited items.
   *
   * return the array of items
   */
  def getItems: util.ArrayList[Any] = items
}
