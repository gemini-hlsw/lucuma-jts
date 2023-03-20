// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.noding.snap

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.index.kdtree.KdNode
import org.locationtech.jts.index.kdtree.KdTree

/*
 * Copyright (c) 2020 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v20.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */

/**
 * An index providing fast creating and lookup of snap points.
 *
 * @author
 *   mdavis
 */
class SnappingPointIndex private[snap] (var snapTolerance: Double) {

  /**
   * Since points are added incrementally, this index needs to be dynamic. This class also makes use
   * of the KdTree support for a tolerance distance for point equality.
   */
  private var snapPointIndex = new KdTree(snapTolerance)

  def snap(p: Coordinate): Coordinate = {

    /**
     * Inserting the coordinate snaps it to any existing one within tolerance, or adds it if not.
     */
    val node = snapPointIndex.insert(p)
    node.getCoordinate
  }

  def getTolerance: Double = snapTolerance
}
