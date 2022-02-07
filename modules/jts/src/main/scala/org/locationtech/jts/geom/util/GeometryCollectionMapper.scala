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
package org.locationtech.jts.geom.util

import java.util
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryCollection
import org.locationtech.jts.geom.GeometryFactory

/**
 * Maps the members of a {link GeometryCollection} into another <tt>GeometryCollection</tt> via a
 * defined mapping function.
 *
 * @author
 *   Martin Davis
 */
object GeometryCollectionMapper {
  def map(gc: GeometryCollection, op: GeometryMapper.MapOp): GeometryCollection = {
    val mapper = new GeometryCollectionMapper(op)
    mapper.map(gc)
  }
}

class GeometryCollectionMapper(val mapOp: GeometryMapper.MapOp) {

  def map(gc: GeometryCollection): GeometryCollection = {
    val mapped = new util.ArrayList[Geometry]
    var i      = 0
    while (i < gc.getNumGeometries) {
      val g = mapOp.map(gc.getGeometryN(i))
      if (!g.isEmpty) mapped.add(g)
      i += 1
    }
    gc.getFactory.createGeometryCollection(GeometryFactory.toGeometryArray(mapped))
  }
}
