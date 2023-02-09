// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2016 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */ /*
 * Copyright (c) 2016 Martin Davis.
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

import org.locationtech.jts.geom.Geometry

import java.util

/**
 * Methods to map various collections of {link Geometry}s via defined mapping functions.
 *
 * @author
 *   Martin Davis
 */
object GeometryMapper {

  /**
   * Maps the members of a {link Geometry} (which may be atomic or composite) into another
   * <tt>Geometry</tt> of most specific type. <tt>null</tt> results are skipped. In the case of
   * hierarchical {link GeometryCollection}s, only the first level of members are mapped.
   *
   * @param geom
   *   the input atomic or composite geometry
   * @param op
   *   the mapping operation return a result collection or geometry of most specific type
   */
  def map(geom: Geometry, op: GeometryMapper.MapOp): Geometry = {
    val mapped = new util.ArrayList[Geometry]
    var i      = 0
    while (i < geom.getNumGeometries) {
      val g = op.map(geom.getGeometryN(i))
      if (g != null) mapped.add(g)
      i += 1
    }
    geom.getFactory.buildGeometry(mapped)
  }

  def map(geoms: util.Collection[Geometry], op: GeometryMapper.MapOp): util.ArrayList[Geometry] = {
    val mapped = new util.ArrayList[Geometry]
    val i      = geoms.iterator
    while (i.hasNext) {
      val g  = i.next
      val gr = op.map(g)
      if (gr != null) mapped.add(gr)
    }
    mapped
  }

  /**
   * An interface for geometry functions used for mapping.
   *
   * @author
   *   Martin Davis
   */
  trait MapOp {

    /**
     * Computes a new geometry value.
     *
     * @param g
     *   the input geometry return a result geometry
     */
    def map(g: Geometry): Geometry
  }

}
