// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2019 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */ // Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2019 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.operation.union

import java.util
import org.locationtech.jts.geom.Dimension
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryCollection
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.GeometryFilter
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.Point
import org.locationtech.jts.geom.Polygon
import org.locationtech.jts.util.Assert
import scala.jdk.CollectionConverters._

/**
 * Extracts atomic elements from input geometries or collections, recording the dimension found.
 * Empty geometries are discarded since they do not contribute to the result of {link UnaryUnionOp}.
 *
 * @author
 *   Martin Davis
 */
object InputExtracter {

  /**
   * Extracts elements from a collection of geometries.
   *
   * @param geoms
   *   a collection of geometries return an extracter over the geometries
   */
  def extract(geoms: util.Collection[Geometry]): InputExtracter = {
    val extracter = new InputExtracter
    extracter.add(geoms)
    extracter
  }

  /**
   * Extracts elements from a geometry.
   *
   * @param geoms
   *   a geometry to extract from return an extracter over the geometry
   */
  def extract(geom: Geometry): InputExtracter = {
    val extracter = new InputExtracter
    extracter.add(geom)
    extracter
  }
}

class InputExtracter() extends GeometryFilter {
  private var geomFactory: GeometryFactory = null
  private val polygons                     = new util.ArrayList[Polygon]
  private val lines                        = new util.ArrayList[LineString]
  private val points                       = new util.ArrayList[Point]

  /**
   * The default dimension for an empty GeometryCollection
   */
  private var dimension = Dimension.FALSE

  /**
   * Tests whether there were any non-empty geometries extracted.
   *
   * return true if there is a non-empty geometry present
   */
  def isEmpty: Boolean = polygons.isEmpty && lines.isEmpty && points.isEmpty

  /**
   * Gets the maximum dimension extracted.
   *
   * return the maximum extracted dimension
   */
  def getDimension: Int = dimension

  /**
   * Gets the geometry factory from the extracted geometry, if there is one. If an empty collection
   * was extracted, will return <code>null</code>.
   *
   * return a geometry factory, or null if one could not be determined
   */
  def getFactory: GeometryFactory = geomFactory

  /**
   * Gets the extracted atomic geometries of the given dimension <code>dim</code>.
   *
   * @param dim
   *   the dimension of geometry to return return a list of the extracted geometries of dimension
   *   dim.
   */
  def getExtract(dim: Int): util.List[Geometry] = {
    dim match {
      case 0 =>
        return points.asScala.map(x => x: Geometry).asJava
      case 1 =>
        return lines.asScala.map(x => x: Geometry).asJava
      case 2 =>
        return polygons.asScala.map(x => x: Geometry).asJava
    }
    Assert.shouldNeverReachHere("Invalid dimension: " + dim)
    null
  }

  private def add(geoms: util.Collection[Geometry]): Unit =
    geoms.asScala.foreach(add)

  private def add(geom: Geometry): Unit = {
    if (geomFactory == null) geomFactory = geom.getFactory
    geom.applyF(this)
  }

  override def filter(geom: Geometry): Unit = {
    recordDimension(geom.getDimension)
    if (geom.isInstanceOf[GeometryCollection]) return

    /**
     * Don't keep empty geometries
     */
    if (geom.isEmpty) return
    if (geom.isInstanceOf[Polygon]) {
      polygons.add(geom.asInstanceOf[Polygon])
      return
    } else if (geom.isInstanceOf[LineString]) {
      lines.add(geom.asInstanceOf[LineString])
      return
    } else if (geom.isInstanceOf[Point]) {
      points.add(geom.asInstanceOf[Point])
      return
    }
    Assert.shouldNeverReachHere("Unhandled geometry type: " + geom.getGeometryType)
  }

  private def recordDimension(dim: Int): Unit = if (dim > dimension) dimension = dim
}
