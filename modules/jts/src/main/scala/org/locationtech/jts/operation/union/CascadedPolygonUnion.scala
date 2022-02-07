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
package org.locationtech.jts.operation.union

import java.util
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.Polygon
import org.locationtech.jts.geom.Polygonal
import org.locationtech.jts.geom.util.PolygonExtracter
import org.locationtech.jts.index.strtree.STRtree
import scala.jdk.CollectionConverters._

/**
 * Provides an efficient method of unioning a collection of {link Polygonal} geometries. The
 * geometries are indexed using a spatial index, and unioned recursively in index order. For
 * geometries with a high degree of overlap, this has the effect of reducing the number of vertices
 * early in the process, which increases speed and robustness. <p> This algorithm is faster and more
 * robust than the simple iterated approach of repeatedly unioning each polygon to a result
 * geometry. <p> The <tt>buffer(0)</tt> trick is sometimes faster, but can be less robust and can
 * sometimes take a long time to complete. This is particularly the case where there is a high
 * degree of overlap between the polygons. In this case, <tt>buffer(0)</tt> is forced to compute
 * with <i>all</i> line segments from the outset, whereas cascading can eliminate many segments at
 * each stage of processing. The best situation for using <tt>buffer(0)</tt> is the trivial case
 * where there is <i>no</i> overlap between the input geometries. However, this case is likely rare
 * in practice.
 *
 * @author
 *   Martin Davis
 */
object CascadedPolygonUnion {
  // /**
  //  * Computes the union of
  //  * a collection of {link Polygonal} {link Geometry}s.
  //  *
  //  * @param polys a collection of { @link Polygonal} { @link Geometry}s
  //  */
  def union(polys: util.Collection[Geometry]): Geometry = {
    val op = new CascadedPolygonUnion(polys)
    op.union
  }

  /**
   * The effectiveness of the index is somewhat sensitive to the node capacity. Testing indicates
   * that a smaller capacity is better. For an STRtree, 4 is probably a good number (since this
   * produces 2x2 "squares").
   */
  private val STRTREE_NODE_CAPACITY = 4

  /**
   * Gets the element at a given list index, or null if the index is out of range.
   *
   * @param list
   * @param index
   *   return the geometry at the given index or null if the index is out of range
   */
  private def getGeometry(list: util.List[_], index: Int): Geometry = {
    if (index >= list.size) return null
    list.get(index).asInstanceOf[Geometry]
  }

  /**
   * Computes a {link Geometry} containing only {link Polygonal} components. Extracts the {link
   * Polygon}s from the input and returns them as an appropriate {link Polygonal} geometry. <p> If
   * the input is already <tt>Polygonal</tt>, it is returned unchanged. <p> A particular use case is
   * to filter out non-polygonal components returned from an overlay operation.
   *
   * @param g
   *   the geometry to filter return a Polygonal geometry
   */
  private def restrictToPolygons(g: Geometry): Geometry = {
    if (g.isInstanceOf[Polygonal]) return g
    val polygons = PolygonExtracter.getPolygons(g)
    if (polygons.size == 1) return polygons.get(0).asInstanceOf[Polygon]
    g.getFactory.createMultiPolygon(GeometryFactory.toPolygonArray(polygons))
  }
}

class CascadedPolygonUnion(var inputPolysArg: util.Collection[Geometry]) {

// /**
//  * Creates a new instance to union
//  * the given collection of {link Geometry}s.
//  *
//  * @param polys a collection of { @link Polygonal} { @link Geometry}s
//  */  // guard against null input
  var inputPolys: util.Collection[Geometry] =
    if (inputPolysArg == null) new util.ArrayList[Geometry] else inputPolysArg
//  private var geomFactory: GeometryFactory = null

  // /**
  //  * Computes the union of the input geometries.
  //  * <p>
  //  * This method discards the input geometries as they are processed.
  //  * In many input cases this reduces the memory retained
  //  * as the operation proceeds.
  //  * Optimal memory usage is achieved
  //  * by disposing of the original input collection
  //  * before calling this method.
  //  *
  //  * return the union of the input geometries
  //  *         or null if no input geometries were provided
  //  * throws IllegalStateException if this method is called more than once
  //  */
  def union: Geometry = {
    if (inputPolys == null) throw new IllegalStateException("union() method cannot be called twice")
    if (inputPolys.isEmpty) return null
//    geomFactory = inputPolys.iterator.next.getFactory
    /**
     * A spatial index to organize the collection into groups of close geometries. This makes
     * unioning more efficient, since vertices are more likely to be eliminated on each round.
     */
    //    STRtree index = new STRtree();
    val index    = new STRtree(CascadedPolygonUnion.STRTREE_NODE_CAPACITY)
    val i        = inputPolys.iterator
    while (i.hasNext) {
      val item = i.next
      index.insert(item.getEnvelopeInternal, item)
    }
    // To avoiding holding memory remove references to the input geometries,
    inputPolys = null
    val itemTree = index.itemsTree
    //    printItemEnvelopes(itemTree);
    val unionAll = unionTree(itemTree.asScala.map(x => x.asInstanceOf[Geometry]).asJava)
    unionAll
  }

  private def unionTree(geomTree: util.List[Geometry]): Geometry = {

    /**
     * Recursively unions all subtrees in the list into single geometries. The result is a list of
     * Geometrys only
     */
    val geoms = reduceToGeometries(geomTree)
    //    Geometry union = bufferUnion(geoms);
    val union = binaryUnion(geoms)
    // print out union (allows visualizing hierarchy)
    //    System.out.println(union);
    union
  }

  def repeatedUnion(geoms: util.List[Geometry]): Geometry = {
    var union: Geometry = null
    val i               = geoms.iterator
    while (i.hasNext) {
      val g = i.next
      if (union == null) union = g.copy
      else union = union.union(g)
    }
    union
  }

  def bufferUnion(geoms: util.List[Geometry]): Geometry = {
    val factory  = geoms.get(0).getFactory
    val gColl    = factory.buildGeometry(geoms)
    val unionAll = gColl.buffer(0.0)
    unionAll
  }

  def bufferUnion(g0: Geometry, g1: Geometry): Geometry = {
    val factory  = g0.getFactory
    val gColl    = factory.createGeometryCollection(Array[Geometry](g0, g1))
    val unionAll = gColl.buffer(0.0)
    unionAll
  }

  /**
   * Unions a list of geometries by treating the list as a flattened binary tree, and performing a
   * cascaded union on the tree.
   */
  private def binaryUnion(geoms: util.List[Geometry]): Geometry = binaryUnion(geoms, 0, geoms.size)

  /**
   * Unions a section of a list using a recursive binary union on each half of the section.
   *
   * @param geoms
   *   the list of geometries containing the section to union
   * @param start
   *   the start index of the section
   * @param end
   *   the index after the end of the section return the union of the list section
   */
  private def binaryUnion(geoms: util.List[Geometry], start: Int, end: Int): Geometry = if (
    end - start <= 1
  ) {
    val g0 = CascadedPolygonUnion.getGeometry(geoms, start)
    unionSafe(g0, null)
  } else if (end - start == 2)
    unionSafe(CascadedPolygonUnion.getGeometry(geoms, start),
              CascadedPolygonUnion.getGeometry(geoms, start + 1)
    )
  else { // recurse on both halves of the list
    val mid = (end + start) / 2
    val g0  = binaryUnion(geoms, start, mid)
    val g1  = binaryUnion(geoms, mid, end)
    unionSafe(g0, g1)
  }

  /**
   * Reduces a tree of geometries to a list of geometries by recursively unioning the subtrees in
   * the list.
   *
   * @param geomTree
   *   a tree-structured list of geometries return a list of Geometrys
   */
  private def reduceToGeometries(geomTree: util.List[Geometry]) = {
    val geoms = new util.ArrayList[Geometry]
    val i     = geomTree.iterator
    while (i.hasNext) {
      val o              = i.next
      var geom: Geometry = null
      if (o.isInstanceOf[util.List[_]]) geom = unionTree(o.asInstanceOf[util.List[Geometry]])
      else if (o.isInstanceOf[Geometry]) geom = o.asInstanceOf[Geometry]
      geoms.add(geom)
    }
    geoms
  }

  /**
   * Computes the union of two geometries, either or both of which may be null.
   *
   * @param g0
   *   a Geometry
   * @param g1
   *   a Geometry return the union of the input(s) or null if both inputs are null
   */
  private def unionSafe(g0: Geometry, g1: Geometry): Geometry = {
    if (g0 == null && g1 == null) return null
    if (g0 == null) return g1.copy
    if (g1 == null) return g0.copy
    unionActual(g0, g1)
  }

  /**
   * Encapsulates the actual unioning of two polygonal geometries.
   *
   * @param g0
   * @param g1
   *   return
   */
  private def unionActual(g0: Geometry, g1: Geometry): Geometry = {
    val union = OverlapUnion.union(g0, g1)
    CascadedPolygonUnion.restrictToPolygons(union)
  }
}
