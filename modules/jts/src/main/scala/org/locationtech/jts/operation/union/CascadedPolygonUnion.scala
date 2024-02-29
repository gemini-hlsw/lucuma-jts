// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.operation.union

import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.Polygon
import org.locationtech.jts.geom.Polygonal
import org.locationtech.jts.geom.TopologyException
import org.locationtech.jts.geom.util.PolygonExtracter
import org.locationtech.jts.index.strtree.STRtree
import org.locationtech.jts.operation.overlay.snap.SnapIfNeededOverlayOp

import java.util
import scala.annotation.nowarn

/*
 * Copyright (c) 2016 Vivid Solutions.
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
 * Provides an efficient method of unioning a collection of {@link Polygonal} geometries. The
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

  /**
   * A union strategy that uses the classic JTS {@link SnapIfNeededOverlayOp} , and for polygonal
   * geometries a robustness fallback using <cod>buffer(0)</code>.
   */
  private[union] val CLASSIC_UNION = new UnionStrategy() {
    override def union(g0: Geometry, g1: Geometry): Geometry = try
      SnapIfNeededOverlayOp.union(g0, g1)
    catch {
      case ex: TopologyException =>
        // union-by-buffer only works for polygons
        if (g0.getDimension != 2 || g1.getDimension != 2) throw ex
        unionPolygonsByBuffer(g0, g1)
    }

    override def isFloatingPrecision() = true

    /**
     * An alternative way of unioning polygonal geometries by using <code>bufer(0)</code>. Only
     * worth using if regular overlay union fails.
     *
     * @param g0
     *   a polygonal geometry
     * @param g1
     *   a polygonal geometry
     * @return
     *   the union of the geometries
     */
    private def unionPolygonsByBuffer(g0: Geometry, g1: Geometry) = { // System.out.println("Unioning by buffer");
      val coll = g0.getFactory.createGeometryCollection(Array[Geometry](g0, g1))
      coll.buffer(0)
    }
  }

  /**
   * Computes the union of a collection of {@link Polygonal} {@link Geometry} s.
   *
   * @param polys
   *   a collection of {@link Polygonal} {@link Geometry} s
   */
  def union(polys: util.Collection[Geometry]): Geometry = {
    val op = new CascadedPolygonUnion(polys)
    op.union
  }

  def union(polys: util.Collection[Geometry], unionFun: UnionStrategy): Geometry = {
    val op = new CascadedPolygonUnion(polys, unionFun)
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
   * @return
   *   the geometry at the given index or null if the index is out of range
   */
  private def getGeometry(list: util.List[Geometry], index: Int): Geometry = {
    if (index >= list.size) return null
    list.get(index).asInstanceOf[Geometry]
  }

  /**
   * Computes a {@link Geometry} containing only {@link Polygonal} components. Extracts the {@link
   * Polygon}s from the input and returns them as an appropriate {@link Polygonal} geometry. <p> If
   * the input is already <tt>Polygonal</tt>, it is returned unchanged. <p> A particular use case is
   * to filter out non-polygonal components returned from an overlay operation.
   *
   * @param g
   *   the geometry to filter
   * @return
   *   a Polygonal geometry
   */
  private def restrictToPolygons(g: Geometry): Geometry = {
    if (g.isInstanceOf[Polygonal]) return g
    val polygons = PolygonExtracter.getPolygons(g)
    if (polygons.size == 1) return polygons.get(0).asInstanceOf[Polygon]
    g.getFactory.createMultiPolygon(GeometryFactory.toPolygonArray(polygons))
  }
}

/**
 * Creates a new instance to union the given collection of {@link Geometry} s.
 *
 * @param polys
 *   a collection of {@link Polygonal} {@link Geometry} s
 */
class CascadedPolygonUnion(var inputPolys: util.Collection[Geometry], var unionFun: UnionStrategy) {

  // guard against null input
  if (inputPolys == null) {
    inputPolys = new util.ArrayList[Geometry]
  }
  private val countInput: Int              = inputPolys.size
  private var countRemainder               = countInput
  @nowarn
  private var geomFactory: GeometryFactory = null

  def this(polys: util.Collection[Geometry]) =
    this(polys, CascadedPolygonUnion.CLASSIC_UNION)

  /**
   * Computes the union of the input geometries. <p> This method discards the input geometries as
   * they are processed. In many input cases this reduces the memory retained as the operation
   * proceeds. Optimal memory usage is achieved by disposing of the original input collection before
   * calling this method.
   *
   * @return
   *   the union of the input geometries or null if no input geometries were provided
   * @throws IllegalStateException
   *   if this method is called more than once
   */
  def union: Geometry = {
    if (inputPolys == null) {
      throw new IllegalStateException("union() method cannot be called twice")
    }
    if (inputPolys.isEmpty) {
      return null
    }
    geomFactory = inputPolys.iterator.next.asInstanceOf[Geometry].getFactory

    /**
     * A spatial index to organize the collection into groups of close geometries. This makes
     * unioning more efficient, since vertices are more likely to be eliminated on each round.
     */
    //    STRtree index = new STRtree();
    val index: STRtree                = new STRtree(CascadedPolygonUnion.STRTREE_NODE_CAPACITY)
    val i: util.Iterator[Geometry]    = inputPolys.iterator
    while (i.hasNext) {
      val item: Geometry = i.next.asInstanceOf[Geometry]
      index.insert(item.getEnvelopeInternal, item)
    }
    // To avoiding holding memory remove references to the input geometries,
    inputPolys = null
    val itemTree: util.List[Geometry] = index.itemsTree.asInstanceOf[util.List[Geometry]]
    //    printItemEnvelopes(itemTree);
    val unionAll: Geometry            = unionTree(itemTree)
    return unionAll
  }

  private def unionTree(geomTree: util.List[Geometry]): Geometry = {

    /**
     * Recursively unions all subtrees in the list into single geometries. The result is a list of
     * Geometrys only
     */
    val geoms: util.List[Geometry] = reduceToGeometries(geomTree)
    //    Geometry union = bufferUnion(geoms);
    val union: Geometry            = binaryUnion(geoms)
    // print out union (allows visualizing hierarchy)
    //    System.out.println(union);
    return union
  }

  /**
   * Unions a list of geometries by treating the list as a flattened binary tree, and performing a
   * cascaded union on the tree.
   */
  private def binaryUnion(geoms: util.List[Geometry]): Geometry =
    return binaryUnion(geoms, 0, geoms.size)

  /**
   * Unions a section of a list using a recursive binary union on each half of the section.
   *
   * @param geoms
   *   the list of geometries containing the section to union
   * @param start
   *   the start index of the section
   * @param end
   *   the index after the end of the section
   * @return
   *   the union of the list section
   */
  private def binaryUnion(geoms: util.List[Geometry], start: Int, end: Int): Geometry =
    if (end - start <= 1) {
      val g0: Geometry = CascadedPolygonUnion.getGeometry(geoms, start)
      return unionSafe(g0, null)
    } else {
      if (end - start == 2) {
        return unionSafe(CascadedPolygonUnion.getGeometry(geoms, start),
                         CascadedPolygonUnion.getGeometry(geoms, start + 1)
        )
      } else { // recurse on both halves of the list
        val mid: Int     = (end + start) / 2
        val g0: Geometry = binaryUnion(geoms, start, mid)
        val g1: Geometry = binaryUnion(geoms, mid, end)
        return unionSafe(g0, g1)
      }
    }

  /**
   * Reduces a tree of geometries to a list of geometries by recursively unioning the subtrees in
   * the list.
   *
   * @param geomTree
   *   a tree-structured list of geometries
   * @return
   *   a list of Geometrys
   */
  private def reduceToGeometries(geomTree: util.List[Geometry]): util.List[Geometry] = {
    val geoms: util.List[Geometry] = new util.ArrayList[Geometry]
    val i: util.Iterator[Geometry] = geomTree.iterator
    while (i.hasNext) {
      val o: Any         = i.next
      var geom: Geometry = null
      if (o.isInstanceOf[util.List[_]]) {
        geom = unionTree(o.asInstanceOf[util.List[Geometry]])
      } else {
        if (o.isInstanceOf[Geometry]) {
          geom = o.asInstanceOf[Geometry]
        }
      }
      geoms.add(geom)
    }
    return geoms
  }

  /**
   * Computes the union of two geometries, either or both of which may be null.
   *
   * @param g0
   *   a Geometry
   * @param g1
   *   a Geometry
   * @return
   *   the union of the input(s) or null if both inputs are null
   */
  private def unionSafe(g0: Geometry, g1: Geometry): Geometry = {
    if (g0 == null && g1 == null) {
      return null
    }
    if (g0 == null) {
      return g1.copy
    }
    if (g1 == null) {
      return g0.copy
    }
    countRemainder -= 1
    // if (Debug.isDebugging) {
    // Debug.println("Remainder: " + countRemainder + " out of " + countInput)
    // Debug.print("Union: A: " + g0.getNumPoints + " / B: " + g1.getNumPoints + "  ---  ")
// }
    val union: Geometry = unionActual(g0, g1)
    // if (Debug.isDebugging) {
    // Debug.println(" Result: " + union.getNumPoints)
// }
    // if (TestBuilderProxy.isActive()) TestBuilderProxy.showIndicator(union);
    return union
  }

  /**
   * Encapsulates the actual unioning of two polygonal geometries.
   *
   * @param g0
   * @param g1
   * @return
   */
  private def unionActual(g0: Geometry, g1: Geometry): Geometry = {
    var union: Geometry     = null
    if (unionFun.isFloatingPrecision()) {
      union = OverlapUnion.union(g0, g1, unionFun)
    } else {
      union = unionFun.union(g0, g1)
    }
    val unionPoly: Geometry = CascadedPolygonUnion.restrictToPolygons(union)
    return unionPoly
  }
}
