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
package org.locationtech.jts.operation.distance

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.index.strtree.ItemBoundable
import org.locationtech.jts.index.strtree.ItemDistance

/**
 * Computes the distance between the facets (segments and vertices) of two {link Geometry}s using a
 * Branch-and-Bound algorithm. The Branch-and-Bound algorithm operates over a traversal of R-trees
 * built on the target and the query geometries. <p> This approach provides the following benefits:
 * <ul> <li>Performance is dramatically improved due to the use of the R-tree index and the pruning
 * due to the Branch-and-Bound approach <li>The spatial index on the target geometry is cached which
 * allow reuse in an repeated query situation. </ul> Using this technique is usually much more
 * performant than using the brute-force {link Geometry#distance(Geometry)} when one or both input
 * geometries are large, or when evaluating many distance computations against a single geometry.
 * <p> This class is thread-safe.
 *
 * @author
 *   Martin Davis
 */
object IndexedFacetDistance {
  private val FACET_SEQ_DIST = new IndexedFacetDistance.FacetSequenceDistance

  /**
   * Computes the distance between facets of two geometries. <p> For geometries with many segments
   * or points, this can be faster than using a simple distance algorithm.
   *
   * @param g1
   *   a geometry
   * @param g2
   *   a geometry return the distance between facets of the geometries
   */
  def distance(g1: Geometry, g2: Geometry): Double = {
    val dist = new IndexedFacetDistance(g1)
    dist.distance(g2)
  }

  /**
   * Tests whether the facets of two geometries lie within a given distance.
   *
   * @param g1
   *   a geometry
   * @param g2
   *   a geometry
   * @param distance
   *   the distance limit return true if two facets lie with the given distance
   */
  def isWithinDistance(g1: Geometry, g2: Geometry, distance: Double): Boolean = {
    val dist = new IndexedFacetDistance(g1)
    dist.isWithinDistance(g2, distance)
  }

  /**
   * Computes the nearest points of the facets of two geometries.
   *
   * @param g1
   *   a geometry
   * @param g2
   *   a geometry return the nearest points on the facets of the geometries
   */
  def nearestPoints(g1: Geometry, g2: Geometry): Array[Coordinate] = {
    val dist = new IndexedFacetDistance(g1)
    dist.nearestPoints(g2)
  }

  private def toPoints(locations: Array[GeometryLocation]): Array[Coordinate] = {
    if (locations == null) return null
    val nearestPts = Array[Coordinate](locations(0).getCoordinate, locations(1).getCoordinate)
    nearestPts
  }

  private class FacetSequenceDistance extends ItemDistance {
    override def distance(item1: ItemBoundable, item2: ItemBoundable): Double = {
      val fs1 = item1.getItem.asInstanceOf[FacetSequence]
      val fs2 = item2.getItem.asInstanceOf[FacetSequence]
      fs1.distance(fs2)
    }
  }

}

class IndexedFacetDistance(var baseGeometry: Geometry) {

  /**
   * Creates a new distance-finding instance for a given target {link Geometry}. <p> Distances will
   * be computed to all facets of the input geometry. The facets of the geometry are the discrete
   * segments and points contained in its components. In the case of {link Lineal} and {link Puntal}
   * inputs, this is equivalent to computing the conventional distance. In the case of {link
   * Polygonal} inputs, this is equivalent to computing the distance to the polygon boundaries.
   *
   * @param geom
   *   a Geometry, which may be of any type.
   */
  private val cachedTree = FacetSequenceTreeBuilder.build(baseGeometry)

  /**
   * Computes the distance from the base geometry to the given geometry.
   *
   * @param g
   *   the geometry to compute the distance to return the computed distance
   */

  def distance(g: Geometry): Double = {
    val tree2 = FacetSequenceTreeBuilder.build(g)
    val obj   = cachedTree.nearestNeighbour(tree2, IndexedFacetDistance.FACET_SEQ_DIST)
    val fs1   = obj(0).asInstanceOf[FacetSequence]
    val fs2   = obj(1).asInstanceOf[FacetSequence]
    fs1.distance(fs2)
  }

  /**
   * Computes the nearest locations on the base geometry and the given geometry.
   *
   * @param g
   *   the geometry to compute the nearest location to return the nearest locations
   */
  def nearestLocations(g: Geometry): Array[GeometryLocation] = {
    val tree2 = FacetSequenceTreeBuilder.build(g)
    val obj   = cachedTree.nearestNeighbour(tree2, IndexedFacetDistance.FACET_SEQ_DIST)
    val fs1   = obj(0).asInstanceOf[FacetSequence]
    val fs2   = obj(1).asInstanceOf[FacetSequence]
    fs1.nearestLocations(fs2)
  }

  /**
   * Compute the nearest locations on the target geometry and the given geometry.
   *
   * @param g
   *   the geometry to compute the nearest point to return the nearest points
   */
  def nearestPoints(g: Geometry): Array[Coordinate] = {
    val minDistanceLocation = nearestLocations(g)
    val nearestPts          = IndexedFacetDistance.toPoints(minDistanceLocation)
    nearestPts
  }

  /**
   * Tests whether the base geometry lies within a specified distance of the given geometry.
   *
   * @param g
   *   the geometry to test
   * @param maxDistance
   *   the maximum distance to test return true if the geometry lies with the specified distance
   */
  def isWithinDistance(g: Geometry, maxDistance: Double): Boolean = { // short-ciruit check
    val envDist = baseGeometry.getEnvelopeInternal.distance(g.getEnvelopeInternal)
    if (envDist > maxDistance) return false
    val tree2   = FacetSequenceTreeBuilder.build(g)
    cachedTree.isWithinDistance(tree2, IndexedFacetDistance.FACET_SEQ_DIST, maxDistance)
  }
}
