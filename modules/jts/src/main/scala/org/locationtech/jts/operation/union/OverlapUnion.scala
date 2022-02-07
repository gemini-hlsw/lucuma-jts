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
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateSequence
import org.locationtech.jts.geom.CoordinateSequenceFilter
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.LineSegment
import org.locationtech.jts.geom.TopologyException
import org.locationtech.jts.geom.util.GeometryCombiner

/**
 * Unions MultiPolygons efficiently by using full topological union only for polygons which may
 * overlap by virtue of intersecting the common area of the inputs. Other polygons are simply
 * combined with the union result, which is much more performant. <p> This situation is likely to
 * occur during cascaded polygon union, since the partitioning of polygons is done heuristically and
 * thus may group disjoint polygons which can lie far apart. It may also occur in real world data
 * which contains many disjoint polygons (e.g. polygons representing parcels on different street
 * blocks). <h2>Algorithm</h2> The overlap region is determined as the common envelope of
 * intersection. The input polygons are partitioned into two sets: <ul> <li>Overlapping: Polygons
 * which intersect the overlap region, and thus potentially overlap each other <li>Disjoint:
 * Polygons which are disjoint from (lie wholly outside) the overlap region </ul> The Overlapping
 * set is fully unioned, and then combined with the Disjoint set. Performing a simple combine works
 * because the disjoint polygons do not interact with each other (since the inputs are valid
 * MultiPolygons). They also do not interact with the Overlapping polygons, since they are outside
 * their envelope.
 *
 * <h2>Verification</h2> In the general case the Overlapping set of polygons will extend beyond the
 * overlap envelope. This means that the union result will extend beyond the overlap region. There
 * is a small chance that the topological union of the overlap region will shift the result linework
 * enough that the result geometry intersects one of the Disjoint geometries. This case is detected
 * and if it occurs is remedied by falling back to performing a full union of the original inputs.
 * Detection is done by a fairly efficient comparison of edge segments which extend beyond the
 * overlap region. If any segments have changed then there is a risk of introduced intersections,
 * and full union is performed. <p> This situation has not been observed in JTS using floating
 * precision, but it could happen due to snapping. It has been observed in other APIs (e.g. GEOS)
 * due to more aggressive snapping. And it will be more likely to happen if a snap-rounding overlay
 * is used.
 *
 * @author
 *   mbdavis
 */
object OverlapUnion {

  /**
   * Union a pair of geometries, using the more performant overlap union algorithm if possible.
   *
   * @param g0
   *   a geometry to union
   * @param g1
   *   a geometry to union return the union of the inputs
   */
  def union(g0: Geometry, g1: Geometry): Geometry = {
    val union = new OverlapUnion(g0, g1)
    union.union
  }

  private def overlapEnvelope(g0: Geometry, g1: Geometry) = {
    val g0Env      = g0.getEnvelopeInternal
    val g1Env      = g1.getEnvelopeInternal
    val overlapEnv = g0Env.intersection(g1Env)
    overlapEnv
  }

  /**
   * Implements union using the buffer-by-zero trick. This seems to be more robust than overlay
   * union, for reasons somewhat unknown.
   *
   * @param g0
   *   a geometry
   * @param g1
   *   a geometry return the union of the geometries
   */
  private def unionBuffer(g0: Geometry, g1: Geometry): Geometry = {
    val factory = g0.getFactory
    val gColl   = factory.createGeometryCollection(Array[Geometry](g0, g1))
    val union   = gColl.buffer(0.0)
    union
  }

  private def intersects(env: Envelope, p0: Coordinate, p1: Coordinate): Boolean =
    env.intersects(p0) || env.intersects(p1)

  private def containsProperly(env: Envelope, p0: Coordinate, p1: Coordinate): Boolean =
    containsProperly(env, p0) && containsProperly(env, p1)

  private def containsProperly(env: Envelope, p: Coordinate): Boolean = {
    if (env.isNull) return false
    p.getX > env.getMinX && p.getX < env.getMaxX && p.getY > env.getMinY && p.getY < env.getMaxY
  }

  private def extractBorderSegments(
    geom: Geometry,
    env:  Envelope,
    segs: util.List[LineSegment]
  ): Unit = geom.applyF(new CoordinateSequenceFilter() {
    override def filter(seq: CoordinateSequence, i: Int): Unit = {
      if (i <= 0) return
      // extract LineSegment
      val p0       = seq.getCoordinate(i - 1)
      val p1       = seq.getCoordinate(i)
      val isBorder = intersects(env, p0, p1) && !containsProperly(env, p0, p1)
      if (isBorder) {
        val seg = new LineSegment(p0, p1)
        segs.add(seg)
      }
      ()
    }

    override def isDone = false

    override def isGeometryChanged = false
  })
}

class OverlapUnion(var g0: Geometry, var g1: Geometry) {

  /**
   * Creates a new instance for unioning the given geometries.
   *
   * @param g0
   *   a geometry to union
   * @param g1
   *   a geometry to union
   */
  private val geomFactory = g0.getFactory
  private var isUnionSafe = false

  /**
   * Unions the input geometries, using the more performant overlap union algorithm if possible.
   *
   * return the union of the inputs
   */
  def union: Geometry = {
    val overlapEnv = OverlapUnion.overlapEnvelope(g0, g1)

    /**
     * If no overlap, can just combine the geometries
     */
    if (overlapEnv.isNull) {
      val g0Copy = g0.copy
      val g1Copy = g1.copy
      return GeometryCombiner.combine(g0Copy, g1Copy)
    }
    val disjointPolys    = new util.ArrayList[Geometry]
    val g0Overlap        = extractByEnvelope(overlapEnv, g0, disjointPolys)
    val g1Overlap        = extractByEnvelope(overlapEnv, g1, disjointPolys)
    //    System.out.println("# geoms in common: " + intersectingPolys.size());
    val unionGeom        = unionFull(g0Overlap, g1Overlap)
    var result: Geometry = null
    isUnionSafe = isBorderSegmentsSame(unionGeom, overlapEnv)
    if (!isUnionSafe) { // overlap union changed border segments... need to do full union
      // System.out.println("OverlapUnion: Falling back to full union");
      result = unionFull(g0, g1)
    } else { // System.out.println("OverlapUnion: fast path");
      result = combine(unionGeom, disjointPolys)
    }
    result
  }

  /**
   * Allows checking whether the optimized or full union was performed. Used for unit testing.
   *
   * return true if the optimized union was performed
   */
  private[union] def isUnionOptimized = isUnionSafe

  private def combine(unionGeom: Geometry, disjointPolys: util.List[Geometry]): Geometry = {
    if (disjointPolys.size <= 0) return unionGeom
    disjointPolys.add(unionGeom)
    val result = GeometryCombiner.combine(disjointPolys)
    result
  }

  private def extractByEnvelope(
    env:           Envelope,
    geom:          Geometry,
    disjointGeoms: util.List[Geometry]
  ): Geometry = {
    val intersectingGeoms = new util.ArrayList[Geometry]
    var i                 = 0
    while (i < geom.getNumGeometries) {
      val elem = geom.getGeometryN(i)
      if (elem.getEnvelopeInternal.intersects(env)) intersectingGeoms.add(elem)
      else {
        val copy = elem.copy
        disjointGeoms.add(copy)
      }
      i += 1
    }
    geomFactory.buildGeometry(intersectingGeoms)
  }

  private def unionFull(geom0: Geometry, geom1: Geometry): Geometry = try geom0.union(geom1)
  catch {
    case _: TopologyException =>
      /**
       * If the overlay union fails, try a buffer union, which often succeeds
       */
      OverlapUnion.unionBuffer(geom0, geom1)
  }

  private def isBorderSegmentsSame(result: Geometry, env: Envelope) = {
    val segsBefore = extractBorderSegments(g0, g1, env)
    val segsAfter  = new util.ArrayList[LineSegment]
    OverlapUnion.extractBorderSegments(result, env, segsAfter)
    // System.out.println("# seg before: " + segsBefore.size() + " - # seg after: " + segsAfter.size());
    isEqual(segsBefore, segsAfter)
  }

  private def isEqual(segs0: util.List[LineSegment], segs1: util.List[LineSegment]): Boolean = {
    if (segs0.size != segs1.size) return false
    val segIndex = new util.HashSet[LineSegment](segs0)
    import scala.jdk.CollectionConverters._
    segs1.asScala.foldLeft(true) { case (v, seg) => if (!segIndex.contains(seg)) false else v }
  }

  private def extractBorderSegments(geom0: Geometry, geom1: Geometry, env: Envelope) = {
    val segs = new util.ArrayList[LineSegment]
    OverlapUnion.extractBorderSegments(geom0, env, segs)
    if (geom1 != null) OverlapUnion.extractBorderSegments(geom1, env, segs)
    segs
  }
}
