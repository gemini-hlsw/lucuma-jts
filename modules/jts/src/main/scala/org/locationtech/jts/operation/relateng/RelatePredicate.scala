// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2023 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v20.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.operation.relateng

import org.locationtech.jts.geom.Dimension
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.Location

/**
 * Creates predicate instances for evaluating OGC-standard named topological relationships.
 * Predicates can be evaluated for geometries using {link RelateNG}.
 *
 * @author
 *   Martin Davis
 */
object RelatePredicate {

  /**
   * Creates a predicate to determine whether two geometries intersect. <p> The
   * <code>intersects</code> predicate has the following equivalent definitions: <ul> <li>The two
   * geometries have at least one point in common <li>The DE-9IM Intersection Matrix for the two
   * geometries matches at least one of the patterns <ul> <li><code>[T********]</code>
   * <li><code>[*T*******]</code> <li><code>[***T*****]</code> <li><code>[****T****]</code> </ul>
   * <li><code>disjoint() = false</code> <br>(<code>intersects</code> is the inverse of
   * <code>disjoint</code>) </ul>
   *
   * return the predicate instance
   *
   * @see
   *   #disjoint()
   */
  def intersects(): TopologyPredicate = new BasicPredicate {

    override def name: String = "intersects"

    override def requireSelfNoding: Boolean =
      // -- self-noding is not required to check for a simple interaction
      false

    override def requireExteriorCheck(isSourceA: Boolean): Boolean =
      // -- intersects only requires testing interaction
      false

    override def init(envA: Envelope, envB: Envelope): Unit =
      require(envA.intersects(envB))

    override def updateDimension(locA: Int, locB: Int, dimension: Int): Unit =
      setValueIf(true, BasicPredicate.isIntersection(locA, locB))

    override def finish(): Unit =
      // -- if no intersecting locations were found
      setValue(false)

  }

  /**
   * Creates a predicate to determine whether two geometries are disjoint. <p> The
   * <code>disjoint</code> predicate has the following equivalent definitions: <ul> <li>The two
   * geometries have no point in common <li>The DE-9IM Intersection Matrix for the two geometries
   * matches <code>[FF*FF****]</code> <li><code>intersects() = false</code>
   * <br>(<code>disjoint</code> is the inverse of <code>intersects</code>) </ul>
   *
   * return the predicate instance
   *
   * @see
   *   #intersects()
   */
  def disjoint(): TopologyPredicate = new BasicPredicate {

    override def name: String = "disjoint"

    override def requireSelfNoding: Boolean =
      // -- self-noding is not required to check for a simple interaction
      false

    override def requireInteraction: Boolean =
      // -- ensure entire matrix is computed
      false

    override def requireExteriorCheck(isSourceA: Boolean): Boolean =
      // -- disjoint only requires testing interaction
      false

    override def init(envA: Envelope, envB: Envelope): Unit =
      setValueIf(true, envA.disjoint(envB))

    override def updateDimension(locA: Int, locB: Int, dimension: Int): Unit =
      setValueIf(false, BasicPredicate.isIntersection(locA, locB))

    override def finish(): Unit =
      // -- if no intersecting locations were found
      setValue(true)

  }

  /**
   * Creates a predicate to determine whether a geometry contains another geometry. <p> The
   * <code>contains</code> predicate has the following equivalent definitions: <ul> <li>Every point
   * of the other geometry is a point of this geometry, and the interiors of the two geometries have
   * at least one point in common. <li>The DE-9IM Intersection Matrix for the two geometries matches
   * the pattern <code>[T*****FF*]</code> <li><code>within(B, A) = true</code>
   * <br>(<code>contains</code> is the converse of {link #within} ) </ul> An implication of the
   * definition is that "Geometries do not contain their boundary". In other words, if a geometry A
   * is a subset of the points in the boundary of a geometry B, <code>B.contains(A) = false</code>.
   * (As a concrete example, take A to be a LineString which lies in the boundary of a Polygon B.)
   * For a predicate with similar behavior but avoiding this subtle limitation, see {link #covers}.
   *
   * return the predicate instance
   *
   * @see
   *   #within()
   */
  def contains(): TopologyPredicate = new IMPredicate {

    override def name: String = "contains"

    override def requireCovers(isSourceA: Boolean): Boolean =
      isSourceA == RelateGeometry.GEOM_A

    override def requireExteriorCheck(isSourceA: Boolean): Boolean =
      // -- only need to check B against Exterior of A
      isSourceA == RelateGeometry.GEOM_B

    override def init(dimA: Int, dimB: Int): Unit = {
      super.init(dimA, dimB)
      require(IMPredicate.isDimsCompatibleWithCovers(dimA, dimB))
    }

    override def init(envA: Envelope, envB: Envelope): Unit =
      requireCovers(envA, envB)

    override def isDetermined: Boolean =
      intersectsExteriorOf(RelateGeometry.GEOM_A)

    override def valueIM: Boolean =
      intMatrix.isContains
  }

  /**
   * Creates a predicate to determine whether a geometry is within another geometry. <p> The
   * <code>within</code> predicate has the following equivalent definitions: <ul> <li>Every point of
   * this geometry is a point of the other geometry, and the interiors of the two geometries have at
   * least one point in common. <li>The DE-9IM Intersection Matrix for the two geometries matches
   * <code>[T*F**F***]</code> <li><code>contains(B, A) = true</code> <br>(<code>within</code> is the
   * converse of {link #contains}) </ul> An implication of the definition is that "The boundary of a
   * Geometry is not within the Geometry". In other words, if a geometry A is a subset of the points
   * in the boundary of a geometry B, <code>within(B, A) = false</code> (As a concrete example, take
   * A to be a LineString which lies in the boundary of a Polygon B.) For a predicate with similar
   * behavior but avoiding this subtle limitation, see {link #coveredBy}.
   *
   * return the predicate instance
   *
   * @see
   *   #contains()
   */
  def within(): TopologyPredicate = new IMPredicate {

    override def name: String = "within"

    override def requireCovers(isSourceA: Boolean): Boolean =
      isSourceA == RelateGeometry.GEOM_B

    override def requireExteriorCheck(isSourceA: Boolean): Boolean =
      // -- only need to check A against Exterior of B
      isSourceA == RelateGeometry.GEOM_A

    override def init(dimA: Int, dimB: Int): Unit = {
      super.init(dimA, dimB)
      require(IMPredicate.isDimsCompatibleWithCovers(dimB, dimA))
    }

    override def init(envA: Envelope, envB: Envelope): Unit =
      requireCovers(envB, envA)

    override def isDetermined: Boolean =
      intersectsExteriorOf(RelateGeometry.GEOM_B)

    override def valueIM: Boolean =
      intMatrix.isWithin
  }

  /**
   * Creates a predicate to determine whether a geometry covers another geometry. <p> The
   * <code>covers</code> predicate has the following equivalent definitions: <ul> <li>Every point of
   * the other geometry is a point of this geometry. <li>The DE-9IM Intersection Matrix for the two
   * geometries matches at least one of the following patterns: <ul> <li><code>[T*****FF*]</code>
   * <li><code>[*T****FF*]</code> <li><code>[***T**FF*]</code> <li><code>[****T*FF*]</code> </ul>
   * <li><code>coveredBy(b, a) = true</code> <br>(<code>covers</code> is the converse of {link
   * #coveredBy}) </ul> If either geometry is empty, the value of this predicate is
   * <code>false</code>. <p> This predicate is similar to {link #contains()}, but is more inclusive
   * (i.e. returns <code>true</code> for more cases). In particular, unlike <code>contains</code> it
   * does not distinguish between points in the boundary and in the interior of geometries. For most
   * cases, <code>covers</code> should be used in preference to <code>contains</code>. As an added
   * benefit, <code>covers</code> is more amenable to optimization, and hence should be more
   * performant.
   *
   * return the predicate instance
   *
   * @see
   *   #coveredBy()
   */
  def covers(): TopologyPredicate = new IMPredicate {

    override def name: String = "covers"

    override def requireCovers(isSourceA: Boolean): Boolean =
      isSourceA == RelateGeometry.GEOM_A

    override def requireExteriorCheck(isSourceA: Boolean): Boolean =
      // -- only need to check B against Exterior of A
      isSourceA == RelateGeometry.GEOM_B

    override def init(dimA: Int, dimB: Int): Unit = {
      super.init(dimA, dimB)
      require(IMPredicate.isDimsCompatibleWithCovers(dimA, dimB))
    }

    override def init(envA: Envelope, envB: Envelope): Unit =
      requireCovers(envA, envB)

    override def isDetermined: Boolean =
      intersectsExteriorOf(RelateGeometry.GEOM_A)

    override def valueIM: Boolean =
      intMatrix.isCovers
  }

  /**
   * Creates a predicate to determine whether a geometry is covered by another geometry. <p> The
   * <code>coveredBy</code> predicate has the following equivalent definitions: <ul> <li>Every point
   * of this geometry is a point of the other geometry. <li>The DE-9IM Intersection Matrix for the
   * two geometries matches at least one of the following patterns: <ul>
   * <li><code>[T*F**F***]</code> <li><code>[*TF**F***]</code> <li><code>[**FT*F***]</code>
   * <li><code>[**F*TF***]</code> </ul> <li><code>covers(B, A) = true</code>
   * <br>(<code>coveredBy</code> is the converse of {link #covers}) </ul> If either geometry is
   * empty, the value of this predicate is <code>false</code>. <p> This predicate is similar to
   * {link #within}, but is more inclusive (i.e. returns <code>true</code> for more cases).
   *
   * return the predicate instance
   *
   * @see
   *   #covers()
   */
  def coveredBy(): TopologyPredicate = new IMPredicate {

    override def name: String = "coveredBy"

    override def requireCovers(isSourceA: Boolean): Boolean =
      isSourceA == RelateGeometry.GEOM_B

    override def requireExteriorCheck(isSourceA: Boolean): Boolean =
      // -- only need to check A against Exterior of B
      isSourceA == RelateGeometry.GEOM_A

    override def init(dimA: Int, dimB: Int): Unit = {
      super.init(dimA, dimB)
      require(IMPredicate.isDimsCompatibleWithCovers(dimB, dimA))
    }

    override def init(envA: Envelope, envB: Envelope): Unit =
      requireCovers(envB, envA)

    override def isDetermined: Boolean =
      intersectsExteriorOf(RelateGeometry.GEOM_B)

    override def valueIM: Boolean =
      intMatrix.isCoveredBy
  }

  /**
   * Creates a predicate to determine whether a geometry crosses another geometry. <p> The
   * <code>crosses</code> predicate has the following equivalent definitions: <ul> <li>The
   * geometries have some but not all interior points in common. <li>The DE-9IM Intersection Matrix
   * for the two geometries matches one of the following patterns: <ul> <li><code>[T*T******]</code>
   * (for P/L, P/A, and L/A cases) <li><code>[T*****T**]</code> (for L/P, A/P, and A/L cases)
   * <li><code>[0********]</code> (for L/L cases) </ul> </ul> For the A/A and P/P cases this
   * predicate returns <code>false</code>. <p> The SFS defined this predicate only for P/L, P/A,
   * L/L, and L/A cases. To make the relation symmetric JTS extends the definition to apply to L/P,
   * A/P and A/L cases as well.
   *
   * return the predicate instance
   */
  def crosses(): TopologyPredicate = new IMPredicate {

    override def name: String = "crosses"

    override def init(dimA: Int, dimB: Int): Unit = {
      super.init(dimA, dimB)
      val isBothPointsOrAreas = (dimA == Dimension.P && dimB == Dimension.P)
        || (dimA == Dimension.A && dimB == Dimension.A)
      require(!isBothPointsOrAreas)
    }

    override def isDetermined: Boolean = {
      if (dimA == Dimension.L && dimB == Dimension.L) {
        // -- L/L interaction can only be dim = P
        if (getDimension(Location.INTERIOR, Location.INTERIOR) > Dimension.P)
          return true
      } else if (dimA < dimB) {
        if (
          isIntersects(Location.INTERIOR, Location.INTERIOR)
          && isIntersects(Location.INTERIOR, Location.EXTERIOR)
        )
          return true
      } else if (dimA > dimB) {
        if (
          isIntersects(Location.INTERIOR, Location.INTERIOR)
          && isIntersects(Location.EXTERIOR, Location.INTERIOR)
        )
          return true
      }
      false
    }

    override def valueIM: Boolean =
      intMatrix.isCrosses(dimA, dimB)
  }

  /**
   * Creates a predicate to determine whether two geometries are topologically equal. <p> The
   * <code>equals</code> predicate has the following equivalent definitions: <ul> <li>The two
   * geometries have at least one point in common, and no point of either geometry lies in the
   * exterior of the other geometry. <li>The DE-9IM Intersection Matrix for the two geometries
   * matches the pattern <code>T*F**FFF*</code> </ul>
   *
   * return the predicate instance
   */
  def equalsTopo(): TopologyPredicate = new IMPredicate {

    override def name: String = "equals"

    override def init(dimA: Int, dimB: Int): Unit = {
      super.init(dimA, dimB)
      require(dimA == dimB)
    }

    override def init(envA: Envelope, envB: Envelope): Unit =
      require(envA.equals(envB))

    override def isDetermined: Boolean = {
      val isEitherExteriorIntersects =
        isIntersects(Location.INTERIOR, Location.EXTERIOR)
          || isIntersects(Location.BOUNDARY, Location.EXTERIOR)
          || isIntersects(Location.EXTERIOR, Location.INTERIOR)
          || isIntersects(Location.EXTERIOR, Location.BOUNDARY)

      isEitherExteriorIntersects
    }

    override def valueIM: Boolean =
      intMatrix.isEquals(dimA, dimB)
  }

  /**
   * Creates a predicate to determine whether a geometry overlaps another geometry. <p> The
   * <code>overlaps</code> predicate has the following equivalent definitions: <ul> <li>The
   * geometries have at least one point each not shared by the other (or equivalently neither covers
   * the other), they have the same dimension, and the intersection of the interiors of the two
   * geometries has the same dimension as the geometries themselves. <li>The DE-9IM Intersection
   * Matrix for the two geometries matches <code>[T*T***T**]</code> (for P/P and A/A cases) or
   * <code>[1*T***T**]</code> (for L/L cases) </ul> If the geometries are of different dimension
   * this predicate returns <code>false</code>. This predicate is symmetric.
   *
   * return the predicate instance
   */
  def overlaps(): TopologyPredicate = new IMPredicate {

    override def name: String = "overlaps"

    override def init(dimA: Int, dimB: Int): Unit = {
      super.init(dimA, dimB)
      require(dimA == dimB)
    }

    override def isDetermined: Boolean = {
      if (dimA == Dimension.A || dimA == Dimension.P) {
        if (
          isIntersects(Location.INTERIOR, Location.INTERIOR)
          && isIntersects(Location.INTERIOR, Location.EXTERIOR)
          && isIntersects(Location.EXTERIOR, Location.INTERIOR)
        )
          return true
      }
      if (dimA == Dimension.L) {
        if (
          isDimension(Location.INTERIOR, Location.INTERIOR, Dimension.L)
          && isIntersects(Location.INTERIOR, Location.EXTERIOR)
          && isIntersects(Location.EXTERIOR, Location.INTERIOR)
        )
          return true
      }
      false
    }

    override def valueIM: Boolean =
      intMatrix.isOverlaps(dimA, dimB)
  }

  /**
   * Creates a predicate to determine whether a geometry touches another geometry. <p> The
   * <code>touches</code> predicate has the following equivalent definitions: <ul> <li>The
   * geometries have at least one point in common, but their interiors do not intersect. <li>The
   * DE-9IM Intersection Matrix for the two geometries matches at least one of the following
   * patterns <ul> <li><code>[FT*******]</code> <li><code>[F**T*****]</code>
   * <li><code>[F***T****]</code> </ul> </ul> If both geometries have dimension 0, the predicate
   * returns <code>false</code>, since points have only interiors. This predicate is symmetric.
   *
   * return the predicate instance
   */
  def touches(): TopologyPredicate = new IMPredicate {

    override def name: String = "touches"

    override def init(dimA: Int, dimB: Int): Unit = {
      super.init(dimA, dimB)
      // -- Points have only interiors, so cannot touch
      val isBothPoints = dimA == 0 && dimB == 0
      require(!isBothPoints)
    }

    override def isDetermined: Boolean = {
      // -- for touches interiors cannot intersect
      val isInteriorsIntersects = isIntersects(Location.INTERIOR, Location.INTERIOR)
      isInteriorsIntersects
    }

    override def valueIM: Boolean =
      intMatrix.isTouches(dimA, dimB)
  }

  /**
   * Creates a predicate that matches a DE-9IM matrix pattern.
   *
   * @param imPattern
   *   the pattern to match return a predicate that matches the pattern
   *
   * @see
   *   IntersectionMatrixPattern
   */
  def matches(imPattern: String): TopologyPredicate =
    new IMPatternMatcher(imPattern)
}
