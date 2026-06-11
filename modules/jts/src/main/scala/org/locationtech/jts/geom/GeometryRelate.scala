// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

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
package org.locationtech.jts.geom

import org.locationtech.jts.operation.relateng.RelateNG
import org.locationtech.jts.operation.relateng.RelatePredicate

/**
 * Internal class which routes the {link Geometry} predicate methods to {link RelateNG}.
 *
 * Upstream JTS switches between the legacy RelateOp and RelateNG via the <code>jts.relate</code>
 * system property; this port uses RelateNG unconditionally (system properties are not meaningful on
 * Scala.js).
 *
 * @author
 *   mdavis
 */
private[geom] object GeometryRelate {

  def intersects(a: Geometry, b: Geometry): Boolean =
    RelateNG.relate(a, b, RelatePredicate.intersects())

  def contains(a: Geometry, b: Geometry): Boolean =
    RelateNG.relate(a, b, RelatePredicate.contains())

  def covers(a: Geometry, b: Geometry): Boolean =
    RelateNG.relate(a, b, RelatePredicate.covers())

  def coveredBy(a: Geometry, b: Geometry): Boolean =
    RelateNG.relate(a, b, RelatePredicate.coveredBy())

  def crosses(a: Geometry, b: Geometry): Boolean =
    RelateNG.relate(a, b, RelatePredicate.crosses())

  def disjoint(a: Geometry, b: Geometry): Boolean =
    RelateNG.relate(a, b, RelatePredicate.disjoint())

  def equalsTopo(a: Geometry, b: Geometry): Boolean =
    RelateNG.relate(a, b, RelatePredicate.equalsTopo())

  def overlaps(a: Geometry, b: Geometry): Boolean =
    RelateNG.relate(a, b, RelatePredicate.overlaps())

  def touches(a: Geometry, b: Geometry): Boolean =
    RelateNG.relate(a, b, RelatePredicate.touches())

  def within(a: Geometry, b: Geometry): Boolean =
    RelateNG.relate(a, b, RelatePredicate.within())

  def relate(a: Geometry, b: Geometry): IntersectionMatrix =
    RelateNG.relate(a, b)

  def relate(a: Geometry, b: Geometry, intersectionPattern: String): Boolean =
    RelateNG.relate(a, b, intersectionPattern)
}
