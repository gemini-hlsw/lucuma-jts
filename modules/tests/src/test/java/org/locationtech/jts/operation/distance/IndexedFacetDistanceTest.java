/*
 * Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.operation.distance;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;

import junit.textui.TestRunner;

public class IndexedFacetDistanceTest extends BaseDistanceTest {

  public static void main(String args[]) {
    TestRunner.run(IndexedFacetDistanceTest.class);
  }

  public IndexedFacetDistanceTest(String name) {
    super(name);
  }

  protected Coordinate[] nearestPoints(Geometry g1, Geometry g2) {
    return IndexedFacetDistance$.MODULE$.nearestPoints(g1, g2);
  }

  public void testClosestPoints7() {
    // skip this test for now, since it relies on checking point-in-polygon
  }

  @Override
  protected double distance(Geometry g1, Geometry g2) {
    return IndexedFacetDistance$.MODULE$.distance(g1,g2);
  }

  @Override
  protected boolean isWithinDistance(Geometry g1, Geometry g2, double distance) {
    return IndexedFacetDistance$.MODULE$.isWithinDistance(g1,g2, distance);
  }
}
