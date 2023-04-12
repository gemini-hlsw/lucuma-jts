/*
 * Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.util;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;

import test.jts.GeometryTestCase;

/**
 * @version 1.7
 */
public class UniqueCoordinateArrayFilterTest
    extends GeometryTestCase
{
  public UniqueCoordinateArrayFilterTest(String name) {
    super(name);
  }

  public static void main(String[] args) {
    junit.textui.TestRunner.run(UniqueCoordinateArrayFilterTest.class);
  }

  public void testFilter() throws Exception {
    Geometry g = read(
          "MULTIPOINT(10 10, 20 20, 30 30, 20 20, 10 10)");
    UniqueCoordinateArrayFilter f = new UniqueCoordinateArrayFilter();
    g.applyF(f);
    assertEquals(3, f.getCoordinates().length);
    assertEquals(new Coordinate(10, 10), f.getCoordinates()[0]);
    assertEquals(new Coordinate(20, 20), f.getCoordinates()[1]);
    assertEquals(new Coordinate(30, 30), f.getCoordinates()[2]);
  }

}
