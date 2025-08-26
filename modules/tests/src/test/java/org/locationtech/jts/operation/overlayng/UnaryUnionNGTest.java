/*
 * Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.operation.overlayng;

import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.PrecisionModel;

import junit.textui.TestRunner;
import test.jts.GeometryTestCase;

public class UnaryUnionNGTest extends GeometryTestCase
{
  public static void main(String args[]) {
    TestRunner.run(UnaryUnionNGTest.class);
  }

  public UnaryUnionNGTest(String name) {
    super(name);
  }

  public void testMultiPolygonNarrowGap( ) {
    checkUnaryUnion("MULTIPOLYGON (((1 9, 5.7 9, 5.7 1, 1 1, 1 9)), ((9 9, 9 1, 6 1, 6 9, 9 9)))",
        1,
        "POLYGON ((1 9, 6 9, 9 9, 9 1, 6 1, 1 1, 1 9))");
  }

  public void testPolygonsRounded( ) {
    checkUnaryUnion("GEOMETRYCOLLECTION (POLYGON ((1 9, 6 9, 6 1, 1 1, 1 9)), POLYGON ((9 1, 2 8, 9 9, 9 1)))",
        1,
        "POLYGON ((1 9, 6 9, 9 9, 9 1, 6 4, 6 1, 1 1, 1 9))");
  }

  public void testPolygonsOverlapping( ) {
    checkUnaryUnion("GEOMETRYCOLLECTION (POLYGON ((100 200, 200 200, 200 100, 100 100, 100 200)), POLYGON ((250 250, 250 150, 150 150, 150 250, 250 250)))",
        1,
        "POLYGON ((100 200, 150 200, 150 250, 250 250, 250 150, 200 150, 200 100, 100 100, 100 200))");
  }

  private void checkUnaryUnion(String wkt, double scaleFactor, String wktExpected) {
    Geometry geom = read(wkt);
    Geometry expected = read(wktExpected);
    PrecisionModel pm = new PrecisionModel(scaleFactor);
    Geometry result = UnaryUnionNG.union(geom, pm);
    checkEqual(expected, result);
  }
}
