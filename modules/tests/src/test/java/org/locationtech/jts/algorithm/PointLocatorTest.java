/*
 * Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.algorithm;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Location;
import org.locationtech.jts.io.WKTReader;

import junit.framework.TestCase;
import junit.textui.TestRunner;

/**
 * Tests PointInRing algorithms
 *
 * @version 1.7
 */
public class PointLocatorTest extends TestCase {
  int Exterior = org.locationtech.jts.geom.Location$.MODULE$.EXTERIOR();
  int Interior = org.locationtech.jts.geom.Location$.MODULE$.INTERIOR();
  int Boundary = org.locationtech.jts.geom.Location$.MODULE$.BOUNDARY();

  private WKTReader reader = new WKTReader();

  public static void main(String args[]) {
    TestRunner.run(PointLocatorTest.class);
  }

  public PointLocatorTest(String name) { super(name); }

  public void testBox() throws Exception
  {
    runPtLocator(Interior, new Coordinate(10, 10),
"POLYGON ((0 0, 0 20, 20 20, 20 0, 0 0))");
  }

  public void testComplexRing() throws Exception
  {
    runPtLocator(Interior, new Coordinate(0, 0),
"POLYGON ((-40 80, -40 -80, 20 0, 20 -100, 40 40, 80 -80, 100 80, 140 -20, 120 140, 40 180,     60 40, 0 120, -20 -20, -40 80))");
  }

  public void testLinearRingLineString() throws Exception
  {
    runPtLocator(Boundary, new Coordinate(0, 0),
                 "GEOMETRYCOLLECTION( LINESTRING(0 0, 10 10), LINEARRING(10 10, 10 20, 20 10, 10 10))");
  }

  public void testPointInsideLinearRing() throws Exception
  {
    runPtLocator(Exterior, new Coordinate(11, 11),
                 "LINEARRING(10 10, 10 20, 20 10, 10 10)");
  }

  public void testPolygon() throws Exception {
    PointLocator pointLocator = new PointLocator(BoundaryNodeRule$.MODULE$.OGC_SFS_BOUNDARY_RULE());
    Geometry polygon = reader.read("POLYGON ((70 340, 430 50, 70 50, 70 340))");
    assertEquals(Exterior, pointLocator.locate(new Coordinate(420, 340), polygon));
    assertEquals(Boundary, pointLocator.locate(new Coordinate(350, 50), polygon));
    assertEquals(Boundary, pointLocator.locate(new Coordinate(410, 50), polygon));
    assertEquals(Interior, pointLocator.locate(new Coordinate(190, 150), polygon));
  }

   private void runPtLocator(int expected, Coordinate pt, String wkt)
      throws Exception
  {
    Geometry geom = reader.read(wkt);
    PointLocator pointLocator = new PointLocator(BoundaryNodeRule$.MODULE$.OGC_SFS_BOUNDARY_RULE());
    int loc = pointLocator.locate(pt, geom);
    assertEquals(expected, loc);
  }

}
