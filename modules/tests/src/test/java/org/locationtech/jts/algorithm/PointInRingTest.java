/*
 * Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.algorithm;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Location;
import org.locationtech.jts.io.WKTReader;

import junit.textui.TestRunner;

/**
 * Tests PointInRing algorithms
 *
 * @version 1.7
 */
public class PointInRingTest extends AbstractPointInRingTest {

  private WKTReader reader = new WKTReader();
  int Boundary = org.locationtech.jts.geom.Location$.MODULE$.BOUNDARY();
  int Interir = org.locationtech.jts.geom.Location$.MODULE$.INTERIOR();

  public static void main(String args[]) {
    TestRunner.run(PointInRingTest.class);
  }

  public PointInRingTest(String name) { super(name); }


   protected void runPtInRing(int expectedLoc, Coordinate pt, String wkt)
      throws Exception
  {
  	 // isPointInRing is not defined for pts on boundary
  	 if (expectedLoc == Boundary)
  		 return;

    Geometry geom = reader.read(wkt);
    boolean expected = expectedLoc == Interior;
    assertEquals(expected, PointLocation.isInRing(pt, geom.getCoordinates()));
  }

}
