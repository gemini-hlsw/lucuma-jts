/*
 * Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.algorithm.locate;

import org.locationtech.jts.algorithm.AbstractPointInRingTest;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Location;
import org.locationtech.jts.io.WKTReader;

import junit.textui.TestRunner;

/**
 * Tests IndexedPointInAreaLocator algorithms
 *
 * @version 1.7
 */
public class IndexedPointInAreaLocatorTest extends AbstractPointInRingTest {
  int Exterior = org.locationtech.jts.geom.Location$.MODULE$.EXTERIOR();

  private WKTReader reader = new WKTReader();

  public static void main(String args[]) {
    TestRunner.run(IndexedPointInAreaLocatorTest.class);
  }

  public IndexedPointInAreaLocatorTest(String name) { super(name); }


   protected void runPtInRing(int expectedLoc, Coordinate pt, String wkt)
      throws Exception
  {
    Geometry geom = reader.read(wkt);
    IndexedPointInAreaLocator loc = new IndexedPointInAreaLocator(geom);
    int result = loc.locate(pt);
    assertEquals(expectedLoc, result);
  }

   /**
    * See JTS GH Issue #19.
    * Used to infinite-loop on empty geometries.
    *
    * @throws Exception
    */
   public void testEmpty() throws Exception {
     runPtInRing(Exterior, new Coordinate(0,0), "POLYGON EMPTY");
  }
}
