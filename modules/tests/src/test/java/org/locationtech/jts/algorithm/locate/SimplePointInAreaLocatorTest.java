/*
 * Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.algorithm.locate;

import org.locationtech.jts.algorithm.AbstractPointInRingTest;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.io.WKTReader;

import junit.textui.TestRunner;

/**
 * Tests IndexedPointInAreaLocator algorithms
 *
 * @version 1.7
 */
public class SimplePointInAreaLocatorTest extends AbstractPointInRingTest {

  private WKTReader reader = new WKTReader();

  public static void main(String args[]) {
    TestRunner.run(SimplePointInAreaLocatorTest.class);
  }

  public SimplePointInAreaLocatorTest(String name) { super(name); }


   protected void runPtInRing(int expectedLoc, Coordinate pt, String wkt)
      throws Exception
  {
    Geometry geom = reader.read(wkt);
    SimplePointInAreaLocator loc = new SimplePointInAreaLocator(geom);
    int result = loc.locate(pt);
    assertEquals(expectedLoc, result);
  }

}
