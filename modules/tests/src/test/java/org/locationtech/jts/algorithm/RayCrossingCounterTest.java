/*
 * Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.algorithm;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.CoordinateSequence;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Location;
import org.locationtech.jts.geom.impl.PackedCoordinateSequenceFactory;
import org.locationtech.jts.io.WKTReader;

import junit.textui.TestRunner;

/**
 * Tests PointInRing algorithms
 *
 * @version 1.7
 */
public class RayCrossingCounterTest extends AbstractPointInRingTest {

  private WKTReader reader = new WKTReader();

  public static void main(String args[]) {
    TestRunner.run(RayCrossingCounterTest.class);
    //new RayCrossingCounterTest("RayCrossingCounterTest").testRunPtInRing4d();
  }

  public RayCrossingCounterTest(String name) { super(name); }

  protected void runPtInRing(int expectedLoc, Coordinate pt, String wkt)
          throws Exception
  {
    Geometry geom = reader.read(wkt);
    assertEquals(expectedLoc, RayCrossingCounter.locatePointInRing(pt, geom.getCoordinates()));
  }

  public void testRunPtInRing4d()
  {
    CoordinateSequence cs = new PackedCoordinateSequenceFactory(PackedCoordinateSequenceFactory.DOUBLE())
            .create(new double[]{
                    0.0, 0.0, 0.0, 0.0,
                    10.0, 0.0, 0.0, 0.0,
                    5.0, 10.0, 0.0, 0.0,
                    0.0, 0.0, 0.0, 0.0
            }, 4, 1);
    assertEquals(Interior, RayCrossingCounter.locatePointInRing(new Coordinate(5.0, 2.0), cs));
  }

}
