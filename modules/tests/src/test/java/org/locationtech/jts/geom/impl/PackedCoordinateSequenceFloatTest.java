/*
 * Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.geom.impl;

import org.locationtech.jts.geom.CoordinateSequence;
import org.locationtech.jts.geom.CoordinateSequenceFactory;

import junit.textui.TestRunner;

/**
 * Test {@link PackedCoordinateSequence.Float}
 * using the {@link CoordinateSequenceTestBase}
 *
 * @version 1.7
 */
public class PackedCoordinateSequenceFloatTest
    extends CoordinateSequenceTestBase
{
  public static void main(String args[]) {
    TestRunner.run(PackedCoordinateSequenceFloatTest.class);
  }

  public PackedCoordinateSequenceFloatTest(String name)
  {
    super(name);
  }

  @Override
  CoordinateSequenceFactory getCSFactory() {
    return PackedCoordinateSequenceFactory.FLOAT_FACTORY();
  }

  public void test4dCoordinateSequence() {
    CoordinateSequence cs = new PackedCoordinateSequenceFactory(PackedCoordinateSequenceFactory.FLOAT())
            .create(new float[]{0.0f,1.0f,2.0f,3.0f,4.0f,5.0f,6.0f,7.0f}, 4);
    assertEquals(2.0, cs.getCoordinate(0).getZ());
    assertEquals(3.0, cs.getCoordinate(0).getM());
  }
}
