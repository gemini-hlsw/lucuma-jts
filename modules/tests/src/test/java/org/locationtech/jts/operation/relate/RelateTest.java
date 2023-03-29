/*
 * Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.operation.relate;

import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.IntersectionMatrix;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKTReader;

import junit.framework.TestCase;
import junit.textui.TestRunner;


/**
 * Tests {@link Geometry#relate}.
 *
 * @author Martin Davis
 * @version 1.7
 */
public class RelateTest
    extends TestCase
{
  public static void main(String args[]) {
    TestRunner.run(RelateTest.class);
  }

  private GeometryFactory fact = new GeometryFactory();
  private WKTReader rdr = new WKTReader(fact);

  public RelateTest(String name)
  {
    super(name);
  }

  /**
   * From GEOS #572
   *
   * The cause is that the longer line nodes the single-segment line.
   * The node then tests as not lying precisely on the original longer line.
   *
   * @throws Exception
   */
  // public void testContainsIncorrectIMMatrix()
  //     throws Exception
  // {
  //   String a = "LINESTRING (1 0, 0 2, 0 0, 2 2)";
  //   String b = "LINESTRING (0 0, 2 2)";
  //
  //   // actual matrix is 001F001F2
  //   // true matrix should be 101F00FF2
  //   runRelateTest(a, b,  "001F001F2"    );
  // }

  public void testContainsCase()
          throws Exception
  {
    String a = "POLYGON ((0 0, 0 50, 50 50, 50 0, 0 0))";
    String b = "POLYGON ((0 0, 0 50, 50 50, 50 0, 0 0))";

    // actual matrix is 001F001F2
    // true matrix should be 101F00FF2
    runRelateTest(a, b,  "2FFF1FFF2"    );
  }

  void runRelateTest(String wkt1, String wkt2, String expectedIM)
      throws ParseException
  {
    Geometry g1 = rdr.read(wkt1);
    Geometry g2 = rdr.read(wkt2);
    IntersectionMatrix im = RelateOp$.MODULE$.relate(g1, g2);
    String imStr = im.toString();
    System.out.println(imStr);
    assertTrue(im.matches(expectedIM));
  }
}