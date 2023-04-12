/*
 * Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

/*
 * Copyright (c) 2016 Vivid Solutions.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v20.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.locationtech.jts.precision;

import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.PrecisionModel;
import org.locationtech.jts.io.WKTReader;

import junit.textui.TestRunner;
import test.jts.GeometryTestCase;


/**
 * @version 1.12
 */
public class GeometryPrecisionReducerTest
    extends GeometryTestCase
{
  private PrecisionModel pmFloat = new PrecisionModel();
  private PrecisionModel pmFixed1 = new PrecisionModel(1);
  private GeometryPrecisionReducer reducer = new GeometryPrecisionReducer(pmFixed1);
  private GeometryPrecisionReducer reducerKeepCollapse
  = new GeometryPrecisionReducer(pmFixed1);

  private GeometryFactory gfFloat = new GeometryFactory(pmFloat, 0);
  WKTReader reader = new WKTReader(gfFloat);

  public static void main(String args[]) {
    TestRunner.run(GeometryPrecisionReducerTest.class);
  }

  public GeometryPrecisionReducerTest(String name)
  {
    super(name);
    reducerKeepCollapse.setRemoveCollapsedComponents(false);
  }

  public void testSquare()
      throws Exception
  {
    checkReduce("POLYGON (( 0 0, 0 1.4, 1.4 1.4, 1.4 0, 0 0 ))",
        "POLYGON (( 0 0, 0 1, 1 1, 1 0, 0 0 ))");
  }
  public void testTinySquareCollapse()
      throws Exception
  {
    checkReduce("POLYGON (( 0 0, 0 .4, .4 .4, .4 0, 0 0 ))",
        "POLYGON EMPTY");
  }

  public void testSquareCollapse()
      throws Exception
  {
    checkReduce("POLYGON (( 0 0, 0 1.4, .4 .4, .4 0, 0 0 ))",
        "POLYGON EMPTY");
  }

  public void testSquareKeepCollapse()
      throws Exception
  {
    checkReduce("POLYGON (( 0 0, 0 1.4, .4 .4, .4 0, 0 0 ))",
        "POLYGON EMPTY");
  }

  // see https://github.com/locationtech/jts/issues/324
  public void testPolygonCollapsesCompletely()
      throws Exception
  {
    checkReduce(1000000, "POLYGON ((-121.816901763 37.3285521, -121.817392418 37.328343575, -121.817876714 37.328137752, -121.818253966 37.327977421, -121.8184719 37.3278848, -121.816901763 37.3285521))",
        "POLYGON EMPTY");
  }

  // see https://sourceforge.net/p/jts-topo-suite/bugs/33/
  public void testPolygonHasValidResult()
      throws Exception
  {
    checkReduce(10,
"POLYGON ((563539.9999829987 6395531.999987871, 558495.0908829987 6395531.999987871, 558495.0914829987 6395533.599987871, 558495.1972829987 6395837.757887871, 558496.0278829987 6395838.339487871, 558497.2604829987 6395839.572087871, 558498.2602829987 6395841.000087871, 558498.9968829987 6395842.579887872, 558499.4480829986 6395844.263487872, 558499.5996829987 6395845.920087871, 558499.9998829986 6395895.974887871, 558499.5998829987 6396031.229687871, 558499.4480829986 6396032.936487871, 558498.9968829987 6396034.620287871, 558498.2602829987 6396036.200087871, 558497.2604829987 6396037.627887871, 558496.0278829987 6396038.860487871, 558495.1938829987 6396039.444487872, 558494.8000829987 6396563.233287871, 558494.3998829987 6396631.258887871, 558494.2480829987 6396632.936487871, 558493.7968829987 6396634.620287871, 558493.0602829987 6396636.200087871, 558492.0604829987 6396637.627887871, 558490.8278829987 6396638.860487871, 558490.3932829987 6396639.164887872, 558490.0000829987 6397089.618487871, 558489.5998829987 6397231.228287871, 558489.4480829986 6397232.936487871, 558488.9968829987 6397234.620287871, 558488.2602829987 6397236.200087871, 558487.2604829987 6397237.627887871, 558486.0278829987 6397238.860487871, 558485.1890829987 6397239.447887871, 558484.4000829987 6397831.213287871, 558484.2480829987 6397832.936487871, 558483.7968829987 6397834.620287871, 558483.0602829987 6397836.200087871, 558482.0604829987 6397837.627887871, 558480.8278829987 6397838.860487871, 558479.9948829986 6397839.443887872, 558479.6000829986 6398431.206687871, 558479.4480829986 6398432.936487871, 558478.9968829987 6398434.620287871, 558478.2602829987 6398436.200087871, 558477.2604829987 6398437.627887871, 558476.0278829987 6398438.860487871, 558475.1890829987 6398439.447887871, 558474.4000829987 6399031.213287871, 558474.2480829987 6399032.936487871, 558473.7968829987 6399034.620287871, 558473.0602829987 6399036.200087871, 558472.0604829987 6399037.627887871, 558470.8278829987 6399038.860487871, 558470.3894829986 6399039.167487871, 558469.6000829986 6399631.213287871, 558469.4480829986 6399632.936487871, 558468.9968829987 6399634.620287871, 558468.2602829987 6399636.200087871, 558467.2604829987 6399637.627887871, 558466.0278829987 6399638.860487871, 558465.1890829987 6399639.447887871, 558464.4000829987 6400231.213287871, 558464.2480829987 6400232.936487871, 558463.7968829987 6400234.620287871, 558463.0602829987 6400236.200087871, 558462.0604829987 6400237.627887871, 558460.8278829987 6400238.860487871, 558459.9948829986 6400239.443887872, 558459.6000829986 6400831.206687871, 558459.4480829986 6400832.936487871, 558458.9968829987 6400834.620287871, 558458.2602829987 6400836.200087871, 558457.2604829987 6400837.627887871, 558456.0278829987 6400838.860487871, 558455.1940829987 6400839.444487872, 558454.7998829987 6401378.041287871, 558454.3996829987 6401431.275087872, 558454.2480829987 6401432.936487871, 558453.7968829987 6401434.620287871, 558453.0602829987 6401436.200087871, 558452.0604829987 6401437.627887871, 558450.8278829987 6401438.860487871, 558450.3936829987 6401439.164687871, 558450.0000829987 6401919.622087872, 558449.5998829987 6402031.235887871, 558449.4480829986 6402032.936487871, 558448.9968829987 6402034.620287871, 558448.2602829987 6402036.200087871, 558447.2604829987 6402037.627887871, 558446.0278829987 6402038.860487871, 558445.1890829987 6402039.447887871, 558444.4000829987 6402631.213287871, 558444.2480829987 6402632.936487871, 558443.7968829987 6402634.620287871, 558443.0602829987 6402636.200087871, 558442.0604829987 6402637.627887871, 558440.8278829987 6402638.860487871, 558439.9948829986 6402639.443887872, 558439.6000829986 6403231.206687871, 558439.4480829986 6403232.936487871, 558438.9968829987 6403234.620287871, 558438.2602829987 6403236.200087871, 558437.2604829987 6403237.627887871, 558436.0278829987 6403238.860487871, 558435.1890829987 6403239.447887871, 558434.4000829987 6403831.213287871, 558434.2480829987 6403832.936487871, 558433.7968829987 6403834.620287871, 558433.0602829987 6403836.200087871, 558432.0604829987 6403837.627887871, 558430.8278829987 6403838.860487871, 558430.3894829986 6403839.167487871, 558429.6000829986 6404431.213287871, 558429.4480829986 6404432.936487871, 558428.9968829987 6404434.620287871, 558428.2602829987 6404436.200087871, 558427.2604829987 6404437.627887871, 558426.0278829987 6404438.860487871, 558425.1890829987 6404439.447887871, 558424.4000829987 6405031.213287871, 558424.2480829987 6405032.936487871, 558423.7968829987 6405034.620287871, 558423.0602829987 6405036.200087871, 558422.0604829987 6405037.627887871, 558420.8278829987 6405038.860487871, 558419.9948829986 6405039.443887872, 558419.6000829986 6405631.206687871, 558419.4480829986 6405632.936487871, 558418.9968829987 6405634.620287871, 558418.2602829987 6405636.200087871, 558417.2604829987 6405637.627887871, 558416.0278829987 6405638.860487871, 558415.1940829987 6405639.444487872, 558414.7998829987 6406180.843287871, 558414.3996829987 6406231.279487872, 558414.2480829987 6406232.936487871, 558413.7968829987 6406234.620287871, 558413.0602829987 6406236.200087871, 558412.0604829987 6406237.627887871, 558410.8278829987 6406238.860487871, 558410.3936829987 6406239.164687871, 558410.0000829987 6406725.222887871, 558409.5998829987 6406831.237687871, 558409.4480829986 6406832.936487871, 558408.9968829987 6406834.620287871, 558408.2602829987 6406836.200087871, 558407.2604829987 6406837.627887871, 558406.0278829987 6406838.860487871, 558405.1890829987 6406839.447887871, 558404.4000829987 6407431.213287871, 558404.2480829987 6407432.936487871, 558403.7968829987 6407434.620287871, 558403.0602829987 6407436.200087871, 558402.0604829987 6407437.627887871, 558400.8278829987 6407438.860487871, 558400.3894829986 6407439.167487871, 558399.6000829986 6408031.213287871, 558399.4480829986 6408032.936487871, 558398.9968829987 6408034.620287871, 558398.2602829987 6408036.200087871, 558397.2604829987 6408037.627887871, 558396.0278829987 6408038.860487871, 558394.7946829987 6408039.724087872, 558394.4000829987 6408631.206687871, 558394.2480829987 6408632.936487871, 558393.7968829987 6408634.620287871, 558393.0602829987 6408636.200087871, 558392.0799829987 6408637.599987871, 558392.0604829987 6408637.627887871, 558390.8278829987 6408638.860487871, 558390.3894829986 6408639.167487871, 558390.3894829986 6408639.199987872, 563539.9999829987 6408639.199987872, 563541.5999829987 6408639.199987872, 563541.5999829987 6408637.599987871, 563541.5999829987 6395533.599987871, 563541.5999829987 6395531.999987871, 563539.9999829987 6395531.999987871))",
"POLYGON ((558495.1 6395532, 558495.1 6395533.6, 558495.2 6395837.8, 558496 6395838.3, 558497.3 6395839.6, 558498.3 6395841, 558499 6395842.6, 558499.4 6395844.3, 558499.6 6395845.9, 558500 6395896, 558499.6 6396031.2, 558499.4 6396032.9, 558499 6396034.6, 558498.3 6396036.2, 558497.3 6396037.6, 558496 6396038.9, 558495.2 6396039.4, 558494.8 6396563.2, 558494.4 6396631.3, 558494.2 6396632.9, 558493.8 6396634.6, 558493.1 6396636.2, 558492.1 6396637.6, 558490.8 6396638.9, 558490.4 6396639.2, 558490 6397089.6, 558489.6 6397231.2, 558489.4 6397232.9, 558489 6397234.6, 558488.3 6397236.2, 558487.3 6397237.6, 558486 6397238.9, 558485.2 6397239.4, 558484.4 6397831.2, 558484.2 6397832.9, 558483.8 6397834.6, 558483.1 6397836.2, 558482.1 6397837.6, 558480.8 6397838.9, 558480 6397839.4, 558479.6 6398431.2, 558479.4 6398432.9, 558479 6398434.6, 558478.3 6398436.2, 558477.3 6398437.6, 558476 6398438.9, 558475.2 6398439.4, 558474.4 6399031.2, 558474.2 6399032.9, 558473.8 6399034.6, 558473.1 6399036.2, 558472.1 6399037.6, 558470.8 6399038.9, 558470.4 6399039.2, 558469.6 6399631.2, 558469.4 6399632.9, 558469 6399634.6, 558468.3 6399636.2, 558467.3 6399637.6, 558466 6399638.9, 558465.2 6399639.4, 558464.4 6400231.2, 558464.2 6400232.9, 558463.8 6400234.6, 558463.1 6400236.2, 558462.1 6400237.6, 558460.8 6400238.9, 558460 6400239.4, 558459.6 6400831.2, 558459.4 6400832.9, 558459 6400834.6, 558458.3 6400836.2, 558457.3 6400837.6, 558456 6400838.9, 558455.2 6400839.4, 558454.8 6401378, 558454.4 6401431.3, 558454.2 6401432.9, 558453.8 6401434.6, 558453.1 6401436.2, 558452.1 6401437.6, 558450.8 6401438.9, 558450.4 6401439.2, 558450 6401919.6, 558449.6 6402031.2, 558449.4 6402032.9, 558449 6402034.6, 558448.3 6402036.2, 558447.3 6402037.6, 558446 6402038.9, 558445.2 6402039.4, 558444.4 6402631.2, 558444.2 6402632.9, 558443.8 6402634.6, 558443.1 6402636.2, 558442.1 6402637.6, 558440.8 6402638.9, 558440 6402639.4, 558439.6 6403231.2, 558439.4 6403232.9, 558439 6403234.6, 558438.3 6403236.2, 558437.3 6403237.6, 558436 6403238.9, 558435.2 6403239.4, 558434.4 6403831.2, 558434.2 6403832.9, 558433.8 6403834.6, 558433.1 6403836.2, 558432.1 6403837.6, 558430.8 6403838.9, 558430.4 6403839.2, 558429.6 6404431.2, 558429.4 6404432.9, 558429 6404434.6, 558428.3 6404436.2, 558427.3 6404437.6, 558426 6404438.9, 558425.2 6404439.4, 558424.4 6405031.2, 558424.2 6405032.9, 558423.8 6405034.6, 558423.1 6405036.2, 558422.1 6405037.6, 558420.8 6405038.9, 558420 6405039.4, 558419.6 6405631.2, 558419.4 6405632.9, 558419 6405634.6, 558418.3 6405636.2, 558417.3 6405637.6, 558416 6405638.9, 558415.2 6405639.4, 558414.8 6406180.8, 558414.4 6406231.3, 558414.2 6406232.9, 558413.8 6406234.6, 558413.1 6406236.2, 558412.1 6406237.6, 558410.8 6406238.9, 558410.4 6406239.2, 558410 6406725.2, 558409.6 6406831.2, 558409.4 6406832.9, 558409 6406834.6, 558408.3 6406836.2, 558407.3 6406837.6, 558406 6406838.9, 558405.2 6406839.4, 558404.4 6407431.2, 558404.2 6407432.9, 558403.8 6407434.6, 558403.1 6407436.2, 558402.1 6407437.6, 558400.8 6407438.9, 558400.4 6407439.2, 558399.6 6408031.2, 558399.4 6408032.9, 558399 6408034.6, 558398.3 6408036.2, 558397.3 6408037.6, 558396 6408038.9, 558394.8 6408039.7, 558394.4 6408631.2, 558394.2 6408632.9, 558393.8 6408634.6, 558393.1 6408636.2, 558392.1 6408637.6, 558390.8 6408638.9, 558390.4 6408639.2, 563540 6408639.2, 563541.6 6408639.2, 563541.6 6408637.6, 563541.6 6395533.6, 563541.6 6395532, 563540 6395532, 558495.1 6395532))");
  }

  public void testLine()
      throws Exception
  {
    checkReduceExact("LINESTRING ( 0 0, 0 1.4 )",
        "LINESTRING (0 0, 0 1)");
  }

  public void testLineNotNoded()
      throws Exception
  {
    checkReduceExact("LINESTRING(1 1, 3 3, 9 9, 5.1 5, 2.1 2)",
        "LINESTRING(1 1, 3 3, 9 9, 5 5, 2 2)");
  }

  public void testLineRemoveCollapse()
      throws Exception
  {
    checkReduceExact("LINESTRING ( 0 0, 0 .4 )",
        "LINESTRING EMPTY");
  }

  /**
   * Disabled for now.
   * @throws Exception
   */
  public void xtestLineKeepCollapse()
      throws Exception
  {
    checkReduceExactSameFactory(reducerKeepCollapse,
        "LINESTRING ( 0 0, 0 .4 )",
        "LINESTRING ( 0 0, 0 0 )");
  }

  public void testPoint()
      throws Exception
  {
    checkReduceExact("POINT(1.1 4.9)",
        "POINT(1 5)");
  }

  public void testMultiPoint()
      throws Exception
  {
    checkReduceExact("MULTIPOINT( (1.1 4.9),(1.2 4.8), (3.3 6.6))",
        "MULTIPOINT((1 5), (1 5), (3 7))");
  }

  public void testPolgonWithCollapsedLine() throws Exception {
    checkReduce("POLYGON ((10 10, 100 100, 200 10.1, 300 10, 10 10))",
        "POLYGON ((10 10, 100 100, 200 10, 10 10))");
	}

  public void testPolgonWithCollapsedPoint() throws Exception {
    checkReduce("POLYGON ((10 10, 100 100, 200 10.1, 300 100, 400 10, 10 10))",
        "MULTIPOLYGON (((10 10, 100 100, 200 10, 10 10)), ((200 10, 300 100, 400 10, 200 10)))");
  }

  public void testMultiPolgonCollapse() throws Exception {
    checkReduce("MULTIPOLYGON (((1 9, 5 9, 5 1, 1 1, 1 9)), ((5.2 8.7, 9 8.7, 9 1, 5.2 1, 5.2 8.7)))",
        "POLYGON ((1 1, 1 9, 5 9, 9 9, 9 1, 5 1, 1 1))");
  }

  public void testGC() throws Exception {
    checkReduce(
        "GEOMETRYCOLLECTION (POINT (1.1 2.2), MULTIPOINT ((1.1 2), (3.1 3.9)), LINESTRING (1 2.1, 3 3.9), MULTILINESTRING ((1 2, 3 4), (5 6, 7 8)), POLYGON ((2 2, -2 2, -2 -2, 2 -2, 2 2), (1 1, 1 -1, -1 -1, -1 1, 1 1)), MULTIPOLYGON (((2 2, -2 2, -2 -2, 2 -2, 2 2), (1 1, 1 -1, -1 -1, -1 1, 1 1)), ((7 2, 3 2, 3 -2, 7 -2, 7 2))))",
        "GEOMETRYCOLLECTION (POINT (1 2),     MULTIPOINT ((1 2), (3 4)),       LINESTRING (1 2, 3 4),     MULTILINESTRING ((1 2, 3 4), (5 6, 7 8)), POLYGON ((2 2, -2 2, -2 -2, 2 -2, 2 2), (1 1, 1 -1, -1 -1, -1 1, 1 1)), MULTIPOLYGON (((2 2, -2 2, -2 -2, 2 -2, 2 2), (1 1, 1 -1, -1 -1, -1 1, 1 1)), ((7 2, 3 2, 3 -2, 7 -2, 7 2))))"
        );
  }

  public void testGCPolygonCollapse() throws Exception {
    checkReduce(
        "GEOMETRYCOLLECTION (POINT (1.1 2.2), POLYGON ((10 10, 100 100, 200 10.1, 300 100, 400 10, 10 10)) )",
        "GEOMETRYCOLLECTION (POINT (1 2),     MULTIPOLYGON (((10 10, 100 100, 200 10, 10 10)), ((200 10, 300 100, 400 10, 200 10))) )"
        );
  }

  public void testGCNested() throws Exception {
    checkReduce(
        "GEOMETRYCOLLECTION (POINT (1.1 2.2), GEOMETRYCOLLECTION( POINT (1.1 2.2), LINESTRING (1 2.1, 3 3.9) ) )",
        "GEOMETRYCOLLECTION (POINT (1 2),     GEOMETRYCOLLECTION( POINT (1 2),     LINESTRING (1 2, 3 4) ) )"
        );
  }

  public void testPolgonWithCollapsedLinePointwise() throws Exception {
    checkReducePointwise("POLYGON ((10 10, 100 100, 200 10.1, 300 10, 10 10))",
        "POLYGON ((10 10, 100 100, 200 10,   300 10, 10 10))");
	}

  public void testPolgonWithCollapsedPointPointwise() throws Exception {
    checkReducePointwise("POLYGON ((10 10, 100 100, 200 10.1, 300 100, 400 10, 10 10))",
        "POLYGON ((10 10, 100 100, 200 10,   300 100, 400 10, 10 10))");
	}

  //=======================================



  private void checkReducePointwise(String wkt, String wktExpected) {
    Geometry g  =        read(wkt);
    Geometry gExpected = read(wktExpected);
    Geometry gReduce = GeometryPrecisionReducer$.MODULE$.reducePointwise(g, pmFixed1);
    assertEqualsExactAndHasSameFactory(gExpected, gReduce);
  }


  private void assertEqualsExactAndHasSameFactory(Geometry expected, Geometry actual)
  {
    checkEqual(expected, actual);
    assertTrue("Factories are not the same", expected.getFactory() == actual.getFactory());
  }



  private void checkReduceExact(String wkt, String wktExpected) {
    checkReduceExactSameFactory(reducer, wkt, wktExpected);
  }

  private void checkReduceExactSameFactory(GeometryPrecisionReducer reducer,
      String wkt,
      String wktExpected) {
    Geometry g = read(wkt);
    Geometry expected = read(wktExpected);
    Geometry actual = reducer.reduce(g);
    assertTrue(actual.equalsExact(expected));
    assertTrue(expected.getFactory() == expected.getFactory());
  }

  private void checkReduceExact(double scaleFactor, String wkt, String wktExpected) {
    PrecisionModel pm = new PrecisionModel(scaleFactor);
    GeometryPrecisionReducer reducer = new GeometryPrecisionReducer(pm);
    checkReduceExactSameFactory(reducer, wkt, wktExpected);
  }

  private void checkReduce(double scaleFactor, String wkt, String wktExpected) {
    PrecisionModel pm = new PrecisionModel(scaleFactor);
    GeometryPrecisionReducer reducer = new GeometryPrecisionReducer(pm);
    checkReduce(reducer, wkt, wktExpected);
  }

  private void checkReduce(
      String wkt,
      String wktExpected) {
    checkReduce(reducer, wkt, wktExpected);
  }
  private void checkReduce(
      GeometryPrecisionReducer reducer,
      String wkt,
      String wktExpected) {
    Geometry g = read(wkt);
    Geometry expected = read(wktExpected);
    Geometry actual = reducer.reduce(g);
    checkEqual(expected, actual);
    assertTrue(expected.getFactory() == expected.getFactory());
  }
}
