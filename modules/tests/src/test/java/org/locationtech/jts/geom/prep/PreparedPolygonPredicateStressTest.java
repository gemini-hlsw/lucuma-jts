/*
 * Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

// /*
//  * Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
//  * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
//  */
//
// package org.locationtech.jts.geom.prep;
//
// import org.locationtech.jts.geom.Geometry;
//
// import junit.framework.TestCase;
// import junit.textui.TestRunner;
//
//
// /**
//  * Stress tests {@link PreparedPolygon} for
//  * correctness of
//  * {@link PreparedPolygon#contains(Geometry)}
//  * and {@link PreparedPolygon#intersects(Geometry)}
//  * operations.
//  *
//  * @author Owner
//  *
//  */
// public class PreparedPolygonPredicateStressTest extends TestCase
// {
//
//   public static void main(String args[]) {
//     TestRunner.run(PreparedPolygonPredicateStressTest.class);
//   }
//
//   boolean testFailed = false;
//
//   public PreparedPolygonPredicateStressTest(String name) {
//     super(name);
//   }
//
//   public void test()
//   {
//   	PredicateStressTester tester = new PredicateStressTester();
//   	tester.run(1000);
//   }
//
//   class PredicateStressTester
//   extends StressTestHarness
//   {
//   	public boolean checkResult(Geometry target, Geometry test) {
//   		if (! checkIntersects(target, test)) return false;
//   		if (! checkContains(target, test)) return false;
//   		return true;
//   	}
//   }
//
//   public boolean checkContains(Geometry target, Geometry test)
//   {
// 	boolean expectedResult = target.contains(test);
//
//     PreparedGeometryFactory pgFact = new PreparedGeometryFactory();
//     PreparedGeometry prepGeom = pgFact.create(target);
//
// 	boolean prepResult = prepGeom.contains(test);
//
// 	if (prepResult != expectedResult) {
// 		return false;
// 	}
// 	return true;
//   }
//
//   public boolean checkIntersects(Geometry target, Geometry test)
//   {
// 	boolean expectedResult = target.intersects(test);
//
// 	PreparedGeometryFactory pgFact = new PreparedGeometryFactory();
// 	PreparedGeometry prepGeom = pgFact.create(target);
//
// 	boolean prepResult = prepGeom.intersects(test);
//
// 	if (prepResult != expectedResult) {
// 		return false;
// 	}
// 	return true;
//   }
//
// }
