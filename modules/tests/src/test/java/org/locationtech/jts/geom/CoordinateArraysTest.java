/*
 * Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.geom;


import junit.framework.TestCase;
import junit.textui.TestRunner;
import org.locationtech.jts.geom.impl.CoordinateArraySequenceFactory;
import org.locationtech.jts.geom.impl.PackedCoordinateSequenceFactory;

/**
 * Unit tests for {@link CoordinateArrays}
 *
 * @author Martin Davis
 * @version 1.7
 */
public class CoordinateArraysTest extends TestCase {

  public static void main(String args[]) {
    TestRunner.run(CoordinateArraysTest.class);
  }

  private static Coordinate[] COORDS_1 = new Coordinate[] { new Coordinate(1, 1), new Coordinate(2, 2), new Coordinate(3, 3) };
  private static Coordinate[] COORDS_EMPTY = new Coordinate[0];

  public CoordinateArraysTest(String name) { super(name); }

  public void testPtNotInList1()
  {
    assertTrue(CoordinateArrays.ptNotInList(
        new Coordinate[] { new Coordinate(1, 1), new Coordinate(2, 2), new Coordinate(3, 3) },
        new Coordinate[] { new Coordinate(1, 1), new Coordinate(1, 2), new Coordinate(1, 3) }
        ).equals2D(new Coordinate(2, 2))
        );
  }
  public void testPtNotInList2()
  {
    assertTrue(CoordinateArrays.ptNotInList(
        new Coordinate[] { new Coordinate(1, 1), new Coordinate(2, 2), new Coordinate(3, 3) },
        new Coordinate[] { new Coordinate(1, 1), new Coordinate(2, 2), new Coordinate(3, 3) }
        ) == null
        );
  }
  public void testEnvelope1()
  {
    assertEquals( CoordinateArrays.envelope(COORDS_1),  new Envelope(1, 3, 1, 3) );
  }
  public void testEnvelopeEmpty()
  {
    assertEquals( CoordinateArrays.envelope(COORDS_EMPTY), new Envelope() );
  }
  public void testIntersection_envelope1()
  {
    assertTrue(CoordinateArrays.equals(
        CoordinateArrays.intersection(COORDS_1, new Envelope(1, 2, 1, 2)),
        new Coordinate[] { new Coordinate(1, 1), new Coordinate(2, 2) }
        ));
  }
  public void testIntersection_envelopeDisjoint()
  {
    assertTrue(CoordinateArrays.equals(
        CoordinateArrays.intersection(COORDS_1, new Envelope(10, 20, 10, 20)),  COORDS_EMPTY )
        );
  }
  public void testIntersection_empty_envelope()
  {
    assertTrue(CoordinateArrays.equals(
        CoordinateArrays.intersection(COORDS_EMPTY, new Envelope(1, 2, 1, 2)), COORDS_EMPTY )
        );
  }
  public void testIntersection_coords_emptyEnvelope()
  {
    assertTrue(CoordinateArrays.equals(
        CoordinateArrays.intersection(COORDS_1, new Envelope()), COORDS_EMPTY )
        );
  }

  public void testScrollRing() {
    // arrange
    Coordinate[] sequence = createCircle(new Coordinate(10, 10), 9d);
    Coordinate[] scrolled = createCircle(new Coordinate(10, 10), 9d);

    // act
    CoordinateArrays.scroll(scrolled, 12);

    // assert
    int io = 12;
    for (int is = 0; is < scrolled.length - 1; is++) {
      checkCoordinateAt(sequence, io, scrolled, is);
      io++;
      io%=scrolled.length-1;
    }
    checkCoordinateAt(scrolled, 0, scrolled, scrolled.length-1);  }

  public void testScroll() {
    // arrange
    Coordinate[] sequence = createCircularString(new Coordinate(20, 20), 7d,
      0.1, 22);
    Coordinate[] scrolled = createCircularString(new Coordinate(20, 20), 7d,
      0.1, 22);;

    // act
    CoordinateArrays.scroll(scrolled, 12);

    // assert
    int io = 12;
    for (int is = 0; is < scrolled.length - 1; is++) {
      checkCoordinateAt(sequence, io, scrolled, is);
      io++;
      io%=scrolled.length;
    }
  }

  public void testEnforceConsistency(){
    Coordinate array[] = new Coordinate[]{
        new Coordinate(1.0, 1.0, 0.0),
        new CoordinateXYM(2.0,2.0,1.0)
    };
    Coordinate array2[] = new Coordinate[]{
            new CoordinateXY(1.0, 1.0),
            new CoordinateXY(2.0,2.0)
    };
    // process into array with dimension 4 and measures 1
    CoordinateArrays.enforceConsistency(array);
    assertEquals( 3, CoordinateArrays.dimension(array));
    assertEquals( 1, CoordinateArrays.measures(array));

    CoordinateArrays.enforceConsistency(array2);

    Coordinate fixed[] = CoordinateArrays.enforceConsistency(array2,2,0);
    assertSame( fixed, array2); // no processing required

    fixed = CoordinateArrays.enforceConsistency(array,3,0);
    assertTrue( fixed != array); // copied into new array
    assertTrue( array[0] != fixed[0] ); // processing needed to CoordinateXYZM
    assertTrue( array[1] != fixed[1] ); // processing needed to CoordinateXYZM
  }

  private static void checkCoordinateAt(Coordinate[] seq1, int pos1,
                                        Coordinate[] seq2, int pos2) {
    Coordinate c1 = seq1[pos1], c2 = seq2[pos2];

    assertEquals("unexpected x-ordinate at pos " + pos2, c1.getX(), c2.getX());
    assertEquals("unexpected y-ordinate at pos " + pos2, c1.getY(), c2.getY());
  }

  private static Coordinate[] createCircle(Coordinate center, double radius) {
    // Get a complete circular string
    Coordinate[] res = createCircularString(center, radius, 0d,49);

    // ensure it is closed
    res[48] = res[0].copy();

    return res;
  }

  private static Coordinate[] createCircularString(Coordinate center, double radius, double startAngle,
                                                   int numPoints) {
    final int numSegmentsCircle = 48;
    final double angleCircle = 2 * Math.PI;
    final double angleStep = angleCircle / numSegmentsCircle;

    Coordinate[] sequence = new Coordinate[numPoints];
    PrecisionModel pm = new PrecisionModel(1000);
    double angle = startAngle;
    for (int i = 0; i < numPoints; i++)
    {
      double dx = Math.cos(angle) * radius;
      double dy = Math.sin(angle) * radius;
      sequence[i] = new CoordinateXY(pm.makePrecise(center.x() +dx), pm.makePrecise(center.y() +dy));

      angle += angleStep;
      angle %= angleCircle;
    }

    return sequence;
  }

}
