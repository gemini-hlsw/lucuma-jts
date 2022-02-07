/*
 * Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.awt;

import java.awt.geom.Point2D;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Point;

/**
 * Transforms a geometry {@link Coordinate} into a Java2D {@link Point},
 * possibly with a mathematical transformation of the ordinate values.
 * Transformation from a model coordinate system to a view coordinate system 
 * can be efficiently performed by supplying an appropriate transformation.
 * 
 * @author Martin Davis
 */
public interface PointTransformation {
	/**
	 * Transforms a {@link Coordinate} into a Java2D {@link Point}.
	 * 
	 * @param src the source Coordinate 
	 * @param dest the destination Point
	 */
  public void transform(Coordinate src, Point2D dest);
}