/*
 * Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.awt;

import java.awt.geom.Point2D;

import org.locationtech.jts.geom.Coordinate;

/**
 * Copies point ordinates with no transformation.
 * 
 * @author Martin Davis
 *
 */
public class IdentityPointTransformation
implements PointTransformation
{
	public void transform(Coordinate model, Point2D view)
	{
		view.setLocation(model.getX(), model.getY());
	}
}