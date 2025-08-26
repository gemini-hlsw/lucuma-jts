// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.io

/*
 * Copyright (c) 2020 Martin Davis.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v20.html
 * and the Eclipse Distribution License is available at
 *
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */

/**
 * Constants used in the WKT (Well-Known Text) format.
 *
 * @author
 *   Martin Davis
 */
object WKTConstants {
  val GEOMETRYCOLLECTION = "GEOMETRYCOLLECTION"
  val LINEARRING         = "LINEARRING"
  val LINESTRING         = "LINESTRING"
  val MULTIPOLYGON       = "MULTIPOLYGON"
  val MULTILINESTRING    = "MULTILINESTRING"
  val MULTIPOINT         = "MULTIPOINT"
  val POINT              = "POINT"
  val POLYGON            = "POLYGON"
  val EMPTY              = "EMPTY"
  val M                  = "M"
  val Z                  = "Z"
  val ZM                 = "ZM"
}
