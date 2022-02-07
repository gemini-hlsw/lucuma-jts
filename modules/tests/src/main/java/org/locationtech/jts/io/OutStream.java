/*
 * Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.io;

import java.io.IOException;

/**
 * A interface for classes providing an output stream of bytes.
 * This interface is similar to the Java <code>OutputStream</code>,
 * but with a narrower interface to make it easier to implement.
 */
public interface OutStream
{
  void write(byte[] buf, int len) throws IOException;
}
