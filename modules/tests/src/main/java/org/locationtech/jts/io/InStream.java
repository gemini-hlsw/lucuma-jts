/*
 * Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.io;

import java.io.IOException;

/**
 * A interface for classes providing an input stream of bytes.
 * This interface is similar to the Java <code>InputStream</code>,
 * but with a narrower interface to make it easier to implement.
 *
 */
public interface InStream
{
  /**
   * Reads <code>buf.length</code> bytes from the input stream
   * and stores them in the supplied buffer.
   *
   * @param buf the buffer to receive the bytes
   *
   * @throws IOException if an I/O error occurs
   */
  void read(byte[] buf) throws IOException;
}
