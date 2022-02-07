/*
 * Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.io;

import java.io.IOException;
import java.io.OutputStream;

/**
 * An adapter to allow an {@link OutputStream} to be used as an {@link OutStream}
 */
public class OutputStreamOutStream
	implements OutStream
{
  private OutputStream os;

  public OutputStreamOutStream(OutputStream os)
  {
    this.os = os;
  }
  public void write(byte[] buf, int len) throws IOException
  {
    os.write(buf, 0, len);
  }
}
