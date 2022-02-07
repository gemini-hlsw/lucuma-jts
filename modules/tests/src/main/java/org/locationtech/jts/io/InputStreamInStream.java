/*
 * Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.io;

import java.io.IOException;
import java.io.InputStream;

/**
 * An adapter to allow an {@link InputStream} to be used as an {@link InStream}
 */
public class InputStreamInStream
	implements InStream
{
  private InputStream is;

  public InputStreamInStream(InputStream is)
  {
    this.is = is;
  }

  public void read(byte[] buf) throws IOException
  {
    is.read(buf);
  }
}
