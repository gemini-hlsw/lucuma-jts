/*
 * Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package org.locationtech.jts.io;

/**
 * Methods to read and write primitive Java datatypes from/to byte
 * sequences, allowing the byte order to be specified
 * <p>
 * Similar to the standard Java <code>ByteBuffer</code> class.
 */
public class ByteOrderValues
{
  public static final int BIG_ENDIAN = 1;
  public static final int LITTLE_ENDIAN = 2;

  public static int getInt(byte[] buf, int byteOrder)
  {
    if (byteOrder == BIG_ENDIAN) {
      return  ( (buf[0] & 0xff) << 24)
            | ( (buf[1] & 0xff) << 16)
            | ( (buf[2] & 0xff) << 8)
            | ( (buf[3] & 0xff) );
    }
    else {// LITTLE_ENDIAN
      return  ( (buf[3] & 0xff) << 24)
            | ( (buf[2] & 0xff) << 16)
            | ( (buf[1] & 0xff) << 8)
            | ( (buf[0] & 0xff) );
    }
  }

  public static void putInt(int intValue, byte[] buf, int byteOrder)
  {
    if (byteOrder == BIG_ENDIAN) {
      buf[0] = (byte)(intValue >> 24);
      buf[1] = (byte)(intValue >> 16);
      buf[2] = (byte)(intValue >> 8);
      buf[3] = (byte) intValue;
    }
    else {// LITTLE_ENDIAN
      buf[0] = (byte) intValue;
      buf[1] = (byte)(intValue >> 8);
      buf[2] = (byte)(intValue >> 16);
      buf[3] = (byte)(intValue >> 24);
    }
  }
  public static long getLong(byte[] buf, int byteOrder)
  {
    if (byteOrder == BIG_ENDIAN) {
      return
            (long) (buf[0] & 0xff) << 56
          | (long) (buf[1] & 0xff) << 48
          | (long) (buf[2] & 0xff) << 40
          | (long) (buf[3] & 0xff) << 32
          | (long) (buf[4] & 0xff) << 24
          | (long) (buf[5] & 0xff) << 16
          | (long) (buf[6] & 0xff) <<  8
          | (long) (buf[7] & 0xff);
    }
    else {// LITTLE_ENDIAN
      return
            (long) (buf[7] & 0xff) << 56
          | (long) (buf[6] & 0xff) << 48
          | (long) (buf[5] & 0xff) << 40
          | (long) (buf[4] & 0xff) << 32
          | (long) (buf[3] & 0xff) << 24
          | (long) (buf[2] & 0xff) << 16
          | (long) (buf[1] & 0xff) <<  8
          | (long) (buf[0] & 0xff);
    }
  }

  public static void putLong(long longValue, byte[] buf, int byteOrder)
  {
    if (byteOrder == BIG_ENDIAN) {
      buf[0] = (byte)(longValue >> 56);
      buf[1] = (byte)(longValue >> 48);
      buf[2] = (byte)(longValue >> 40);
      buf[3] = (byte)(longValue >> 32);
      buf[4] = (byte)(longValue >> 24);
      buf[5] = (byte)(longValue >> 16);
      buf[6] = (byte)(longValue >> 8);
      buf[7] = (byte) longValue;
    }
    else {  // LITTLE_ENDIAN
      buf[0] = (byte) longValue;
      buf[1] = (byte)(longValue >> 8);
      buf[2] = (byte)(longValue >> 16);
      buf[3] = (byte)(longValue >> 24);
      buf[4] = (byte)(longValue >> 32);
      buf[5] = (byte)(longValue >> 40);
      buf[6] = (byte)(longValue >> 48);
      buf[7] = (byte)(longValue >> 56);
    }
  }

  public static double getDouble(byte[] buf, int byteOrder)
  {
    long longVal = getLong(buf, byteOrder);
    return Double.longBitsToDouble(longVal);
  }

  public static void putDouble(double doubleValue, byte[] buf, int byteOrder)
  {
    long longVal = Double.doubleToLongBits(doubleValue);
    putLong(longVal, buf, byteOrder);
  }

}