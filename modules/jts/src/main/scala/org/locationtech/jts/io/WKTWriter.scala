// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.locationtech.jts.io

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.CoordinateSequence
import org.locationtech.jts.geom.CoordinateSequenceFilter
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryCollection
import org.locationtech.jts.geom.LineString
import org.locationtech.jts.geom.LinearRing
import org.locationtech.jts.geom.MultiLineString
import org.locationtech.jts.geom.MultiPoint
import org.locationtech.jts.geom.MultiPolygon
import org.locationtech.jts.geom.Point
import org.locationtech.jts.geom.Polygon
import org.locationtech.jts.geom.PrecisionModel
import org.locationtech.jts.util.Assert

import java.io.IOException
import java.io.StringWriter
import java.io.Writer
import scala.annotation.nowarn

/*
 * Copyright (c) 2016 Vivid Solutions.
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
 * Writes the Well-Known Text representation of a {@link Geometry}. The Well-Known Text format is
 * defined in the OGC <a href="http://www.opengis.org/techno/specs.htm"> <i>Simple Features
 * Specification for SQL</i></a>. See {@link WKTReader} for a formal specification of the format
 * syntax. <p> The <code>WKTWriter</code> outputs coordinates rounded to the precision model. Only
 * the maximum number of decimal places necessary to represent the ordinates to the required
 * precision will be output. <p> The SFS WKT spec does not define a special tag for {@link
 * LinearRing}s. Under the spec, rings are output as <code>LINESTRING</code>s. In order to allow
 * precisely specifying constructed geometries, JTS also supports a non-standard
 * <code>LINEARRING</code> tag which is used to output LinearRings.
 *
 * @version 1.7
 * @see
 *   WKTReader
 */
object WKTWriter {

  /**
   * Generates the WKT for a <tt>POINT</tt> specified by a {@link Coordinate}.
   *
   * @param p0
   *   the point coordinate
   * @return
   *   the WKT
   */
  def toPoint(p0: Coordinate): String = WKTConstants.POINT + " ( " + format(p0) + " )"

  /**
   * Generates the WKT for a <tt>LINESTRING</tt> specified by a {@link CoordinateSequence}.
   *
   * @param seq
   *   the sequence to write
   * @return
   *   the WKT string
   */
  def toLineString(seq: CoordinateSequence): String = {
    val buf = new StringBuilder
    buf.append(WKTConstants.LINESTRING)
    buf.append(" ")
    if (seq.size == 0) buf.append(WKTConstants.EMPTY)
    else {
      buf.append("(")
      for (i <- 0 until seq.size) {
        if (i > 0) buf.append(", ")
        buf.append(format(seq.getX(i), seq.getY(i)))
      }
      buf.append(")")
    }
    buf.toString
  }

  /**
   * Generates the WKT for a <tt>LINESTRING</tt> specified by a {@link CoordinateSequence}.
   *
   * @param coord
   *   the sequence to write
   * @return
   *   the WKT string
   */
  def toLineString(coord: Array[Coordinate]): String = {
    val buf = new StringBuilder
    buf.append(WKTConstants.LINESTRING)
    buf.append(" ")
    if (coord.length == 0) buf.append(WKTConstants.EMPTY)
    else {
      buf.append("(")
      for (i <- 0 until coord.length) {
        if (i > 0) buf.append(", ")
        buf.append(format(coord(i)))
      }
      buf.append(")")
    }
    buf.toString
  }

  /**
   * Generates the WKT for a <tt>LINESTRING</tt> specified by two {@link Coordinate}s.
   *
   * @param p0
   *   the first coordinate
   * @param p1
   *   the second coordinate
   * @return
   *   the WKT
   */
  def toLineString(p0: Coordinate, p1: Coordinate): String =
    WKTConstants.LINESTRING + " ( " + format(p0) + ", " + format(p1) + " )"

  def format(p: Coordinate): String = format(p.x, p.y)

  private def format(x: Double, y: Double) =
    OrdinateFormat.DEFAULT.format(x) + " " + OrdinateFormat.DEFAULT.format(y)

  private val INDENT           = 2
  private val OUTPUT_DIMENSION = 2

  /**
   * Creates the <code>DecimalFormat</code> used to write <code>double</code>s with a sufficient
   * number of decimal places.
   *
   * @param precisionModel
   *   the <code>PrecisionModel</code> used to determine the number of decimal places to write.
   * @return
   *   a <code>DecimalFormat</code> that write <code>double</code> s without scientific notation.
   */
  private def createFormatter(precisionModel: PrecisionModel) =
    OrdinateFormat.create(precisionModel.getMaximumSignificantDigits)

  /**
   * Returns a <code>String</code> of repeated characters.
   *
   * @param ch
   *   the character to repeat
   * @param count
   *   the number of times to repeat the character
   * @return
   *   a <code>String</code> of characters
   */
  private def stringOfChar(ch: Char, count: Int) = {
    val buf = new StringBuilder(count)
    for (_ <- 0 until count)
      buf.append(ch)
    buf.toString
  }

  /**
   * Converts a <code>double</code> to a <code>String</code>, not in scientific notation.
   *
   * @param d
   *   the <code>double</code> to convert
   * @return
   *   the <code>double</code> as a <code>String</code>, not in scientific notation
   */
  private def writeNumber(d: Double, formatter: OrdinateFormat) = formatter.format(d)
}

class WKTWriter(val outputDimension: Int) {

  /**
   * Creates a writer that writes {@link Geometry}s with the given output dimension (2 to 4). The
   * output follows the following rules: <ul> <li>If the specified <b>output dimension is 3</b> and
   * the <b>z is measure flag is set to true</b>, the Z value of coordinates will be written if it
   * is present (i.e. if it is not <code>Double.NaN</code>)</li> <li>If the specified <b>output
   * dimension is 3</b> and the <b>z is measure flag is set to false</b>, the Measure value of
   * coordinates will be written if it is present (i.e. if it is not <code>Double.NaN</code>)</li>
   * <li>If the specified <b>output dimension is 4</b>, the Z value of coordinates will be written
   * even if it is not present when the Measure value is present.The Measrue value of coordinates
   * will be written if it is present (i.e. if it is not <code>Double.NaN</code>)</li> </ul>
   *
   * @param outputDimension
   *   the coordinate dimension to output (2 to 4)
   */
  setTab(WKTWriter.INDENT)
  private var outputOrdinates: Ordinate.ValueSet = null
  if (outputDimension < 2 || outputDimension > 4) {
    throw new IllegalArgumentException("Invalid output dimension (must be 2 to 4)")
  }
  this.outputOrdinates = Ordinate.ValueSet(Ordinate.X, Ordinate.Y)
  if (outputDimension > 2) {
    outputOrdinates = outputOrdinates + Ordinate.Z
  }
  if (outputDimension > 3) {
    outputOrdinates = outputOrdinates + Ordinate.M
  }

  /**
   * A filter implementation to test if a coordinate sequence actually has meaningful values for an
   * ordinate bit-pattern
   */
  private class CheckOrdinatesFilter(val checkOrdinateFlags: Ordinate.ValueSet)

  /**
   * Creates an instance of this class
   *
   * @param checkOrdinateFlags
   *   the index for the ordinates to test.
   */
      extends CoordinateSequenceFilter {
    final private var outputOrdinates = Ordinate.ValueSet(Ordinate.X, Ordinate.Y)

    /** @see org.locationtech.jts.geom.CoordinateSequenceFilter#isGeometryChanged */
    override def filter(seq: CoordinateSequence, i: Int): Unit = {
      if (checkOrdinateFlags.contains(Ordinate.Z) && !outputOrdinates.contains(Ordinate.Z))
        if (!java.lang.Double.isNaN(seq.getZ(i))) outputOrdinates = outputOrdinates + Ordinate.Z
      if (checkOrdinateFlags.contains(Ordinate.M) && !outputOrdinates.contains(Ordinate.M))
        if (!java.lang.Double.isNaN(seq.getM(i))) outputOrdinates = outputOrdinates + Ordinate.M
    }

    override def isGeometryChanged = false

    /** @see org.locationtech.jts.geom.CoordinateSequenceFilter#isDone */
    override def isDone: Boolean = outputOrdinates == checkOrdinateFlags

    /**
     * Gets the evaluated ordinate bit-pattern
     *
     * @return
     *   A bit-pattern of ordinates with valid values masked by {@link # checkOrdinateFlags}.
     */
    private[io] def getOutputOrdinates = outputOrdinates
  }

  @nowarn
  private var precisionModel: PrecisionModel = null
  private var ordinateFormat: OrdinateFormat = null
  private var isFormatted: Boolean           = false
  private var coordsPerLine: Int             = -1
  private var indentTabStr: String           = null

  /**
   * Creates a new WKTWriter with default settings
   */
  def this() =
    this(WKTWriter.OUTPUT_DIMENSION)

  /**
   * Sets whether the output will be formatted.
   *
   * @param isFormatted
   *   true if the output is to be formatted
   */
  def setFormatted(isFormatted: Boolean): Unit =
    this.isFormatted = isFormatted

  /**
   * Sets the maximum number of coordinates per line written in formatted output. If the provided
   * coordinate number is &lt;= 0, coordinates will be written all on one line.
   *
   * @param coordsPerLine
   *   the number of coordinates per line to output.
   */
  def setMaxCoordinatesPerLine(coordsPerLine: Int): Unit =
    this.coordsPerLine = coordsPerLine

  /**
   * Sets the tab size to use for indenting.
   *
   * @param size
   *   the number of spaces to use as the tab string
   * @throws IllegalArgumentException
   *   if the size is non-positive
   */
  def setTab(size: Int): Unit = {
    if (size <= 0) {
      throw new IllegalArgumentException("Tab count must be positive")
    }
    this.indentTabStr = WKTWriter.stringOfChar(' ', size)
  }

  /**
   * Sets the {@link Ordinate} that are to be written. Possible members are: <ul> <li>{@link
   * Ordinate# X}</li> <li>{@link Ordinate# Y}</li> <li>{@link Ordinate# Z}</li> <li>{@link
   * Ordinate# M}</li> </ul> Values of {@link Ordinate# X} and {@link Ordinate# Y} are always
   * assumed and not particularly checked for.
   *
   * @param outputOrdinates
   *   A set of {@link Ordinate} values
   */
  def setOutputOrdinates(outputOrdinates: Ordinate.ValueSet): Unit = {
    this.outputOrdinates = this.outputOrdinates - Ordinate.Z
    this.outputOrdinates = this.outputOrdinates - Ordinate.M
    if (this.outputDimension == 3) {
      if (outputOrdinates.contains(Ordinate.Z)) {
        this.outputOrdinates = this.outputOrdinates + Ordinate.Z
      } else {
        if (outputOrdinates.contains(Ordinate.M)) {
          this.outputOrdinates = this.outputOrdinates + Ordinate.M
        }
      }
    }
    if (this.outputDimension == 4) {
      if (outputOrdinates.contains(Ordinate.Z)) {
        this.outputOrdinates = this.outputOrdinates + Ordinate.Z
      }
      if (outputOrdinates.contains(Ordinate.M)) {
        this.outputOrdinates = this.outputOrdinates + Ordinate.M
      }
    }
  }

  /**
   * Gets a bit-pattern defining which ordinates should be
   *
   * @return
   *   an ordinate bit-pattern
   * @see
   *   #setOutputOrdinates(EnumSet)
   */
  def getOutputOrdinates: Ordinate.ValueSet =
    return this.outputOrdinates

  /**
   * Sets a {@link PrecisionModel} that should be used on the ordinates written. <p>If none/{@code
   * null} is assigned, the precision model of the {@link Geometry# getFactory ( )} is used.</p>
   * <p>Note: The precision model is applied to all ordinate values, not just x and y.</p>
   *
   * @param precisionModel
   *   the flag indicating if {@link Coordinate# z}/{} is actually a measure value.
   */
  def setPrecisionModel(precisionModel: PrecisionModel): Unit = {
    this.precisionModel = precisionModel
    this.ordinateFormat = OrdinateFormat.create(precisionModel.getMaximumSignificantDigits)
  }

  /**
   * Converts a <code>Geometry</code> to its Well-known Text representation.
   *
   * @param geometry
   *   a <code>Geometry</code> to process
   * @return
   *   a &lt;Geometry Tagged Text&gt; string (see the OpenGIS Simple Features Specification)
   */
  def write(geometry: Geometry): String = {
    val sw: Writer = new StringWriter
    try writeFormatted(geometry, false, sw)
    catch {
      case _: IOException =>
        Assert.shouldNeverReachHere()
    }
    return sw.toString
  }

  /**
   * Converts a <code>Geometry</code> to its Well-known Text representation.
   *
   * @param geometry
   *   a <code>Geometry</code> to process
   */
  @throws[IOException]
  def write(geometry: Geometry, writer: Writer): Unit = // write the geometry
    writeFormatted(geometry, isFormatted, writer)

  /**
   * Same as <code>write</code>, but with newlines and spaces to make the well-known text more
   * readable.
   *
   * @param geometry
   *   a <code>Geometry</code> to process
   * @return
   *   a &lt;Geometry Tagged Text&gt; string (see the OpenGIS Simple Features Specification), with
   *   newlines and spaces
   */
  def writeFormatted(geometry: Geometry): String = {
    val sw: Writer = new StringWriter
    try writeFormatted(geometry, true, sw)
    catch {
      case _: IOException =>
        Assert.shouldNeverReachHere()
    }
    return sw.toString
  }

  /**
   * Same as <code>write</code>, but with newlines and spaces to make the well-known text more
   * readable.
   *
   * @param geometry
   *   a <code>Geometry</code> to process
   */
  @throws[IOException]
  def writeFormatted(geometry: Geometry, writer: Writer): Unit =
    writeFormatted(geometry, true, writer)

  @throws[IOException]
  private def writeFormatted(geometry: Geometry, useFormatting: Boolean, writer: Writer): Unit = {
    val formatter: OrdinateFormat = getFormatter(geometry)
    // append the WKT
    appendGeometryTaggedText(geometry, useFormatting, writer, formatter)
  }

  private def getFormatter(geometry: Geometry): OrdinateFormat = { // if present use the cached formatter
    if (ordinateFormat != null) {
      return ordinateFormat
    }
    // no precision model was specified, so use the geometry's
    val pm: PrecisionModel        = geometry.getPrecisionModel
    val formatter: OrdinateFormat = WKTWriter.createFormatter(pm)
    return formatter
  }

  /**
   * Converts a <code>Geometry</code> to &lt;Geometry Tagged Text&gt; format, then appends it to the
   * writer.
   *
   * @param geometry
   *   the <code>Geometry</code> to process
   * @param useFormatting
   *   flag indicating that the output should be formatted
   * @param writer
   *   the output writer to append to
   * @param formatter
   *   the <code>DecimalFormatter</code> to use to convert from a precise coordinate to an external
   *   coordinate
   */
  @throws[IOException]
  private def appendGeometryTaggedText(
    geometry:      Geometry,
    useFormatting: Boolean,
    writer:        Writer,
    formatter:     OrdinateFormat
  ): Unit = { // evaluate the ordinates actually present in the geometry
    val cof: CheckOrdinatesFilter =
      new CheckOrdinatesFilter(this.outputOrdinates)
    geometry.applyF(cof)
    // Append the WKT
    appendGeometryTaggedText(geometry, cof.getOutputOrdinates, useFormatting, 0, writer, formatter)
  }

  /**
   * Converts a <code>Geometry</code> to &lt;Geometry Tagged Text&gt; format, then appends it to the
   * writer.
   *
   * @param geometry
   *   the <code>Geometry</code> to process
   * @param useFormatting
   *   flag indicating that the output should be formatted
   * @param level
   *   the indentation level
   * @param writer
   *   the output writer to append to
   * @param formatter
   *   the <code>DecimalFormatter</code> to use to convert from a precise coordinate to an external
   *   coordinate
   */
  @throws[IOException]
  private def appendGeometryTaggedText(
    geometry:        Geometry,
    outputOrdinates: Ordinate.ValueSet,
    useFormatting:   Boolean,
    level:           Int,
    writer:          Writer,
    formatter:       OrdinateFormat
  ): Unit = {
    indent(useFormatting, level, writer)
    if (geometry.isInstanceOf[Point]) {
      appendPointTaggedText(geometry.asInstanceOf[Point],
                            outputOrdinates,
                            useFormatting,
                            level,
                            writer,
                            formatter
      )
    } else {
      if (geometry.isInstanceOf[LinearRing]) {
        appendLinearRingTaggedText(geometry.asInstanceOf[LinearRing],
                                   outputOrdinates,
                                   useFormatting,
                                   level,
                                   writer,
                                   formatter
        )
      } else {
        if (geometry.isInstanceOf[LineString]) {
          appendLineStringTaggedText(geometry.asInstanceOf[LineString],
                                     outputOrdinates,
                                     useFormatting,
                                     level,
                                     writer,
                                     formatter
          )
        } else {
          if (geometry.isInstanceOf[Polygon]) {
            appendPolygonTaggedText(geometry.asInstanceOf[Polygon],
                                    outputOrdinates,
                                    useFormatting,
                                    level,
                                    writer,
                                    formatter
            )
          } else {
            if (geometry.isInstanceOf[MultiPoint]) {
              appendMultiPointTaggedText(geometry.asInstanceOf[MultiPoint],
                                         outputOrdinates,
                                         useFormatting,
                                         level,
                                         writer,
                                         formatter
              )
            } else {
              if (geometry.isInstanceOf[MultiLineString]) {
                appendMultiLineStringTaggedText(geometry.asInstanceOf[MultiLineString],
                                                outputOrdinates,
                                                useFormatting,
                                                level,
                                                writer,
                                                formatter
                )
              } else {
                if (geometry.isInstanceOf[MultiPolygon]) {
                  appendMultiPolygonTaggedText(geometry.asInstanceOf[MultiPolygon],
                                               outputOrdinates,
                                               useFormatting,
                                               level,
                                               writer,
                                               formatter
                  )
                } else {
                  if (geometry.isInstanceOf[GeometryCollection]) {
                    appendGeometryCollectionTaggedText(geometry.asInstanceOf[GeometryCollection],
                                                       outputOrdinates,
                                                       useFormatting,
                                                       level,
                                                       writer,
                                                       formatter
                    )
                  } else {
                    Assert.shouldNeverReachHere(
                      "Unsupported Geometry implementation:" + geometry.getClass
                    )
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  /**
   * Converts a <code>Coordinate</code> to &lt;Point Tagged Text&gt; format, then appends it to the
   * writer.
   *
   * @param point
   *   the <code>Point</code> to process
   * @param useFormatting
   *   flag indicating that the output should be formatted
   * @param level
   *   the indentation level
   * @param writer
   *   the output writer to append to
   * @param formatter
   *   the formatter to use when writing numbers
   */
  @throws[IOException]
  private def appendPointTaggedText(
    point:           Point,
    outputOrdinates: Ordinate.ValueSet,
    useFormatting:   Boolean,
    level:           Int,
    writer:          Writer,
    formatter:       OrdinateFormat
  ): Unit = {
    writer.write(WKTConstants.POINT)
    writer.write(" ")
    appendOrdinateText(outputOrdinates, writer)
    appendSequenceText(point.getCoordinateSequence,
                       outputOrdinates,
                       useFormatting,
                       level,
                       false,
                       writer,
                       formatter
    )
  }

  /**
   * Converts a <code>LineString</code> to &lt;LineString Tagged Text&gt; format, then appends it to
   * the writer.
   *
   * @param lineString
   *   the <code>LineString</code> to process
   * @param useFormatting
   *   flag indicating that the output should be formatted
   * @param level
   *   the indentation level
   * @param writer
   *   the output writer to append to
   * @param formatter
   *   the <code>DecimalFormatter</code> to use to convert from a precise coordinate to an external
   *   coordinate
   */
  @throws[IOException]
  private def appendLineStringTaggedText(
    lineString:      LineString,
    outputOrdinates: Ordinate.ValueSet,
    useFormatting:   Boolean,
    level:           Int,
    writer:          Writer,
    formatter:       OrdinateFormat
  ): Unit = {
    writer.write(WKTConstants.LINESTRING)
    writer.write(" ")
    appendOrdinateText(outputOrdinates, writer)
    appendSequenceText(lineString.getCoordinateSequence,
                       outputOrdinates,
                       useFormatting,
                       level,
                       false,
                       writer,
                       formatter
    )
  }

  /**
   * Converts a <code>LinearRing</code> to &lt;LinearRing Tagged Text&gt; format, then appends it to
   * the writer.
   *
   * @param linearRing
   *   the <code>LinearRing</code> to process
   * @param useFormatting
   *   flag indicating that the output should be formatted
   * @param level
   *   the indentation level
   * @param writer
   *   the output writer to append to
   * @param formatter
   *   the <code>DecimalFormatter</code> to use to convert from a precise coordinate to an external
   *   coordinate
   */
  @throws[IOException]
  private def appendLinearRingTaggedText(
    linearRing:      LinearRing,
    outputOrdinates: Ordinate.ValueSet,
    useFormatting:   Boolean,
    level:           Int,
    writer:          Writer,
    formatter:       OrdinateFormat
  ): Unit = {
    writer.write(WKTConstants.LINEARRING)
    writer.write(" ")
    appendOrdinateText(outputOrdinates, writer)
    appendSequenceText(linearRing.getCoordinateSequence,
                       outputOrdinates,
                       useFormatting,
                       level,
                       false,
                       writer,
                       formatter
    )
  }

  /**
   * Converts a <code>Polygon</code> to &lt;Polygon Tagged Text&gt; format, then appends it to the
   * writer.
   *
   * @param polygon
   *   the <code>Polygon</code> to process
   * @param useFormatting
   *   flag indicating that the output should be formatted
   * @param level
   *   the indentation level
   * @param writer
   *   the output writer to append to
   * @param formatter
   *   the <code>DecimalFormatter</code> to use to convert from a precise coordinate to an external
   *   coordinate
   */
  @throws[IOException]
  private def appendPolygonTaggedText(
    polygon:         Polygon,
    outputOrdinates: Ordinate.ValueSet,
    useFormatting:   Boolean,
    level:           Int,
    writer:          Writer,
    formatter:       OrdinateFormat
  ): Unit = {
    writer.write(WKTConstants.POLYGON)
    writer.write(" ")
    appendOrdinateText(outputOrdinates, writer)
    appendPolygonText(polygon, outputOrdinates, useFormatting, level, false, writer, formatter)
  }

  /**
   * Converts a <code>MultiPoint</code> to &lt;MultiPoint Tagged Text&gt; format, then appends it to
   * the writer.
   *
   * @param multipoint
   *   the <code>MultiPoint</code> to process
   * @param useFormatting
   *   flag indicating that the output should be formatted
   * @param level
   *   the indentation level
   * @param writer
   *   the output writer to append to
   * @param formatter
   *   the <code>DecimalFormatter</code> to use to convert from a precise coordinate to an external
   *   coordinate
   */
  @throws[IOException]
  private def appendMultiPointTaggedText(
    multipoint:      MultiPoint,
    outputOrdinates: Ordinate.ValueSet,
    useFormatting:   Boolean,
    level:           Int,
    writer:          Writer,
    formatter:       OrdinateFormat
  ): Unit = {
    writer.write(WKTConstants.MULTIPOINT)
    writer.write(" ")
    appendOrdinateText(outputOrdinates, writer)
    appendMultiPointText(multipoint, outputOrdinates, useFormatting, level, writer, formatter)
  }

  /**
   * Converts a <code>MultiLineString</code> to &lt;MultiLineString Tagged Text&gt; format, then
   * appends it to the writer.
   *
   * @param multiLineString
   *   the <code>MultiLineString</code> to process
   * @param useFormatting
   *   flag indicating that the output should be formatted
   * @param level
   *   the indentation level
   * @param writer
   *   the output writer to append to
   * @param formatter
   *   the <code>DecimalFormatter</code> to use to convert from a precise coordinate to an external
   *   coordinate
   */
  @throws[IOException]
  private def appendMultiLineStringTaggedText(
    multiLineString: MultiLineString,
    outputOrdinates: Ordinate.ValueSet,
    useFormatting:   Boolean,
    level:           Int,
    writer:          Writer,
    formatter:       OrdinateFormat
  ): Unit = {
    writer.write(WKTConstants.MULTILINESTRING)
    writer.write(" ")
    appendOrdinateText(outputOrdinates, writer)
    appendMultiLineStringText(multiLineString,
                              outputOrdinates,
                              useFormatting,
                              level, /*false, */ writer,
                              formatter
    )
  }

  /**
   * Converts a <code>MultiPolygon</code> to &lt;MultiPolygon Tagged Text&gt; format, then appends
   * it to the writer.
   *
   * @param multiPolygon
   *   the <code>MultiPolygon</code> to process
   * @param useFormatting
   *   flag indicating that the output should be formatted
   * @param level
   *   the indentation level
   * @param writer
   *   the output writer to append to
   * @param formatter
   *   the <code>DecimalFormatter</code> to use to convert from a precise coordinate to an external
   *   coordinate
   */
  @throws[IOException]
  private def appendMultiPolygonTaggedText(
    multiPolygon:    MultiPolygon,
    outputOrdinates: Ordinate.ValueSet,
    useFormatting:   Boolean,
    level:           Int,
    writer:          Writer,
    formatter:       OrdinateFormat
  ): Unit = {
    writer.write(WKTConstants.MULTIPOLYGON)
    writer.write(" ")
    appendOrdinateText(outputOrdinates, writer)
    appendMultiPolygonText(multiPolygon, outputOrdinates, useFormatting, level, writer, formatter)
  }

  /**
   * Converts a <code>GeometryCollection</code> to &lt;GeometryCollection Tagged Text&gt; format,
   * then appends it to the writer.
   *
   * @param geometryCollection
   *   the <code>GeometryCollection</code> to process
   * @param useFormatting
   *   flag indicating that the output should be formatted
   * @param level
   *   the indentation level
   * @param writer
   *   the output writer to append to
   * @param formatter
   *   the <code>DecimalFormatter</code> to use to convert from a precise coordinate to an external
   *   coordinate
   */
  @throws[IOException]
  private def appendGeometryCollectionTaggedText(
    geometryCollection: GeometryCollection,
    outputOrdinates:    Ordinate.ValueSet,
    useFormatting:      Boolean,
    level:              Int,
    writer:             Writer,
    formatter:          OrdinateFormat
  ): Unit = {
    writer.write(WKTConstants.GEOMETRYCOLLECTION)
    writer.write(" ")
    appendOrdinateText(outputOrdinates, writer)
    appendGeometryCollectionText(geometryCollection,
                                 outputOrdinates,
                                 useFormatting,
                                 level,
                                 writer,
                                 formatter
    )
  }

  /**
   * Appends the i'th coordinate from the sequence to the writer <p>If the {@code seq} has
   * coordinates that are {@link double.NAN}, these are not written, even though {@link #
   * outputDimension} suggests this.
   *
   * @param seq
   *   the <code>CoordinateSequence</code> to process
   * @param i
   *   the index of the coordinate to write
   * @param writer
   *   the output writer to append to
   * @param formatter
   *   the formatter to use for writing ordinate values
   */
  @throws[IOException]
  private def appendCoordinate(
    seq:             CoordinateSequence,
    outputOrdinates: Ordinate.ValueSet,
    i:               Int,
    writer:          Writer,
    formatter:       OrdinateFormat
  ): Unit = {
    writer.write(
      WKTWriter.writeNumber(seq.getX(i), formatter) + " " + WKTWriter.writeNumber(seq.getY(i),
                                                                                  formatter
      )
    )
    if (outputOrdinates.contains(Ordinate.Z)) {
      writer.write(" ")
      writer.write(WKTWriter.writeNumber(seq.getZ(i), formatter))
    }
    if (outputOrdinates.contains(Ordinate.M)) {
      writer.write(" ")
      writer.write(WKTWriter.writeNumber(seq.getM(i), formatter))
    }
  }

  /**
   * Appends additional ordinate information. This function may <ul> <li>append 'Z' if in {@code
   * outputOrdinates} the {@link Ordinate# Z} value is included </li> <li>append 'M' if in {@code
   * outputOrdinates} the {@link Ordinate# M} value is included </li> <li> append 'ZM' if in {@code
   * outputOrdinates} the {@link Ordinate# Z} and {@link Ordinate# M} values are included </li>
   * </ul>
   *
   * @param outputOrdinates
   *   a bit-pattern of ordinates to write.
   * @param writer
   *   the output writer to append to.
   * @throws IOException
   *   if an error occurs while using the writer.
   */
  @throws[IOException]
  private def appendOrdinateText(outputOrdinates: Ordinate.ValueSet, writer: Writer): Unit = {
    if (outputOrdinates.contains(Ordinate.Z)) {
      writer.append(WKTConstants.Z)
    }
    if (outputOrdinates.contains(Ordinate.M)) {
      writer.append(WKTConstants.M)
    }
    ()
  }

  /**
   * Appends all members of a <code>CoordinateSequence</code> to the stream. Each {@code Coordinate}
   * is separated from another using a colon, the ordinates of a {@code Coordinate} are separated by
   * a space.
   *
   * @param seq
   *   the <code>CoordinateSequence</code> to process
   * @param useFormatting
   *   flag indicating that
   * @param level
   *   the indentation level
   * @param indentFirst
   *   flag indicating that the first {@code Coordinate} of the sequence should be indented for
   *   better visibility
   * @param writer
   *   the output writer to append to
   * @param formatter
   *   the formatter to use for writing ordinate values.
   */
  @throws[IOException]
  private def appendSequenceText(
    seq:             CoordinateSequence,
    outputOrdinates: Ordinate.ValueSet,
    useFormatting:   Boolean,
    level:           Int,
    indentFirst:     Boolean,
    writer:          Writer,
    formatter:       OrdinateFormat
  ): Unit =
    if (seq.size == 0) {
      writer.write(WKTConstants.EMPTY)
    } else {
      if (indentFirst) {
        indent(useFormatting, level, writer)
      }
      writer.write("(")
      for (i <- 0 until seq.size) {
        if (i > 0) {
          writer.write(", ")
          if (coordsPerLine > 0 && i % coordsPerLine == 0) {
            indent(useFormatting, level + 1, writer)
          }
        }
        appendCoordinate(seq, outputOrdinates, i, writer, formatter)
      }
      writer.write(")")
    }

  /**
   * Converts a <code>Polygon</code> to &lt;Polygon Text&gt; format, then appends it to the writer.
   *
   * @param polygon
   *   the <code>Polygon</code> to process
   * @param useFormatting
   *   flag indicating that
   * @param level
   *   the indentation level
   * @param indentFirst
   *   flag indicating that the first {@code Coordinate} of the sequence should be indented for
   *   better visibility
   * @param writer
   *   the output writer to append to
   * @param formatter
   *   the formatter to use for writing ordinate values.
   */
  @throws[IOException]
  private def appendPolygonText(
    polygon:         Polygon,
    outputOrdinates: Ordinate.ValueSet,
    useFormatting:   Boolean,
    level:           Int,
    indentFirst:     Boolean,
    writer:          Writer,
    formatter:       OrdinateFormat
  ): Unit =
    if (polygon.isEmpty) {
      writer.write(WKTConstants.EMPTY)
    } else {
      if (indentFirst) {
        indent(useFormatting, level, writer)
      }
      writer.write("(")
      appendSequenceText(polygon.getExteriorRing.getCoordinateSequence,
                         outputOrdinates,
                         useFormatting,
                         level,
                         false,
                         writer,
                         formatter
      )
      for (i <- 0 until polygon.getNumInteriorRing) {
        writer.write(", ")
        appendSequenceText(polygon.getInteriorRingN(i).getCoordinateSequence,
                           outputOrdinates,
                           useFormatting,
                           level + 1,
                           true,
                           writer,
                           formatter
        )
      }
      writer.write(")")
    }

  /**
   * Converts a <code>MultiPoint</code> to &lt;MultiPoint Text&gt; format, then appends it to the
   * writer.
   *
   * @param multiPoint
   *   the <code>MultiPoint</code> to process
   * @param useFormatting
   *   flag indicating that
   * @param level
   *   the indentation level
   * @param writer
   *   the output writer to append to
   * @param formatter
   *   the formatter to use for writing ordinate values.
   */
  @throws[IOException]
  private def appendMultiPointText(
    multiPoint:      MultiPoint,
    outputOrdinates: Ordinate.ValueSet,
    useFormatting:   Boolean,
    level:           Int,
    writer:          Writer,
    formatter:       OrdinateFormat
  ): Unit =
    if (multiPoint.isEmpty) {
      writer.write(WKTConstants.EMPTY)
    } else {
      writer.write("(")
      for (i <- 0 until multiPoint.getNumGeometries) {
        if (i > 0) {
          writer.write(", ")
          indentCoords(useFormatting, i, level + 1, writer)
        }
        appendSequenceText(multiPoint.getGeometryN(i).asInstanceOf[Point].getCoordinateSequence,
                           outputOrdinates,
                           useFormatting,
                           level,
                           false,
                           writer,
                           formatter
        )
      }
      writer.write(")")
    }

  /**
   * Converts a <code>MultiLineString</code> to &lt;MultiLineString Text&gt; format, then appends it
   * to the writer.
   *
   * @param multiLineString
   *   the <code>MultiLineString</code> to process
   * @param useFormatting
   *   flag indicating that
   * @param level
   *   the indentation level //@param indentFirst flag indicating that the first {@code Coordinate}
   *   of the sequence should be indented for // better visibility
   * @param writer
   *   the output writer to append to
   * @param formatter
   *   the formatter to use for writing ordinate values.
   */
  @throws[IOException]
  private def appendMultiLineStringText(
    multiLineString: MultiLineString,
    outputOrdinates: Ordinate.ValueSet,
    useFormatting:   Boolean,
    level:           Int, /*boolean indentFirst, */ writer: Writer,
    formatter:       OrdinateFormat
  ): Unit =
    if (multiLineString.isEmpty) {
      writer.write(WKTConstants.EMPTY)
    } else {
      var level2: Int       = level
      var doIndent: Boolean = false
      writer.write("(")
      for (i <- 0 until multiLineString.getNumGeometries) {
        if (i > 0) {
          writer.write(", ")
          level2 = level + 1
          doIndent = true
        }
        appendSequenceText(
          multiLineString.getGeometryN(i).asInstanceOf[LineString].getCoordinateSequence,
          outputOrdinates,
          useFormatting,
          level2,
          doIndent,
          writer,
          formatter
        )
      }
      writer.write(")")
    }

  /**
   * Converts a <code>MultiPolygon</code> to &lt;MultiPolygon Text&gt; format, then appends it to
   * the writer.
   *
   * @param multiPolygon
   *   the <code>MultiPolygon</code> to process
   * @param useFormatting
   *   flag indicating that
   * @param level
   *   the indentation level
   * @param writer
   *   the output writer to append to
   * @param formatter
   *   the formatter to use for writing ordinate values.
   */
  @throws[IOException]
  private def appendMultiPolygonText(
    multiPolygon:    MultiPolygon,
    outputOrdinates: Ordinate.ValueSet,
    useFormatting:   Boolean,
    level:           Int,
    writer:          Writer,
    formatter:       OrdinateFormat
  ): Unit =
    if (multiPolygon.isEmpty) {
      writer.write(WKTConstants.EMPTY)
    } else {
      var level2: Int       = level
      var doIndent: Boolean = false
      writer.write("(")
      for (i <- 0 until multiPolygon.getNumGeometries) {
        if (i > 0) {
          writer.write(", ")
          level2 = level + 1
          doIndent = true
        }
        appendPolygonText(multiPolygon.getGeometryN(i).asInstanceOf[Polygon],
                          outputOrdinates,
                          useFormatting,
                          level2,
                          doIndent,
                          writer,
                          formatter
        )
      }
      writer.write(")")
    }

  /**
   * Converts a <code>GeometryCollection</code> to &lt;GeometryCollectionText&gt; format, then
   * appends it to the writer.
   *
   * @param geometryCollection
   *   the <code>GeometryCollection</code> to process
   * @param useFormatting
   *   flag indicating that
   * @param level
   *   the indentation level
   * @param writer
   *   the output writer to append to
   * @param formatter
   *   the formatter to use for writing ordinate values.
   */
  @throws[IOException]
  private def appendGeometryCollectionText(
    geometryCollection: GeometryCollection,
    outputOrdinates:    Ordinate.ValueSet,
    useFormatting:      Boolean,
    level:              Int,
    writer:             Writer,
    formatter:          OrdinateFormat
  ): Unit =
    if (geometryCollection.isEmpty) {
      writer.write(WKTConstants.EMPTY)
    } else {
      var level2: Int = level
      writer.write("(")
      for (i <- 0 until geometryCollection.getNumGeometries) {
        if (i > 0) {
          writer.write(", ")
          level2 = level + 1
        }
        appendGeometryTaggedText(geometryCollection.getGeometryN(i),
                                 outputOrdinates,
                                 useFormatting,
                                 level2,
                                 writer,
                                 formatter
        )
      }
      writer.write(")")
    }

  @throws[IOException]
  private def indentCoords(
    useFormatting: Boolean,
    coordIndex:    Int,
    level:         Int,
    writer:        Writer
  ): Unit = {
    if (coordsPerLine <= 0 || coordIndex % coordsPerLine != 0) {
      return
    }
    indent(useFormatting, level, writer)
  }

  @throws[IOException]
  private def indent(useFormatting: Boolean, level: Int, writer: Writer): Unit = {
    if (!useFormatting || level <= 0) {
      return
    }
    writer.write("\n")
    for (_ <- 0 until level)
      writer.write(indentTabStr)
  }
}
