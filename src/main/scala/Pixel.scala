package com.stephentu

object TriPixel {
  /**
   * Creates a TriPixel from a TYPE_INT_ARGB representation
   * http://download.oracle.com/javase/6/docs/api/java/awt/image/BufferedImage.html
   */
  def fromIntRepr[N](repr: Int)(implicit ev: Numeric[N]): TriPixel[N] = {
    val b = (repr & 0xff)
    val g = ((repr >> 8) & 0xff)
    val r = ((repr >> 16) & 0xff)
    val scale = ((repr >> 24) & 0xff).toDouble / 255.0
    new TriPixel[N](ev.fromInt((r * scale).toInt), ev.fromInt((g * scale).toInt), ev.fromInt((b * scale).toInt))
  }
}

trait Pixel[Repr[Number], @specialized(Double, Int) Number] {
  def scale(n: Number): Repr[Number]
  def plus(that: Repr[Number]): Repr[Number]

  def *(n: Number): Repr[Number] = scale(n)
  def +(that: Repr[Number]): Repr[Number] = plus(that)
}

class TriPixel[@specialized(Double, Int) N](val r: N, val g: N, val b: N)(implicit ev: Numeric[N]) 
  extends Pixel[TriPixel, N] {
  def scale(n: N) = 
    new TriPixel[N](ev.times(r, n), ev.times(g, n), ev.times(b, n))
  def plus(p: TriPixel[N]) =
    new TriPixel[N](ev.plus(r, p.r), ev.plus(g, p.g), ev.plus(b, p.b))
}

class SinglePixel[@specialized(Double, Int) N](val value: N)(implicit ev: Numeric[N]) extends Pixel[SinglePixel, N] {
  def scale(n: N) = 
    new SinglePixel[N](ev.times(value, n))
  def plus(p: SinglePixel[N]) =
    new SinglePixel[N](ev.plus(value, p.value))
}
