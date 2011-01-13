package com.stephentu

import java.awt.image._

case class SquareKernel(side: Int) {
  require(side > 0, "SquareKernel requires positive side")

  def computeMask[N](f: (Int, Int) => N)(implicit ev: Numeric[N], m: ClassManifest[N]): SquareMask[N] = {
    val data = for (i <- 0 until side; j <- 0 until side) yield f(i, j)
    new SquareMask[N](side, data.toArray) 
  }
}

/** 5x5 kernel */
object DefaultSquareKernel extends SquareKernel(5)

trait VectorOps[V[N], N] {
  def unit: V[N]
  def fromImage(x: Int, y: Int, img: BufferedImage): V[N]
  def scale(elem: V[N], factor: N): V[N]
  def add(lhs: V[N], rhs: V[N]): V[N] 
}

object VectorOps {
  implicit def singlePxOp[N : Numeric] = new SinglePixelOps[N]
  implicit def triPxOp[N : Numeric] = new TriPixelOps[N]
}

abstract class PixelVectorOps[P[N] <: Pixel[P, N], N] extends VectorOps[P, N] {
  def scale(elem: P[N], factor: N) = elem.scale(factor)
  def add(lhs: P[N], rhs: P[N]) = lhs + rhs
}

class SinglePixelOps[N](implicit ev: Numeric[N]) extends PixelVectorOps[SinglePixel, N] {
  def unit = new SinglePixel[N](ev.zero) 
  def fromImage(x: Int, y: Int, img: BufferedImage) = {
    assert(img.getRaster.getNumBands == 1, "image has > 1 channel")
    new SinglePixel[N](ev.fromInt(img.getRaster.getSample(x, y, 0)))
  }
}

class TriPixelOps[N](implicit ev: Numeric[N]) extends PixelVectorOps[TriPixel, N] {
  def unit = new TriPixel[N](ev.zero, ev.zero, ev.zero)
  def fromImage(x: Int, y: Int, img: BufferedImage) = {
    assert(img.getRaster.getNumBands == 3, "image has != 3 channels")
    TriPixel.fromIntRepr(img.getRGB(x, y))
  }
}

class SquareMask[N](side: Int, data: Array[N])(implicit ev: Numeric[N]) {
  require(side > 0, "SquareMask requires positive side")
  require(data.length == side * side, "data not filled out")

  private val mid = side / 2

  def getData(x: Int, y: Int): N = data(x * side + y)

  def evaluate[V[N]](x: Int, y: Int, img: BufferedImage)(implicit ops: VectorOps[V, N]): V[N] = {

    //println("eval (%d, %d)".format(x, y))

    def outOfXBounds(pos: Int) = pos < 0 || pos >= img.getWidth
    def outOfYBounds(pos: Int) = pos < 0 || pos >= img.getHeight

    val pxes = for (i <- 0 until side; j <- 0 until side) yield {
      val curX = i - mid + x
      val curY = j - mid + y
      //println("  curX = %d, curY = %d".format(curX, curY))
      if (outOfXBounds(curX) || outOfYBounds(curY)) ops.unit 
      else ops.scale(ops.fromImage(curX, curY, img), getData(i, j))
    }
    pxes.foldLeft(ops.unit) { case (acc, vec) => ops.add(acc, vec) }
    //println("  sum = %s".format(sum))
  }
}

trait Convolution[V[N], N, Result] {

  protected def newMask(kernel: SquareKernel): SquareMask[N]
  protected def newResult(img: BufferedImage): Result

  protected def update(x: Int, y: Int, agg: V[N], canvas: Result): Unit
  protected def widthOf(canvas: Result): Int
  protected def heightOf(canvas: Result): Int

  def apply(img: BufferedImage, kernel: SquareKernel): Result = convolve(img, kernel)

  protected implicit def ops: VectorOps[V, N]

  def convolve(img: BufferedImage, kernel: SquareKernel): Result = {
    val canvas = newResult(img)
    val mask   = newMask(kernel)
    for (x <- 0 until widthOf(canvas); y <- 0 until heightOf(canvas)) {
      update(x, y, mask.evaluate[V](x, y, img), canvas)
    }
    canvas
  }
}

class GaussConvolution(val sigma: Double) extends Convolution[TriPixel, Double, BufferedImage] {
  private def gauss(x: Int, y: Int): Double = {
    val sigma_2 = sigma * sigma
    1.0 / (2.0 * math.Pi * sigma_2) * math.exp( - (x*x + y*y).toDouble / (2.0 * sigma_2) ) 
  }

  def ops = implicitly[VectorOps[TriPixel, Double]]

  def newMask(kernel: SquareKernel) = {
    val mid = kernel.side / 2
    kernel.computeMask((x,y) => gauss(math.abs(x - mid), math.abs(y - mid)))
    // identity mask (1 at mid, 0 elsewhere)
    //kernel.computeMask((x, y) => if (x == mid && y == mid) 1.0 else 0.0)
  }

  def newResult(img: BufferedImage) = 
    new BufferedImage(img.getWidth, img.getHeight, BufferedImage.TYPE_INT_ARGB)

  def update(x: Int, y: Int, agg: TriPixel[Double], canvas: BufferedImage) = {
    def toIntRepr(px: TriPixel[Double]) =
      px.b.toInt | (px.g.toInt << 8) | (px.r.toInt << 16) | (0xff << 24)
    canvas.setRGB(x, y, toIntRepr(agg))
  }

  def widthOf(img: BufferedImage) = img.getWidth
  def heightOf(img: BufferedImage) = img.getHeight
}

trait SobelOperator extends Convolution[SinglePixel, Double, GenericImage] {
  def ops = implicitly[VectorOps[SinglePixel, Double]]

  def newResult(img: BufferedImage) =
    new GenericImage(img.getWidth, img.getHeight)

  def update(x: Int, y: Int, agg: SinglePixel[Double], canvas: GenericImage) = 
    canvas.set(x, y, agg.value)

  def widthOf(canvas: GenericImage) = canvas.width
  def heightOf(canvas: GenericImage) = canvas.height
}
