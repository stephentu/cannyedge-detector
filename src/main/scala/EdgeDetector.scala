package com.stephentu

import java.awt.image._
import annotation._

object EdgeDetector {

  sealed abstract class Direction(val angle: Double)
  case object Zero extends Direction(0.0)
  case object FortyFive extends Direction(math.Pi / 4.0)
  case object Ninety extends Direction(math.Pi / 2.0)
  case object OneThirtyFive extends Direction(3.0 / 4.0 * math.Pi)

  def detectEdges(img: BufferedImage): GenericImage[Boolean] = {
    // grayscale if necessary
    val grayImg = 
      if (img.getRaster.getNumBands > 1) ImageUtil.toGrayscale(img)
      else img

    // apply gaussian blur on grayscaled image
    val gaussian = new GrayscaleGaussConvolution(1.6, DefaultSquareKernel)
    val blurredImg = gaussian.convolve(grayImg)

    // apply sobel operator
    val gx = SobelOperatorX.convolve(blurredImg)
    val gy = SobelOperatorY.convolve(blurredImg)
    val g = gx.combine(gy)((l, r) => math.sqrt(l*l + r*r)) 

    // compute angles
    val theta = gx.combine(gy)((l, r) => {
      if (l == 0.0) {
        if (r == 0.0) 0.0
        else math.Pi / 2.0
      } else math.atan(r / l)
    })

    @tailrec def canonicalAngle(th: Double): Double = 
      if (th >= math.Pi) canonicalAngle(th - math.Pi)
      else if (th < 0.0) canonicalAngle(th + math.Pi)
      else th

    def canonicalAngleToDirection(th: Double): Direction = 
      if (0.0 <= th && th < (math.Pi / 8.0)) Zero
      else if ((math.Pi / 8.0) <= th && th < (math.Pi * (3.0/8.0))) FortyFive
      else if ((math.Pi * (3.0/8.0)) <= th && th < (math.Pi * (5.0/8.0))) Ninety 
      else if ((math.Pi * (5.0/8.0)) <= th && th < (math.Pi * (7.0/8.0))) OneThirtyFive
      else if ((math.Pi * (7.0/8.0)) <= th && th < math.Pi) Zero
      else error("not canonical angle: " + th)

    // canonicalize the angles into 4 directions
    val directions = theta.map(th => canonicalAngleToDirection(canonicalAngle(th)))

    def safeGet(x: Int, y: Int) = 
      if (x < 0 || x >= g.width || y < 0 || y >= g.height) None
      else Some(g.get(x, y))

    // apply non-maximum suppression
    directions.mapWithIndex((x, y, dir) => (g.get(x, y), dir match {
      case Zero =>
        (safeGet(x+1, y), safeGet(x-1, y)) 
      case FortyFive =>
        (safeGet(x+1, y+1), safeGet(x-1, y-1))
      case Ninety =>
        (safeGet(x, y+1), safeGet(x, y-1))
      case OneThirtyFive =>
        (safeGet(x+1, y-1), safeGet(x-1, y+1))
    })).map { case (center, (leftOpt, rightOpt)) =>
      val isLeftBigger = leftOpt.map(_ > center)
      val isRightBigger = rightOpt.map(_ > center)
      !isLeftBigger.getOrElse(false) && !isRightBigger.getOrElse(false)
    }
  }
}
