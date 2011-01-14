package com.stephentu

import java.awt.image._
import annotation._

object EdgeDetector extends GridTraversal {

  sealed abstract class Direction(val angle: Double)
  case object Zero extends Direction(0.0)
  case object FortyFive extends Direction(math.Pi / 4.0)
  case object Ninety extends Direction(math.Pi / 2.0)
  case object OneThirtyFive extends Direction(3.0 / 4.0 * math.Pi)

  private val Tlow = 25.0
  private val Thigh = 35.0

  /** most likely should not turn thresholding off, but the option is here */
  private val DoThresholding = true

  def detectEdges(img: BufferedImage): GenericImage[Boolean] = {

    // this implementation is based off of:
    // http://www.cs.uiowa.edu/~cwyman/classes/spring08-22C251/homework/canny.pdf

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

    @inline def safeGet(x: Int, y: Int) = 
      if (x < 0 || x >= g.width || y < 0 || y >= g.height) None
      else Some(g.get(x, y))

    // apply non-maximum suppression
    val initialGrid = directions.mapWithIndex((x, y, dir) => (g.get(x, y), dir match {
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

    // thresholding
    if (DoThresholding)
      initialGrid.mapWithIndex {
        case (x, y, true) => 
          val thisGrad = g.get(x, y)
          if (thisGrad < Tlow) false
          else if (Tlow <= thisGrad && thisGrad <= Thigh) {
            import math._
            // look @ 3x3 grid around (x, y)
            val xUpper = max(0, x - 1)
            val yUpper = max(0, y - 1)
            val xLower = min(initialGrid.width - 1, x + 1)
            val yLower = min(initialGrid.height - 1, y + 1)

            var keep = false
            var canDoLargerSearch = false

            @inline def notCurrentPos(xp: Int, yp: Int) = xp != x && yp != y
            @inline def exceedsThigh(gval: Double) = gval > Thigh
            @inline def inBetweenTlowAndThigh(gval: Double) = Tlow <= gval && gval <= Thigh

            traverseGrid(xUpper, yUpper, xLower + 1, yLower + 1) {
              case (xp, yp) 
                if !keep && notCurrentPos(xp, yp) && exceedsThigh(g.get(xp, yp)) => 
                  keep = true
              case (xp, yp) 
                if !keep && !canDoLargerSearch && notCurrentPos(xp, yp) && inBetweenTlowAndThigh(g.get(xp, yp)) =>
                  canDoLargerSearch = true
              case _ => // no-op
            }

            if (keep) true
            else if (canDoLargerSearch) {
              // look @ 5x5 grid around (x, y)
              // since we already looked at the 3x3 grid around (x, y) and we
              // didn't keep, then we know we only need to look at the pixels on
              // the edge of the 5x5 region
              val xUpper = max(0, x - 2)
              val yUpper = max(0, y - 2)
              val xLower = min(initialGrid.width - 1, x + 2)
              val yLower = min(initialGrid.height - 1, y + 2)

              @inline def onBoundary(xp: Int, yp: Int) = 
                xp == xUpper || xp == xLower || yp == yUpper || yp == yLower

              traverseGrid(xUpper, yUpper, xLower + 1, yLower + 1) {
                case (xp, yp) 
                  if !keep && onBoundary(xp, yp) /*&& notCurrentPos(xp, yp)*/ && exceedsThigh(g.get(xp, yp)) => 
                    // we omit notCurrentPos b/c we already check to see if its
                    // on the boundary. the only case where onBoundary returns
                    // true for the current position is the degenerate case of a
                    // 1x1 pixel, in which case the problem is not really well
                    // defined
                    keep = true
                case _ => // no-op
              }

              keep
            } else false
          } else true
        case _ => false
      }
    else initialGrid
  }
}
