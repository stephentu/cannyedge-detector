package com.stephentu

import java.awt.image._
import java.io._
import javax.imageio._

import collection.mutable.HashSet

object EdgeDetector extends GridTraversal {

  // for non-max suppression part
  private sealed abstract class Direction(val angle: Double)
  private case object Zero extends Direction(0.0)
  private case object FortyFive extends Direction(math.Pi / 4.0)
  private case object Ninety extends Direction(math.Pi / 2.0)
  private case object OneThirtyFive extends Direction(3.0 / 4.0 * math.Pi)

  // for hystersis part
  private sealed trait EdgeType
  private case object NoEdge extends EdgeType
  private case object WeakEdge extends EdgeType
  private case object StrongEdge extends EdgeType

  private val Debug = util.Properties.propOrFalse("canny.debug")

  private def sobel(img: BufferedImage): (GenericImage[Double], GenericImage[Double]) = {
    assert(img.getRaster.getNumBands == 1, "grayscale img only")

    // apply sobel operator
    val gx = SobelOperatorX.convolve(img)
    val gy = SobelOperatorY.convolve(img)
    val g = gx.combine(gy)((l, r) => math.sqrt(l*l + r*r)) 

    // compute angles
    val theta = gx.combine(gy)((l, r) => {
      val res = math.atan(r / l)
      if (java.lang.Double.isNaN(res)) 0.0 else res
    })

    (g, theta)
  }

  def detectEdges(img: BufferedImage, thresholds: Option[(Double, Double)] = None): GenericImage[Boolean] = {
    thresholds.foreach { case (low, high) => require(low <= high, "invalid thresholds given") }

    // this implementation is based off of:
    // http://www.cs.uiowa.edu/~cwyman/classes/spring08-22C251/homework/canny.pdf

    // grayscale if necessary
    val grayImg = 
      if (img.getRaster.getNumBands > 1) ImageUtil.toGrayscale(img)
      else img

    // apply gaussian blur on grayscaled image
    val gaussian = new GrayscaleGaussConvolution(1.6, DefaultSquareKernel)
    val blurredImg = gaussian.convolve(grayImg)

    // apply sobel operator for image gradients
    val (g, theta) = sobel(blurredImg)

    if (Debug)
      ImageIO.write(ImageVisualizer.visualize(g), "png", new File("debug_gradient.png"))

    @inline def canonicalAngle(th: Double): Double = 
      if (th < 0.0) th + math.Pi else th

    @inline def canonicalAngleToDirection(th: Double): Direction = 
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
      val isLeftSmaller = leftOpt.map(_ < center)
      val isRightSmaller = rightOpt.map(_ < center)
      isLeftSmaller.getOrElse(false) && isRightSmaller.getOrElse(false)
    }

    if (Debug) {
      val numEdges = initialGrid.count(identity) 
      val d = initialGrid.mapWithIndex {
        case (x, y, true) => g.get(x, y)
        case (x, y, false) => 0.0
      }
      System.err.println("numEdges = %d".format(numEdges))
      ImageIO.write(ImageVisualizer.visualize(d), "png", new File("debug_d.png"))
    }

    // thresholding (hystersis)
    val hist = new Histogram[Double](initialGrid.mapWithIndex {
      case (x, y, true) => Some(g.get(x, y))
      case (x, y, false) => None
    }.toArray.flatMap(_.toList).toArray)

    val (thigh, tlow) = thresholds.map { case (low, high) => (high, low) }.getOrElse {
      val Thigh = hist.percentile(0.85) // 85th percentile of gradients for high threshold
      val Tlow  = hist.percentile(0.70) // 70th percentile of gradients for low threshold
      (Thigh, Tlow)
    }

    if (Debug) {
      System.err.println("THigh = %f, TLow = %f, max = %f".format(thigh, tlow, hist.max))
      val sup = initialGrid.mapWithIndex {
        case (x, y, true) => 
          if (g.get(x, y) > thigh) 255.0
          else if (tlow <= g.get(x, y) && g.get(x, y) <= thigh) 127.0
          else 0.0
        case _ => 0.0
      }
      ImageIO.write(ImageVisualizer.visualize(sup), "png", new File("debug_sup.png"))
    }

    // TODO: throw error on overflow
    var counter = 0
    def freshId(): Int = {
      val res = counter
      counter += 1
      res
    }

    val edgeGrid = initialGrid.mapWithIndex {
      case (x, y, true) => 
        val thisGrad = g.get(x, y)
        if (thisGrad < tlow) NoEdge
        else if (tlow <= thisGrad && thisGrad < thigh) WeakEdge
        else StrongEdge
      case _ => NoEdge
    }

    val nodeGrid = new GenericImage[Option[DisjointSet.Node[Int]]](edgeGrid.width, edgeGrid.height)

    // raster connected components algorithm
    edgeGrid.foreachWithIndex {
      case (x, y, StrongEdge | WeakEdge) =>
        // neighbors are other strong/weak edges
        val coords = List( (x-1, y), (x-1, y-1), (x, y-1), (x+1, y-1) )
        @inline def isOOB(xp: Int, yp: Int): Boolean =
          xp < 0 || xp >= nodeGrid.width || yp < 0 || yp >= nodeGrid.height
        val neighbors = coords.map { case (xp, yp) => if (isOOB(xp, yp)) None else nodeGrid.get(xp, yp) }.flatMap(_.toList)
        
        if (neighbors.isEmpty)
          nodeGrid.set(x, y, Some(DisjointSet.unit(freshId())))
        else {
          val thisPx = neighbors.head
          nodeGrid.set(x, y, Some(thisPx))
          neighbors.tail.foreach(_.unionWith(thisPx))
        }
      case (x, y, NoEdge) =>
        nodeGrid.set(x, y, None)
    }

    val strongEdges = new HashSet[Int]
    edgeGrid.foreachWithIndex((i, j, e) => if (e == StrongEdge) strongEdges += nodeGrid.get(i, j).get.repr.element)

    nodeGrid.map {
      case Some(node) => strongEdges.contains(node.repr.element)
      case None => false
    }
  }
}
