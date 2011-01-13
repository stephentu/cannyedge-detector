package com.stephentu

import java.awt.image._
import java.io._
import javax.imageio._

object TestApp {
  def visualize(bitmap: GenericImage[Boolean]): BufferedImage = {
    val img = new BufferedImage(bitmap.width, bitmap.height, BufferedImage.TYPE_BYTE_GRAY)
    bitmap.foreachWithIndex((x, y, bool) => if (bool) img.getRaster.setSample(x, y, 0, 255) else img.getRaster.setSample(x, y, 0, 0))
    img
  }

  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      System.err.println("[USAGE] TestApp [in file] [out file]")
      System.exit(1)
    }
    val inFile = args(0)
    val outFile = args(1)
    val inImg = ImageIO.read(new File(inFile))

    val edges = EdgeDetector.detectEdges(inImg)
    val outImg = visualize(edges)

    // TODO: use extension of file name
    ImageIO.write(outImg, "png", new File(outFile))
  }
}
