package com.stephentu

import java.io._
import javax.imageio._

object TestApp {
  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      System.err.println("[USAGE] TestApp [in file] [out file]")
      System.exit(1)
    }
    val inFile = args(0)
    val outFile = args(1)
    val inImg = ImageIO.read(new File(inFile))
    val outImg = (new GaussConvolution(1.6)).convolve(inImg, DefaultSquareKernel)
    // TODO: use extension of file name
    ImageIO.write(outImg, "png", new File(outFile))
  }
}
