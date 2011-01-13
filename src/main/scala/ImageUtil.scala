package com.stephentu

import java.awt.image._

object ImageUtil extends GridTraversal {
  def toGrayscale(img: BufferedImage): BufferedImage = {
    val gs = new BufferedImage(img.getWidth, img.getHeight, BufferedImage.TYPE_BYTE_GRAY)
    traverseGrid(img.getWidth, img.getHeight)((i, j) => {
      val px = TriPixel.fromIntRepr[Double](img.getRGB(i, j))
      val gsValue = 0.3 * px.r.toDouble + 0.59 * px.g.toDouble + 0.11 * px.b.toDouble
      gs.getRaster.setSample(i, j, 0, gsValue.toInt) 
    })
    gs
  }
}
