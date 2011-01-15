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
  def toImage[N : Numeric](width: Int, height: Int, data: Array[N]): BufferedImage = {
    require(width * height == data.length)
    val img = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_GRAY)
    val ev = implicitly[Numeric[N]]
    traverseGrid(width, height)((i, j) => img.getRaster.setSample(i, j, 0, ev.toInt(data(j * width + i))))
    img
  }
  def toImage[N : Numeric](data: Array[Array[N]]): BufferedImage = {
    require(data.isEmpty || data.map(_.length).toSet.size == 1, "rows all need same length")
    val width = if (data.isEmpty) 0 else data(0).length
    val img = new BufferedImage(width, data.length, BufferedImage.TYPE_BYTE_GRAY)
    val ev = implicitly[Numeric[N]]
    traverseGrid(width, data.length)((i, j) => img.getRaster.setSample(i, j, 0, ev.toInt(data(j)(i))))
    img
  }
}
