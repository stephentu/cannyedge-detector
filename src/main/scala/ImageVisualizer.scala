package com.stephentu

import java.awt.image._

trait ImageVisualizer[T] {
  def visualize(img: GenericImage[T]): BufferedImage
}

object ImageVisualizer {
  trait BooleanImageVisualizer extends ImageVisualizer[Boolean] {
    def visualize(bitmap: GenericImage[Boolean]) = {
      val img = new BufferedImage(bitmap.width, bitmap.height, BufferedImage.TYPE_BYTE_GRAY)
      bitmap.foreachWithIndex((x, y, bool) => if (bool) img.getRaster.setSample(x, y, 0, 255) else img.getRaster.setSample(x, y, 0, 0))
      img
    }
  }
  implicit object BooleanVisualizer extends BooleanImageVisualizer

  trait NumericImageVisualizer[N] extends ImageVisualizer[N] {
    implicit def ev: Numeric[N]
    def visualize(img: GenericImage[N]) = {
      val viz = new BufferedImage(img.width, img.height, BufferedImage.TYPE_BYTE_GRAY)

      // simple strategy for now: map [min, max] -> [0, 255]
      val min = ev.toDouble(img.min)
      val max = ev.toDouble(img.max)
      val diff = max - min 

      def mapped(value: N): Int =
        // (value - min)/(max - min) * 255
        ((ev.toDouble(value) - min)/diff * 255.0).toInt

      img.foreachWithIndex((x, y, value) => viz.getRaster.setSample(x, y, 0, mapped(value)))
      viz
    }
  }
  implicit def numericViz[N : Numeric]: ImageVisualizer[N] = new NumericImageVisualizer[N] {
    def ev = implicitly[Numeric[N]]
  }

  def visualize[T](img: GenericImage[T])(implicit viz: ImageVisualizer[T]) = 
    viz.visualize(img)
}
