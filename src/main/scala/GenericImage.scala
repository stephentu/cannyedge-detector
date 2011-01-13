package com.stephentu

/** Generic 2D image with double (single) pixels */
class GenericImage[@specialized(Double, Int, Boolean) Elem](val width: Int, val height: Int) extends GridTraversal {
  require(width > 0, "width must be > 0")
  require(height > 0, "height must be > 0")

  private final val buffer = new Array[AnyRef](width * height)

  // no bounds checking for speed
  
  final def get(x: Int, y: Int): Elem = 
    buffer(y * width + x).asInstanceOf[Elem]

  final def set(x: Int, y: Int, value: Elem): Unit =
    buffer(y * width + x) = value.asInstanceOf[AnyRef]

  def combine[ThatElem, ResElem](that: GenericImage[ThatElem])(f: (Elem, ThatElem) => ResElem): GenericImage[ResElem] = {
    // TODO: relax assumption
    assert(width == that.width && height == that.height)
    val newImg = new GenericImage[ResElem](width, height)
    traverseGrid(width, height)((i, j) => newImg.set(i, j, f(get(i, j), that.get(i, j))))
    newImg
  }

  def map[ToElem](f: Elem => ToElem): GenericImage[ToElem] =
    mapWithIndex((_, _, e) => f(e))

  def mapWithIndex[ToElem](f: (Int, Int, Elem) => ToElem): GenericImage[ToElem] = {
    val newImg = new GenericImage[ToElem](width, height)
    traverseGrid(width, height)((i, j) => newImg.set(i, j, f(i, j, get(i, j))))
    newImg
  }

  def foreachWithIndex(f: (Int, Int, Elem) => Unit): Unit = 
    traverseGrid(width, height)((i, j) => f(i, j, get(i, j)))

  def foreach(f: Elem => Unit): Unit = foreachWithIndex((_, _, e) => f(e))

  def to2DArray[SuperElem >: Elem : ClassManifest]: Array[Array[SuperElem]] = 
    (0 until height).map(i => buffer.slice(i * height, i * height + width).map(_.asInstanceOf[SuperElem]).toArray).toArray

  def toArray[SuperElem >: Elem : ClassManifest]: Array[SuperElem] = buffer.map(_.asInstanceOf[SuperElem]).toArray
}
