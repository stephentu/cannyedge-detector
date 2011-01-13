package com.stephentu

/** Generic 2D image with double (single) pixels */
class GenericImage[@specialized(Double, Int, Boolean) Elem](val width: Int, val height: Int)(implicit m: ClassManifest[Elem]) {
  require(width > 0, "width must be > 0")
  require(height > 0, "height must be > 0")

  private val buffer = new Array[Elem](width * height)

  // no bounds checking for speed
  
  def get(x: Int, y: Int): Elem = 
    buffer(x * width + height)

  def set(x: Int, y: Int, value: Elem): Unit = 
    buffer(x * width + height) = value

  def combine[ThatElem, ResElem : ClassManifest](that: GenericImage[ThatElem])(f: (Elem, ThatElem) => ResElem): GenericImage[ResElem] = {
    // TODO: relax assumption
    assert(width == that.width && height == that.height)
    val newImg = new GenericImage[ResElem](width, height)
    for (i <- 0 until width; j <- 0 until height) {
      newImg.set(i, j, f(get(i, j), that.get(i, j)))
    }
    newImg
  }

  def map[ToElem : ClassManifest](f: Elem => ToElem): GenericImage[ToElem] =
    mapWithIndex((_, _, e) => f(e))

  def mapWithIndex[ToElem : ClassManifest](f: (Int, Int, Elem) => ToElem): GenericImage[ToElem] = {
    val newImg = new GenericImage[ToElem](width, height)
    for (i <- 0 until width; j <- 0 until height) newImg.set(i, j, f(i, j, get(i, j)))
    newImg
  }

  def foreachWithIndex(f: (Int, Int, Elem) => Unit): Unit = 
    for (i <- 0 until width; j <- 0 until height) f(i, j, get(i, j))

  def foreach(f: Elem => Unit): Unit = foreachWithIndex((_, _, e) => f(e))
}
