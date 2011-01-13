package com.stephentu

/** Generic 2D image with double (single) pixels */
class GenericImage(val width: Int, val height: Int) {
  require(width > 0, "width must be > 0")
  require(height > 0, "height must be > 0")

  private val buffer = new Array[Double](width * height)

  // no bounds checking for speed
  
  def get(x: Int, y: Int): Double = 
    buffer(x * width + height)

  def set(x: Int, y: Int, value: Double): Unit = 
    buffer(x * width + height) = value
}
