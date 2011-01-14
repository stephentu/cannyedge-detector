package com.stephentu

import collection.mutable.ArrayBuffer

trait GridTraversal {
  /**
   * Drop in replacement for
   * for (i <- 0 until x; j <- 0 until y) { 
   *   // do something  
   * }
   */
  final def traverseGrid[A](x: Int, y: Int)(f: (Int, Int) => A): Unit =
    traverseGrid(0, 0, x, y)(f)

  /**
   * Drop in replacement for
   * for (i <- xStart until xFinish; j <- yStart until yFinish) { 
   *   // do something  
   * }
   */
  final def traverseGrid[A](xStart: Int, yStart: Int, xFinish: Int, yFinish: Int)(f: (Int, Int) => A): Unit = {
    var i = xStart 
    while (i < xFinish) {
      var j = yStart 
      while (j < yFinish) {
        f(i, j)
        j += 1
      }
      i += 1
    }
  }

  /**
   * Drop in replacement for
   * val elems = for (i <- 0 until x; j <- 0 until y) yield { 
   *   // yield something  
   * }
   */
  final def mapGrid[Elem](x: Int, y: Int)(f: (Int, Int) => Elem): Seq[Elem] =
    mapGrid(0, 0, x, y)(f)

  /**
   * Drop in replacement for
   * val elems = for (i <- xStart until xFinish; j <- yStart until yFinish) yield { 
   *   // yield something  
   * }
   */
  final def mapGrid[Elem](xStart: Int, yStart: Int, xFinish: Int, yFinish: Int)(f: (Int, Int) => Elem): Seq[Elem] = {
    val buf = new ArrayBuffer[Elem]
    var i = xStart 
    while (i < xFinish) {
      var j = yStart 
      while (j < yFinish) {
        buf += f(i, j)
        j += 1
      }
      i += 1
    }
    buf
  }
}
