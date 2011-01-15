package com.stephentu

import collection.mutable.ArrayBuffer

trait GridTraversal {
  /**
   * Drop in replacement for
   * for (j <- 0 until y; i <- 0 until x) { 
   *   // do something  
   * }
   */
  final def traverseGrid[A](x: Int, y: Int)(f: (Int, Int) => A): Unit =
    traverseGrid(0, 0, x, y)(f)

  /**
   * Drop in replacement for
   * for (j <- yStart until yFinish; i <- xStart until xFinish) { 
   *   // do something  
   * }
   */
  final def traverseGrid[A](xStart: Int, yStart: Int, xFinish: Int, yFinish: Int)(f: (Int, Int) => A): Unit = {
    var j = yStart 
    while (j < yFinish) {
      var i = xStart 
      while (i < xFinish) {
        f(i, j)
        i += 1
      }
      j += 1
    }
  }

  /**
   * Drop in replacement for
   * val elems = for (j <- 0 until y; i <- 0 until x) yield { 
   *   // yield something  
   * }
   */
  final def mapGrid[Elem](x: Int, y: Int)(f: (Int, Int) => Elem): Seq[Elem] =
    mapGrid(0, 0, x, y)(f)

  /**
   * Drop in replacement for
   * val elems = for (j <- yStart until yFinish; i <- xStart until xFinish) yield { 
   *   // yield something  
   * }
   */
  final def mapGrid[Elem](xStart: Int, yStart: Int, xFinish: Int, yFinish: Int)(f: (Int, Int) => Elem): Seq[Elem] = {
    val buf = new ArrayBuffer[Elem]
    var j = yStart 
    while (j < yFinish) {
      var i = xStart 
      while (i < xFinish) {
        buf += f(i, j)
        i += 1
      }
      j += 1
    }
    buf
  }
}
