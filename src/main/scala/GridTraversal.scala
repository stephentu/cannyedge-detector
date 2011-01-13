package com.stephentu

import collection.mutable.ArrayBuffer

trait GridTraversal {
  /**
   * Drop in replacement for
   * for (i <- 0 until x; j <- 0 until y) { 
   *   // do something  
   * }
   */
  @inline final def traverseGrid(x: Int, y: Int)(f: (Int, Int) => Unit): Unit = {
    var i = 0
    while (i < x) {
      var j = 0
      while (j < y) {
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
  @inline final def mapGrid[Elem](x: Int, y: Int)(f: (Int, Int) => Elem): Seq[Elem] = {
    val buf = new ArrayBuffer[Elem]
    var i = 0
    while (i < x) {
      var j = 0
      while (j < y) {
        buf += f(i, j)
        j += 1
      }
      i += 1
    }
    buf
  }
}
