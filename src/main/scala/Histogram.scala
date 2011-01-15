package com.stephentu

/** an immutable histogram */
class Histogram[N](values: Array[N])(implicit ev: Numeric[N]) {
  require(values.length > 0, "values cannot be empty in histogram")

  private val sorted = values.sorted

  /** p in [0.0, 1.0] ? */
  private def isPercent(p: Double) = 
    p >= 0.0 && p <= 1.0

  def percentile(p: Double): N = {
    require(isPercent(p), "Not a valid percentile: " + p)

    // nearest rank definition of percentile
    if (p == 1.0) sorted(sorted.length - 1) // special case
    else {
      val rank = math.round(p * sorted.length.toDouble).toInt
      sorted(rank)
    }
  }

  def max: N = sorted(sorted.length - 1)
  def min: N = sorted(0)
}
