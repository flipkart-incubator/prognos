package com.prognos.forecast

import breeze.linalg._
import breeze.numerics._
import com.prognos.Series

import scala.collection.immutable.Range.Inclusive

class MovingAverage {
  def forecast(series:Series, order:Int):Series = {
    if(!isOdd(order)) throw new IllegalArgumentException("MovingAverage order must be odd")
    val k = (order - 1) / 2
    val length = series.data.length
    val averages:Array[Double] = (k to (length-1-k)).map { i =>
      val range = (i - k) to (i + k)
      series.data(range).sum / order
    }.toArray
    Series(DenseVector(averages))
  }

  private def isOdd(n:Int) = n % 2 == 1
}
