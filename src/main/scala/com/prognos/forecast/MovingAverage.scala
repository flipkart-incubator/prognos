package com.prognos.forecast

import breeze.linalg._
import breeze.numerics._
import com.prognos.Series

import scala.collection.immutable.Range.Inclusive

class MovingAverage {

  def calculate(series:Series, order:Int):Series = {
    if(!isOdd(order)) throw new IllegalArgumentException("MovingAverage order must be odd")
    val averages = series.data.toArray.sliding(order).map(avg)
    Series(DenseVector(averages.toArray))
  }

  private def isOdd(n:Int) = n % 2 == 1

  private def avg(values: Array[Double]): Double = values.sum / values.length
}
