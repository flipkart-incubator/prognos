package com.prognos.forecast

import breeze.linalg.DenseVector
import com.prognos.Series
import breeze.linalg._
import breeze.numerics._

class SimpleExponentialSmoothing {
  def calculate(series: Series, alpha: Double, algoType: String, horizon: Int) = {
    val data = series.data
    var prevValue = data(0)
    var prevLevel = data(0)
    val levels:DenseVector[Double] = data.map { value =>
      val level = alpha * value + (1 - alpha) * prevLevel
      prevValue = value
      prevLevel = level
      level
    }
    val forecasts = DenseVector(Array.fill(horizon)(prevLevel))
    (levels, forecasts)
  }
}
