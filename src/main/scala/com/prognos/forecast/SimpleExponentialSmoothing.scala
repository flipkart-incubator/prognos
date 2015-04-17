package com.prognos.forecast

import breeze.linalg.DenseVector
import com.prognos.Series
import breeze.linalg._
import breeze.numerics._

class SimpleExponentialSmoothing {
  def calculate(series: Series, alpha: Double, algoType: String, horizon: Int) = {
    if(!"simple".equals(algoType)) throw new IllegalArgumentException("Invalid SES algoType:" + algoType)
    val data = series.data
    val initialLevel = data(0)
    val forecast = data.foldLeft(initialLevel) {case (level, value) =>
      alpha * value + (1 - alpha) * level
    }
    DenseVector(Array.fill(horizon)(forecast))
  }
}
