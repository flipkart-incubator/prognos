package com.prognos.forecast

import breeze.linalg.DenseVector
import com.prognos.{DoubleUtil, Series}
import com.prognos.testdata.OilData
import org.scalatest.{Matchers, FlatSpec}

class SimpleExponentialSmoothingTest extends FlatSpec with Matchers {
  it should "calculate forecasts using simple exponential smoothing for alpha=0.2" in {
    val series = Series(OilData.rangeData(1996, 2007))
    val algo = new SimpleExponentialSmoothing
    val (alpha, sesType, horizon) = (0.2, "simple", 3)
    val forecasts = algo.calculate(series, alpha, sesType, horizon)
    forecasts.length should equal(3)
    forecasts.map{value => DoubleUtil.round(value)} should equal(DenseVector(484.80, 484.80, 484.80))
  }
}
