package com.prognos.forecast

import breeze.linalg.DenseVector
import com.prognos.Series
import com.prognos.testdata.AirPassengerData
import com.prognos.util.DoubleUtil
import org.scalatest.{Matchers, FlatSpec}

class HoltLinearTest extends FlatSpec with Matchers {
  it should "calculate forecasts using HoltLinear method with alpha=0.8 and beta=0.2" in {
    val series = Series(AirPassengerData.rangeData(1990, 2004).map {value => DoubleUtil.round(value)})
    val algo = new HoltLinear
    val (alpha, beta, algoType, horizon) = (0.8, 0.2, "simple", 5)
    val forecasts = algo.calculate(series, alpha, beta, algoType, horizon)
    forecasts.length should equal(5)
    forecasts.map(DoubleUtil.round(_, 1)) should equal(DenseVector(43.8, 45.6, 47.4, 49.3, 51.1))
  }
}
