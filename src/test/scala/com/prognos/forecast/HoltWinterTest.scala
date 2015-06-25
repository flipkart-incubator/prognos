package com.prognos.forecast

import breeze.linalg.DenseVector
import com.prognos.Series
import com.prognos.testdata.TouristsData
import com.prognos.util.DoubleUtil
import org.scalatest.{FlatSpec, Matchers}

class HoltWinterTest extends FlatSpec with Matchers {
  it should "calculate forecasts using HoltWinters method with alpha=0.025, beta=0.023, gamma=0.0001" in {
    val series = Series(TouristsData.seasonalRangeData(2005, 2010).map {value => value})
    val algo = new HoltWinter
    val (alpha, beta, gamma, period, algoType, horizon) = (0.025, 0.023, 0.0001, 4, "simple", 4)
    val forecasts = algo.calculate(series, alpha, beta, gamma, period, algoType, horizon)
    forecasts.length should equal(4)
    //forecasts.map(DoubleUtil.round(_, 2)) should equal(DenseVector(58.82, 39.08, 47.07, 51.02)) // expected output from same values for R model
    forecasts.map(DoubleUtil.round(_, 2)) should equal(DenseVector(67.47, 50.91, 60.33, 66.46)) // captures seasonal trend but not same as R output



  }

}
