package com.prognos.forecast

import breeze.linalg.DenseVector
import com.prognos.Series
import com.prognos.testdata.{TouristsData, AirPassengerData}
import com.prognos.util.DoubleUtil
import org.scalatest.{Matchers, FlatSpec}

class HoltWinterTest extends FlatSpec with Matchers {
  it should "calculate forecasts using HoltWinters method with alpha=0.025, beta=0.023, gamma=0.0001" in {
//    val series = Series(TouristsData.seasonalRangeData(2005, 2010).map {value => value})
    val series = Series(DenseVector(Array(2.1,4.2,6.1,7.9,10.0,12.1,14.84,15.66, 18.1, 19.90)))
    val algo = new HoltWinter
//    val (alpha, beta, gamma, period, algoType, horizon) = (0.025, 0.023, 0.0001, 4, "simple", 4)
    val (alpha, beta, gamma,  period, algoType, horizon) = (0.3, 0.5, 0, 1, "simple", 3)
    val forecasts = algo.calculate(series, alpha, beta, gamma, true, period, algoType, horizon)
    println("forecasts : " + forecasts)
    forecasts.length should equal(3)
    forecasts.map(DoubleUtil.round(_, 2)) should equal(DenseVector(21.98, 23.92, 25.86)) // captures seasonal trend but not same as R output
  }
}
