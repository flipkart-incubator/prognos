package com.prognos.forecast

import breeze.linalg.DenseVector
import com.prognos.{DoubleUtil, Series}
import com.prognos.testdata.ElectricalSales
import org.scalatest.{Matchers, FlatSpec}

class MovingAverageTest extends FlatSpec with Matchers {
  it should "calculate moving averages" in {
    val series = Series(ElectricalSales.data)
    val algo = new MovingAverage
    val forecasts:Series = algo.forecast(series, 5)
    forecasts.data.length should equal(16)
    val actual:DenseVector[Double] = forecasts.data.map {x => DoubleUtil.round(x)}
    actual should equal(ElectricalSales.expected)
  }
}
