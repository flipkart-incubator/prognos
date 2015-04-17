package com.prognos.forecast

import breeze.linalg.DenseVector
import com.prognos.{DoubleUtil, Series}
import com.prognos.testdata.ElectricalSalesData
import org.scalatest.{Matchers, FlatSpec}

class MovingAverageTest extends FlatSpec with Matchers {
  it should "calculate moving averages" in {
    val series = Series(ElectricalSalesData.data)
    val algo = new MovingAverage
    val forecasts:Series = algo.calculate(series, 5)
    forecasts.data.length should equal(16)
    val actual:DenseVector[Double] = forecasts.data.map {x => DoubleUtil.round(x)}
    actual should equal(ElectricalSalesData.expected)
  }
}
