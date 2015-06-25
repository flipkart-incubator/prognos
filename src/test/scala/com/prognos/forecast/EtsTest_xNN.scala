package com.prognos.forecast

/**
 * Created by nikhil.vavs on 11/06/15.
 */

import com.prognos.Series
import breeze.linalg.DenseVector
import com.prognos.util.DoubleUtil
import org.scalatest.{FlatSpec,Matchers}
import com.prognos.testdata.OilData

class EtsTest_xNN extends FlatSpec with Matchers {
  it should "testing ANN, MNN" in {
    val series = Series(OilData.rangeData(1965, 2010))
    val algo = new Ets()
    algo.setInitialValues(111.1126, 0, DenseVector(0))
    val (alpha, beta, gamma, period, horizon) = (0.9999, 0, 0, 1, 3)
    val forecastsANN = algo.calculateETS(series, alpha, beta, gamma, "ANN", period, horizon)
    val forecastsMNN = algo.calculateETS(series, alpha, beta, gamma, "MNN", period, horizon)
    forecastsANN.length should equal(3)
    forecastsANN.map{value => DoubleUtil.round(value)} should equal(DenseVector(467.77, 467.77, 467.77))
    forecastsMNN.length should equal(3)
    forecastsMNN.map{value => DoubleUtil.round(value)} should equal(DenseVector(467.77, 467.77, 467.77))
  }
}
