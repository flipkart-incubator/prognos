package com.ets.forecast

/**
 * Created by nikhil.vavs on 11/06/15.
 */


import com.prognos.Series
import breeze.linalg.DenseVector
import com.prognos.util.DoubleUtil
import org.scalatest.{FlatSpec,Matchers}
import com.prognos.testdata.TouristsData

class EtsTest_xAA extends FlatSpec with Matchers{
  it should "testing AAA, MAA" in {
    val series = Series(TouristsData.seasonalRangeData(1999, 2010).map {value => value})
    //    Smoothing parameters: for AAA
    //      alpha = 0.2889
    //    beta  = 1e-04
    //    gamma = 0.4452
    //
    //    Initial states:
    //      l = 24.9416
    //    b = 0.5088
    //    s=0.9781, -0.7702, -6.9848, 6.7769


    //    Smoothing parameters: for maa
    //      alpha = 0.3804
    //    beta  = 1e-04
    //    gamma = 0.3507
    //
    //    Initial states:
    //      l = 24.1182
    //    b = 0.4086
    //    s=2.1252, -1.2615, -6.08, 5.2162

    val algo = new Ets()
    algo.setInitialValues(24.9416, 0.5088, DenseVector(0.9781, -0.7702, -6.9848, 6.7769))
    val (alpha, beta, gamma, period, horizon) = (0.2889, 0.0001, 0.4452, 4, 4)
    val forecasts = algo.calculateETS(series, alpha, beta, gamma, "AAA", period , horizon)
    forecasts.length should equal(4)
    forecasts.map(DoubleUtil.round(_, 1)) should equal(DenseVector(60.1, 37.5, 46.4, 50.0))
  }

}
