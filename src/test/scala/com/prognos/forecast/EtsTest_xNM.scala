package com.ets.forecast

/**
 * Created by nikhil.vavs on 11/06/15.
 */

import com.prognos.Series
import breeze.linalg.DenseVector
import com.prognos.util.DoubleUtil
import org.scalatest.{FlatSpec,Matchers}
import com.prognos.testdata.TouristsData

// anm is not allowed
class EtsTest_xNM extends FlatSpec with Matchers {
  it should "testing MNM" in {
    val series = Series(TouristsData.seasonalRangeData(1999, 2010))
    val algo = new Ets(25.5275, 0, DenseVector(1.290, 0.777, 0.965, 1.0786))
    val (alpha, beta, gamma, period, horizon) = (0.5158, 0, 0.0001, 4, 4)
    //    Smoothing parameters:
    //      alpha = 0.5158
    //    gamma = 1e-04
    //
    //    Initial states:
    //      l = 25.5275
    //    s=1.290, 0.777, 0.965, 1.0786

    // change this to MNA for testing MNA
    val forecastsMNM = algo.calculateETS(series, alpha, beta, gamma, "MNM", period, horizon)
    forecastsMNM.length should equal(4)
    forecastsMNM.map{value => DoubleUtil.round(value,1)} should equal(DenseVector(57.9, 34.9,43.4, 48.5))
  }

}
