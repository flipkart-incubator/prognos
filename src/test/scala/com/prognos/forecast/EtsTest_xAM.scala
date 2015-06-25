package com.prognos.forecast

/**
 * Created by nikhil.vavs on 11/06/15.
 */

import com.prognos.Series
import breeze.linalg.DenseVector
import com.prognos.util.DoubleUtil
import org.scalatest.{FlatSpec,Matchers}
import com.prognos.testdata.TouristsData

class EtsTest_xAM extends FlatSpec with Matchers{
  // aam is forbidden
  it should "testing MAM" in {
    val series = Series(TouristsData.seasonalRangeData(1999, 2010).map {value => value})
    //    Smoothing parameters:
    //      alpha = 0.4544
    //    beta  = 1e-04
    //    gamma = 1e-04
    //
    //    Initial states:
    //      l = 24.5123
    //    b = 0.4898
    //    s=1.2772, 0.760, 0.9907, 1.0788
    val algo = new Ets()
    algo.setInitialValues(24.5123, 0.4898, DenseVector(1.2772, 0.760, 0.9907, 1.0788))
    val (alpha, beta, gamma, period, horizon) = (0.4544, 0.0001, 0.0001, 4, 4)
    val forecasts = algo.calculateETS(series, alpha, beta, gamma, "MAM", period , horizon)
    forecasts.length should equal(4)
    forecasts.map(DoubleUtil.round(_, 1)) should equal(DenseVector(58.5, 35.2, 46.4, 51.0))
  }
}


