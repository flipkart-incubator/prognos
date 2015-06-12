package com.ets.forecast

/**
 * Created by nikhil.vavs on 11/06/15.
 */

import com.prognos.Series
import breeze.linalg.DenseVector
import com.prognos.util.DoubleUtil
import org.scalatest.{FlatSpec,Matchers}
import com.prognos.testdata.TouristsData

class EtsTest_xMM extends FlatSpec with Matchers{
  // amm not allowed
  it should "testing MMM" in {
    val series = Series(TouristsData.seasonalRangeData(1999, 2010).map {value => DoubleUtil.round(value)})
    val algo = new Ets(23.5318, 1.014, DenseVector(1.2772, 0.760, 0.9907, 1.07882))
    //    Smoothing parameters:
    //      alpha = 0.4966
    //    beta  = 1e-04
    //    gamma = 1e-04
    //
    //    Initial states:
    //      l = 23.5319
    //    b = 1.014
    //    s=1.2772, 0.760, 0.9907, 1.0788
    val (alpha, beta, gamma, period, horizon) = (0.4966, 0.0001, 0.0001, 4, 4)
    val forecasts_MMN = algo.calculateETS(series, alpha, beta, gamma, "MMM",period, horizon)
    forecasts_MMN.length should equal(4)
    forecasts_MMN.map(DoubleUtil.round(_, 1)) should equal(DenseVector(59.4, 37.1, 46.2, 51.5))
  }

}
