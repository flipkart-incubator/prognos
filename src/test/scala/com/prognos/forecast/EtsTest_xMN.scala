package com.ets.forecast

/**
 * Created by nikhil.vavs on 11/06/15.
 */

import com.prognos.Series
import breeze.linalg.DenseVector
import com.prognos.util.DoubleUtil
import org.scalatest.{FlatSpec,Matchers}
import com.prognos.testdata.AirPassengerData

class EtsTest_xMN extends FlatSpec with Matchers{
  it should "calculate forecasts using  method with alpha=0.8 and beta=0.2" in {
    val series = Series(AirPassengerData.rangeData(1970, 2009).map {value => DoubleUtil.round(value)})
    val algo = new Ets()
    algo.setInitialValues(7.6358, 1.0433, DenseVector(1.0))
    val (alpha, beta, gamma, period, horizon) = (0.9105, 0.0001, 0, 1, 5)
    //    Smoothing parameters:
    //      alpha = 0.9105
    //    beta  = 1e-04
    //
    //    Initial states:
    //      l = 7.6357
    //    b = 1.0433

    // amn is not allowed, just use mmn
    val forecasts_MMN = algo.calculateETS(series, alpha, beta, gamma, "MMN",period, horizon)
    forecasts_MMN.length should equal(5)
    forecasts_MMN.map(DoubleUtil.round(_, 1)) should equal(DenseVector(52.5, 54.8, 57.2, 59.7, 62.3))
  }

}
