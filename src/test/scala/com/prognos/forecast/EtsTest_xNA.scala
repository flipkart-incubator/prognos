package com.ets.forecast

/**
 * Created by nikhil.vavs on 11/06/15.
 */

// incomplete

import com.prognos.Series
import breeze.linalg.DenseVector
import com.prognos.util.DoubleUtil
import org.scalatest.{FlatSpec,Matchers}
import com.prognos.testdata.TouristsData

class EtsTest_xNA extends FlatSpec with Matchers {
  it should "testing ANA, MNA" in {
    val series = Series(TouristsData.seasonalRangeData(1999, 2010))
    val algo = new Ets(25.233, 0, DenseVector(1.9832, -0.5609, -6.2767, 4.8545))
    val (alpha, beta, gamma, period, horizon) = (0.511, 0, 0.4153, 4, 4)
    //    Smoothing parameters:
    //      alpha = 0.511
    //    gamma = 0.4153
    //
    //    Initial states:
    //      l = 25.233
    //    s=1.9832, -0.5609, -6.2767, 4.8545

    // change this to MNA for testing MNA
    val forecastsANA = algo.calculateETS(series, alpha, beta, gamma, "ANA", period, horizon)
    forecastsANA.length should equal(4)
    forecastsANA.map{value => DoubleUtil.round(value,1)} should equal(DenseVector(59.2, 35.8,44.6, 47.9))
  }

}
