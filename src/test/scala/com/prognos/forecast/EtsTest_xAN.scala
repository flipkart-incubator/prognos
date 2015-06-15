package com.ets.forecast

/**
 * Created by nikhil.vavs on 11/06/15.
 */
import com.prognos.Series
import breeze.linalg.DenseVector
import com.prognos.util.DoubleUtil
import org.scalatest.{FlatSpec,Matchers}
import com.prognos.testdata.AirPassengerData

// tested AAN

class EtsTest_xAN extends FlatSpec with Matchers{
  it should "testing AAN, MAN" in {
    val series = Series(AirPassengerData.rangeData(1970, 2009).map {value => DoubleUtil.round(value)})
    val algo = new Ets(7.3187, 1.0321, DenseVector(1.0))
    val (alpha, beta, gamma, period, horizon) = (0.9999, 0.0001, 0, 1, 5)
    //      Smoothing parameters:
    //    alpha = 0.9999
    //    beta  = 1e-04

    //    Initial states:
    //      l = 6.2557
    //      b = 1.0875

    val forecasts_MAN = algo.calculateETS(series, alpha, beta, gamma, "MAN",period, horizon)
    val forecasts_AAN = algo.calculateETS(series, alpha, beta, gamma, "AAN",period, horizon)
    forecasts_MAN.length should equal(5)
    forecasts_MAN.map(DoubleUtil.round(_, 1)) should equal(DenseVector(51.1, 52.2, 53.3, 54.4, 55.5))
    forecasts_AAN.length should equal(5)
    forecasts_AAN.map(DoubleUtil.round(_, 1)) should equal(DenseVector(51.1, 52.2, 53.3, 54.4, 55.5))
  }

}
