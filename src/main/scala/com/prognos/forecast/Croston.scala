package com.prognos.forecast

import breeze.linalg.sum

/**
 * Created by ajay.v on 18/06/15.
 */
class Croston {

  /*
* References:
*
* http://www.diku.dk/hjemmesider/ansatte/pisinger/production/forecasting4.pdf
* https://help.sap.com/saphelp_scm70/helpdata/en/ac/216b89337b11d398290000e8a49608/content.htm
* http://www.robjhyndman.com/papers/croston.pdf
* */
  def predict(data:Array[Int],alpha:Double):(Double,Double)={
    predict(data.map{_.toDouble},alpha)
  }
  def predict(data: Array[Double],alpha:Double): (Double,Double) = {
    val estimateDemandVolume: Array[Double] = Array.fill(data.length) {
      1.000
    }
    val estimateTimeInterval: Array[Double] = Array.fill(data.length) {
      1.000
    }
    if (data(0) != 0) {
      estimateDemandVolume(0) = data(1)
      estimateTimeInterval(0) = 1


    }
    else {


      estimateDemandVolume(0) = 1
      estimateTimeInterval(0) = 2
    }
    var q = 0
    for (i <- 1 to data.length - 1) {
      if (data(i) != 0) {
        estimateDemandVolume(i) = (1 - alpha) * estimateDemandVolume(i - 1) + alpha * (data(i) - estimateDemandVolume(i - 1))
        estimateTimeInterval(i) = (1 - alpha) * estimateTimeInterval(i - 1) + alpha * (q - estimateTimeInterval(i - 1))


        //println(estimateDemandVolume(i))
        //println(data(i))
        //println("______________________")
        q = 1
      }
      else {
        estimateDemandVolume(i) = estimateDemandVolume(i - 1)
        estimateTimeInterval(i) = estimateTimeInterval(i - 1)
        q = q + 1
      }
    }
      val forecast = estimateDemandVolume(data.length - 1) / estimateTimeInterval(data.length - 1)
      val meanTT = sum(estimateTimeInterval) / estimateTimeInterval.length
    (forecast,meanTT)
  }
}


