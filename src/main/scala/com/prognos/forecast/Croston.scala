package com.prognos.forecast

/**
 * Created by ajay.v on 18/06/15.
 */
class Croston {
  def predict(data:Array[Double],t:Double):Double={

    val estimateDemandVolume:Array[Double]= Array.fill(data.length) {
      1.000
    }
    val estimateTimeInterval:Array[Double]= Array.fill(data.length) {
      1.000
    }

    val alpha = 0.1
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


    val answer = estimateDemandVolume(data.length - 1) / estimateTimeInterval(data.length - 1)
    println(answer)


    return answer

  }
}
