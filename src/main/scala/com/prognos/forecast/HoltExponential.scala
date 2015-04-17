package com.prognos.forecast

import breeze.linalg.DenseVector
import com.prognos.Series

class HoltExponential {
   def calculate(series: Series, alpha: Double, beta: Double, algoType: String, horizon: Int) = {
     if(!"simple".equals(algoType)) throw new IllegalArgumentException("Invalid Holt algoType:" + algoType)
     val data = series.data
     val initialLevel = data(0)
     val initialTrend = data(1) / data(0)
     val (level, trend) = data.toArray.foldLeft((initialLevel, initialTrend)) {case (levelAndTrend:(Double,Double), value:Double) =>
       val (prevLevel, prevTrend) = levelAndTrend
       println((prevLevel, prevTrend))
       val level:Double = calcLevel(alpha, value, prevLevel, prevTrend)
       val trend:Double = calcTrend(beta, prevLevel, prevTrend, level)
       (level, trend)
     }
     val forecasts = (1 to horizon).map {h => calcForecast(level, trend, h)}.toArray
     DenseVector(forecasts)
   }

   private def calcForecast(level: Double, trend: Double, horizon: Int): Double = {
     level * Math.pow(trend, horizon)
   }

   private def calcLevel(alpha: Double, value: Double, prevLevel: Double, prevTrend: Double): Double = {
     alpha * value + (1 - alpha) * (prevLevel * prevTrend)
   }

   private def calcTrend(beta: Double, prevLevel: Double, prevTrend: Double, level: Double): Double = {
     beta * (level / prevLevel) + (1 - beta) * prevTrend
   }
 }
