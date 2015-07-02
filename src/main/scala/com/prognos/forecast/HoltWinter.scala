package com.prognos.forecast

import breeze.linalg.DenseVector
import com.prognos.Series
import breeze.stats.mean

class HoltWinter {
  def calculate(series: Series, alpha: Double, beta: Double, gamma: Double = 0.0,
                nonSeasonal:Boolean, period:Int = 1, algoType: String, horizon: Int) = {
    if(!"simple".equals(algoType)) throw new IllegalArgumentException("Invalid HoltWinter algoType:" + algoType)
    if (nonSeasonal && (period!=1 || gamma!=0.0)){
      throw new IllegalArgumentException("dude!! with non-seasonal=true, gamma has to zero and period has to be 1")
    }
    val dataForInit = series.data
    val initialLevel:Double = calcInitialLevel(dataForInit, period, nonSeasonal)
    val initialTrend:Double = calcInitialTrend(dataForInit, period, initialLevel, nonSeasonal)
    val initialSeasonal:DenseVector[Double] = calcInitialSeasonalIndex(dataForInit, period, initialLevel, nonSeasonal)
    val startTime = 3
    val data = if (nonSeasonal) dataForInit.slice(startTime-1, dataForInit.length) else dataForInit

    val (level, trend, seasonalIndices) = data.toArray.foldLeft((initialLevel, initialTrend, initialSeasonal)) {
      case (levelTrendAndSeason:(Double,Double,DenseVector[Double]), value:Double) =>
        val (prevLevel, prevTrend, seasonalIndices) = levelTrendAndSeason
        val level:Double = calcLevel(alpha, value, prevLevel, prevTrend, seasonalIndices, period)
        val trend:Double = calcTrend(beta, prevLevel, prevTrend, level)
        val seasonalIndex:Double = calcSeasonalIndex(gamma, value, prevLevel, prevTrend, seasonalIndices, period)
        //val fitted = level + trend + seasonalIndices(-period)
        //println((value, level, trend, seasonalIndex, fitted))
        (level, trend, concat(seasonalIndices, seasonalIndex))
      }
    val forecasts = (1 to horizon).map {h => calcForecast(level, trend, seasonalIndices, h, period)}.toArray

    DenseVector(forecasts)
  }

  private def calcInitialLevel(data: DenseVector[Double], period:Int, nonSeasonal:Boolean): Double = {
    nonSeasonal match {
      case true => data(1)
      case false => data(0)
    }
  }

  private def calcInitialTrend(data: DenseVector[Double], period:Int, initialLevel:Double, nonSeasonal:Boolean): Double = {
    nonSeasonal match {
      case true => data(1) - data(0)
      case false => val meanValue:Double = mean(data(period until (2 * period)))
                    (meanValue - initialLevel) / period
    }
  }

  private def calcInitialSeasonalIndex(data: DenseVector[Double], period:Int, initialLevel:Double, nonSeasonal:Boolean): DenseVector[Double] = {
    if (nonSeasonal){
      DenseVector(0.0)
    }
    else{
      data(0 until period) - initialLevel
    }
  }

  private def calcForecast(level: Double, trend: Double, seasonalIndex: DenseVector[Double], horizon: Int, period:Int): Double = {
    level + horizon * trend + seasonalIndex(-period + (horizon % period - 1))
  }

  private def calcLevel(alpha: Double, value: Double, prevLevel: Double, prevTrend: Double, seasonalIndex: DenseVector[Double], period:Int): Double = {

    val level = alpha * (value - seasonalIndex(-period)) + (1 - alpha) * (prevLevel + prevTrend)
    level
  }

  private def calcTrend(beta: Double, prevLevel: Double, prevTrend: Double, level: Double): Double = {
    beta * (level - prevLevel) + (1 - beta) * prevTrend
  }

  private def calcSeasonalIndex(gamma: Double, value: Double, prevLevel: Double, prevTrend: Double, seasonalIndex: DenseVector[Double], period:Int): Double = {
    gamma * (value - prevLevel - prevTrend) + (1 - gamma) * seasonalIndex(-period)
  }

  private def concat(seasonalIndices: DenseVector[Double], seasonalIndex: Double): DenseVector[Double] = {
    //TODO optimize concat in DenseVector
    DenseVector.vertcat(seasonalIndices, DenseVector(seasonalIndex))
  }
}
