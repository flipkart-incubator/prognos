package com.prognos.forecast

import breeze.linalg.DenseVector
import com.prognos.Series
import breeze.stats.mean

class HoltWinter {
  def calculate(series: Series, alpha: Double, beta: Double, gamma: Double, period:Int, algoType: String, horizon: Int) = {
    if(!"simple".equals(algoType)) throw new IllegalArgumentException("Invalid Holt algoType:" + algoType)
    val data = series.data
    val initialLevel:Double = calcInitialLevel(data, period)
    val initialTrend:Double = calcInitialTrend(data, period, initialLevel)
    val initialSeasonal:DenseVector[Double] = calcInitialSeasonalIndex(data, period, initialLevel)
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

  private def calcInitialLevel(data: DenseVector[Double], period:Int): Double = {
    mean(data(0 until period))
  }

  private def calcInitialTrend(data: DenseVector[Double], period:Int, initialLevel:Double): Double = {
    val meanValue:Double = mean(data(period until (2 * period)))
    (meanValue - initialLevel) / period
  }

  private def calcInitialSeasonalIndex(data: DenseVector[Double], period:Int, initialLevel:Double): DenseVector[Double] = {
    data(0 until period) - initialLevel
  }

  private def calcForecast(level: Double, trend: Double, seasonalIndex: DenseVector[Double], horizon: Int, period:Int): Double = {
    level + horizon * trend + seasonalIndex(-period + (horizon % period - 1))
  }

  private def calcLevel(alpha: Double, value: Double, prevLevel: Double, prevTrend: Double, seasonalIndex: DenseVector[Double], period:Int): Double = {
    alpha * (value - seasonalIndex(-period)) + (1 - alpha) * (prevLevel + prevTrend)
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
