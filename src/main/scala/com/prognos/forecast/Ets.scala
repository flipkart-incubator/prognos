package com.prognos.forecast

import breeze.linalg.DenseVector
import com.prognos.Series
import math.pow

/**
 * Created by nikhil.vavs on 04/06/15.
 */

/*
* Usage :  val a = new Ets()
* */
class Ets {
  // amn, anm, aam, ama, mma, amm models are not allowed

  var initialLevel = -1.0
  var initialTrend = 1.0
  var initialSeasonal = DenseVector(0.0)
  var sumOfSquaredErrors = 1.0
  var initialValuesPassed = false

  def setInitialValues(x:Double, y:Double, z:DenseVector[Double]) = {
    initialLevel = x
    initialTrend = y
    initialSeasonal = z
    initialValuesPassed = true
  }

  /*
  * Calculates the forecast for the next horizon time units
  * */

  def calculateETS(series: Series, alpha:Double, beta:Double, gamma:Double, modelName:String, period:Int , horizon:Int) = {
    val modelType = modelName.toUpperCase
    if (!valid(modelType)) throw new IllegalArgumentException("Invalid Model Type " + modelType)
    val data  = series.data
    val initialLevel = calcInitialLevel(data, modelType)
    val initialTrend = calcInitialTrend(data, modelType, initialLevel)
    val initialSeasonalIndices:DenseVector[Double] = calcInitialSeasonal(data, modelType, initialLevel, initialTrend)

    val (level, trend, seasonalIndices, sse) = data.toArray.foldLeft((initialLevel, initialTrend, initialSeasonalIndices, 0.0)) {
      case (levelTrendSeasonAndError:(Double, Double, DenseVector[Double], Double), value:Double) =>
        val (prevLevel, prevTrend, prevSeasonal, currSSE) = levelTrendSeasonAndError
        val currError = calcError(value, modelType, prevLevel, prevTrend, prevSeasonal, period)
        val nextSSE = currSSE + currError*currError
        val level:Double = calcLevel(alpha, beta, gamma, modelType, prevLevel, prevTrend, prevSeasonal, currError, period)
        val trend:Double = calcTrend(alpha, beta, gamma, modelType, prevLevel, prevTrend, prevSeasonal, currError, period)
        val seasonalIndex:Double = calcSeasonal(alpha, beta, gamma, modelType, prevLevel, prevTrend, prevSeasonal, currError, period)
        (level, trend, concat(prevSeasonal, seasonalIndex), nextSSE)
    }
    //    println("level:"+level)
    //    println("trend:"+trend)
    //    printSeasonal(seasonalIndices)

    val n = data.length
    val k = 3 - modelType.count( c => c=='N') // number of parameters estimated

    sumOfSquaredErrors = sse
    val aic = n*math.log(sse/n) + 2*k

    val forecasts = (1 to horizon).map(h => calcForecast(level, trend, seasonalIndices, h, period, modelType)).toArray
    (DenseVector(forecasts), aic)
  }

  def getSSE():Double = {
    sumOfSquaredErrors
  }

  //debug functions
  private def printSeasonal(x: DenseVector[Double]) = {
    for (y <- 1 to x.length){
      println("x:"+x(y-1))
    }
    println("#########end########")
  }
  //end of debug functions

  // calculation of initial values

  private def calcInitialLevel(data:DenseVector[Double], modelType:String):Double = {
    (initialValuesPassed, modelType) match {
      case (true, _) => initialLevel
      case (_, model) if Set("ANN", "MNN", "AAN", "MAN").contains(model) => data(0)
      case _ => 1.0
    }
  }

  private def calcInitialTrend(data:DenseVector[Double], modelType:String, initialLevel:Double):Double = {
    (initialValuesPassed, modelType) match {
      case (true, _) => initialTrend
      case (_, model) if Set("AAN", "MAN").contains(model) => data(1) - data(0)
      // TODO implement initialTrend Calculation for other models
      case  _ => 1.0
    }
  }

  private def calcInitialSeasonal(date:DenseVector[Double], modelType:String, initialLevel:Double, initialTrend:Double)
  :DenseVector[Double] =
    initialValuesPassed match {
      case true => initialSeasonal
      // TODO implement initialSeasonalIndices Calculation for models requiring this
      case _ => DenseVector(1.0)
    }

  // sees the character and returns the appropriate result
  private def operByChar(c:Char, x:Double, y:Double):Double = {
    c match {
      case 'A' => x+y
      case 'M' => x*y
      case 'N' => x
      case _ => throw new IllegalArgumentException("Invalid argument of character in operLevelTrend decision")
    }
  }

  // sees the character and returns the appropriate result, used in forecasting
  private def powerByChar(c:Char, x:Double, y:Double, h:Int):Double = {
    c match {
      case 'A' => x+(h*y)
      case 'M' => x*(pow(y, h))
      case 'N' => x
      case _ => throw new IllegalArgumentException("Invalid arguemnt of character in powerByChar")
    }
  }

  // calculation of next values starts here

  /*
  * This function calculates the error in the forecasting
  * */
  private def calcError(value:Double, modelType:String, prevLevel:Double, prevTrend:Double, prevSeasonal:DenseVector[Double], period:Int):Double = {
    val Seq(errorType, trendType, seasonType) = modelType:Seq[Char]
    val levelTrendCombiner = operByChar(trendType, prevLevel, prevTrend)
    val predicted = operByChar(seasonType, levelTrendCombiner, prevSeasonal(-period))
//    println("predicted : " + predicted )
    if (errorType=='A'){  // additive error, return the difference
      value - predicted
    }
    else{
      (value - predicted)/predicted
    }
  }

  /*
  * Calculates l_t based on the current values
  * refer to https://www.otexts.org/sites/default/files/fpp/images/Table7-10.png for more details
  * halfExp is just part of the expression involving prevLevel and prevTrend
  * addedExp is the error expression that is going to be added to this
  * */
  private def calcLevel(alpha:Double, beta:Double, gamma:Double, modelType:String, prevLevel:Double, prevTrend:Double,
                        prevSeasonal:DenseVector[Double], error: Double, period:Int):Double = {

    //    prevLevel*(1 + alpha*error)
    // errorType, trendType, seasonalityType, levelTrendCombiner, levelCalculator
    val Seq(errorType, trendType, seasonType) = modelType:Seq[Char]
    val levelTrendCombiner = operByChar(trendType, prevLevel, prevTrend) // half of the expresssion
    (errorType, seasonType) match {
      case ('A' ,'M') => levelTrendCombiner + (alpha*error)/prevSeasonal(-period)
      case ('A', _) => levelTrendCombiner + (alpha*error)
      case (_, 'A') => levelTrendCombiner*(1+(alpha*error)) + alpha*error*prevSeasonal(-period)
      case (_, _) => levelTrendCombiner*(1+(alpha*error))
    }
  }

  /*
  * Calculates b_t based on the current values
  * mTrendFactor is the factor with which we have to divide if the trend is multiplicative
  * mSeasonalFactor is the factor with which we have to divide if the seasonal is multiplicative
  * */
  private def calcTrend(alpha:Double, beta:Double, gamma:Double, modelType:String, prevLevel:Double, prevTrend:Double,
                        prevSeasonal:DenseVector[Double], error:Double, period:Int) = {
    val Seq(errorType, trendType, seasonType) = modelType:Seq[Char]
    if (trendType=='N'){
      // need this to make sure there are no divideByZeros in case of this function being called with
      // other data
      1.0
    }
    else{
      if (errorType=='A') {
        val mTrendFactor = trendType match {
          case 'M' => prevLevel
          case _ => 1
        }
        val mSeasonalFactor = seasonType match {
          case 'M' => prevSeasonal(-period)
          case _ => 1
        }
        // this is the return value l(t-1) + beta*eps_t/(_*_)
        // refer to the website declared above for more details
        prevTrend + beta * error / (mTrendFactor * mSeasonalFactor)
      }
      else if (errorType=='M'){
        val levelTrend = operByChar(trendType, prevLevel, prevTrend)*(1 + beta*error)
        val seasonalPart = seasonType match {
          case 'A' => beta*error*prevSeasonal(-period)
          case _ => 0
        }
        // difference factor - amount to be subtracted
        val diffFactor = trendType match {
          case 'A' => prevLevel
          case _ => 0
        }
        // division factor - amount to be divided
        val divFactor = trendType match {
          case 'M' => prevLevel
          case _ => 1
        }
        val levelTrendWithSeason = levelTrend - seasonalPart
        (levelTrendWithSeason - diffFactor)/divFactor
      }
      else{
        throw new IllegalArgumentException("unknown errorType to calculate trend:" + errorType)
      }
    }

  }

  /*
  * Calculates s_t based on the current values
  * */
  private def calcSeasonal(alpha:Double, beta:Double, gamma:Double, modelType:String, prevLevel:Double, prevTrend:Double,
                           prevSeasonal:DenseVector[Double], error:Double, period:Int) = {
    // to ensure no divideByZeros
    val Seq(errorType, trendType, seasonType) = modelType:Seq[Char]
    (seasonType, errorType) match {
      case ('N', _) => 1.0
      case ('A', 'A') => prevSeasonal(-period) + (gamma*error)
      case ('M', 'A') => prevSeasonal(-period) + (gamma*error)/operByChar(trendType, prevLevel, prevTrend)
      case ('A', _) => prevSeasonal(-period)*(1 + gamma*error) + (gamma * error*operByChar(trendType, prevLevel, prevTrend))
      case ('M', _) => prevSeasonal(-period)*(1 + gamma*error)
    }
  }


  private def valid(x:String):Boolean = {
    if (x.length !=3) return false
    val invalidModels = Set("amn", "anm", "aam", "ama", "mma", "amm")
    (!invalidModels.contains(x.toLowerCase()) && x.forall(goodType) && x.charAt(0)!='N')
  }

  // return true if the character is among the valid types
  // a - additive
  // m - multiplicative
  // n - none
  private def goodType(c:Char):Boolean = {
    c=='A' || c=='N' || c=='M'
  }

  private def concat(seasonalIndices: DenseVector[Double], seasonalIndex: Double): DenseVector[Double] = {
    //TODO optimize concat in DenseVector
    DenseVector.vertcat(seasonalIndices, DenseVector(seasonalIndex))
  }

  // forecasting
  private def calcForecast(level:Double, trend:Double, seasonalIndices:DenseVector[Double], h:Int, period:Int, modelType:String):Double= {
    // refer to https://www.otexts.org/sites/default/files/fpp/images/Table7-8.png for more details
    val Seq(_, trendType, seasonType) = modelType:Seq[Char]
    val levelAndTrend = powerByChar(trendType, level, trend, h) // expression containing only level and trend
    val hPlus = (h - 1)%period
    operByChar(seasonType, levelAndTrend, seasonalIndices( hPlus -period ))
  }
}
