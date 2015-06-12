package com.ets.forecast

import breeze.linalg.DenseVector
import com.prognos.Series
import math.pow

/**
 * Created by nikhil.vavs on 04/06/15.
 */

/*
* Usage : val a = new Ets(1.0, 3.6, DenseVector(1.3, 3,2, 2))
*       or val a = new Ets()
* */
class Ets(initialLevel:Double, initialTrend:Double, initialSeasonal:DenseVector[Double], initialValuesPassed:Boolean) {
  // amn, anm, aam, ama, mma, amm models are not allowed

  def this(){
    this(0, 0, DenseVector(0), false)
  }

  def this(x:Double, y:Double, z:DenseVector[Double]){
    this(x, y, z, true)
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

    val (level, trend, seasonalIndices) = data.toArray.foldLeft((initialLevel, initialTrend, initialSeasonalIndices)) {
      case (levelTrendSeasonAndError:(Double, Double, DenseVector[Double]), value:Double) =>
        val (prevLevel, prevTrend, prevSeasonal) = levelTrendSeasonAndError
        //        printSeasonal(prevSeasonal)
        val currError = calcError(value, modelType, prevLevel, prevTrend, prevSeasonal, period)
        val level:Double = calcLevel(alpha, beta, gamma, modelType, prevLevel, prevTrend, prevSeasonal, currError, period)
        val trend:Double = calcTrend(alpha, beta, gamma, modelType, prevLevel, prevTrend, prevSeasonal, currError, period)
        val seasonalIndex:Double = calcSeasonal(alpha, beta, gamma, modelType, prevLevel, prevTrend, prevSeasonal, currError, period)
        (level, trend, concat(prevSeasonal, seasonalIndex))
    }
    //    println("level:"+level)
    //    println("trend:"+trend)
    //    printSeasonal(seasonalIndices)

    val forecasts = (1 to horizon).map(h => calcForecast(level, trend, seasonalIndices, h, period, modelType)).toArray
    DenseVector(forecasts)
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
    if (initialValuesPassed){
      initialLevel
    }
    else{
      // TODO implement initialLevel Calculation
      1.0
    }
  }

  private def calcInitialTrend(data:DenseVector[Double], modelType:String, initialLevel:Double):Double = {
    if (initialValuesPassed){
      initialTrend
    }
    else{
      // TODO implement initialTrend Calculation
      1.0
    }
  }

  private def calcInitialSeasonal(date:DenseVector[Double], modelType:String, initialLevel:Double, initialTrend:Double)
  :DenseVector[Double] =
    if (initialValuesPassed){
      initialSeasonal
    }
    else{
      DenseVector(1.0)
      // TODO implement intialSeasonalIndices Calculation
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
    val halfExp = operByChar(modelType.charAt(1), prevLevel, prevTrend)
    val predicted = operByChar(modelType.charAt(2), halfExp, prevSeasonal(-period))
    if (modelType.charAt(0)=='A'){  // additive error, return the difference
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
    val halfExp = operByChar(modelType.charAt(1), prevLevel, prevTrend) // half of the expresssion
    if (modelType.charAt(0)=='A'){
      // additive type error
      val addedExp = {
        modelType.charAt(2) match {
          case 'M' => (alpha*error)/prevSeasonal(-period)
          case _ => alpha*error
        }
      }
      halfExp + addedExp
    }
    else{ // multiplicative type
    val addedExp = {
      modelType.charAt(2) match {
        case 'A' => alpha*error*prevSeasonal(-period)
        case _ => 0
      }
    }
      halfExp*(1+(alpha*error)) + addedExp
    }
  }

  /*
  * Calculates b_t based on the current values
  * mTrendFactor is the factor with which we have to divide if the trend is multiplicative
  * mSeasonalFactor is the factor with which we have to divide if the seasonal is multiplicative
  * */
  private def calcTrend(alpha:Double, beta:Double, gamma:Double, modelType:String, prevLevel:Double, prevTrend:Double,
                        prevSeasonal:DenseVector[Double], error:Double, period:Int) = {

    if (modelType.charAt(1)=='N'){
      // need this to make sure there are no divideByZeros in case of this function being called with
      // other data
      1.0
    }
    else{
      if (modelType.charAt(0)=='A') {
        val mTrendFactor = modelType.charAt(1) match {
          case 'M' => prevLevel
          case _ => 1
        }
        val mSeasonalFactor = modelType.charAt(2) match {
          case 'M' => prevSeasonal(-period)
          case _ => 1
        }
        // this is the return value l(t-1) + beta*eps_t/(_*_)
        // refer to the website declared above for more details
        prevTrend + beta * error / (mTrendFactor * mSeasonalFactor)
      }
      else if (modelType.charAt(0)=='M'){
        val levelTrend = operByChar(modelType.charAt(1), prevLevel, prevTrend)*(1 + beta*error)
        val sub = modelType.charAt(2) match {
          case 'A' => beta*error*prevSeasonal(-period)
          case _ => 0
        }
        val diffFactor = modelType.charAt(1) match {
          case 'A' => prevLevel
          case _ => 0
        }
        val divFactor = modelType.charAt(1) match {
          case 'M' => prevLevel
          case _ => 1
        }
        val levelTrendWithSeason = levelTrend - sub
        (levelTrendWithSeason - diffFactor)/divFactor
      }
      else{
        throw new IllegalArgumentException("unknown errorType to calculate trend:" + modelType.charAt(0))
      }
    }

  }

  /*
  * Calculates s_t based on the current values
  * */
  private def calcSeasonal(alpha:Double, beta:Double, gamma:Double, modelType:String, prevLevel:Double, prevTrend:Double,
                           prevSeasonal:DenseVector[Double], error:Double, period:Int) = {
    // to ensure no divideByZeros
    if (modelType.charAt(2)=='N'){1.0}
    else if (modelType.charAt(0)=='A'){
      val common = prevSeasonal(-period)
      val divFactor = modelType.charAt(2) match  {
        case 'A' => 1
        case 'M' => operByChar(modelType.charAt(1), prevLevel, prevTrend)
      }
      common + (gamma*error)/divFactor
    }
    else {
      val common = prevSeasonal(-period) * (1 + gamma * error)
      val addedExp = modelType.charAt(2) match {
        case 'A' => operByChar(modelType.charAt(1), prevLevel, prevTrend)
        case 'M' => 0
      }
      common + (gamma * error * addedExp)
    }
  }


  private def valid(x:String):Boolean = {
    if (x.length !=3) return false
    val invalidModels = Set("amn", "anm", "aam", "ama", "mma", "amm")
    if (invalidModels.contains(x.toLowerCase())){
      false
    }
    else{
      x.forall(goodType)
    }
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
    val levelAndTrend = powerByChar(modelType.charAt(1), level, trend, h) // expression containing only level and trend
    val hPlus = (h - 1)%period
    operByChar(modelType.charAt(2), levelAndTrend, seasonalIndices( hPlus -period ))
  }
}
