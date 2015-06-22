package com.prognos.forecast

import com.prognos.testdata.CrostonTestData
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by ajay.v on 18/06/15.
 */
class CrostonTest extends FlatSpec with Matchers{


    val algo = new Croston()

    it must "forecast using croston algorithm" in {
      CrostonTestData.inputSeriesList.zip(CrostonTestData.targetList).foreach { case (data:Array[Int], expected:Double) =>
        val series = data.map(_.toDouble)
        val prediction = algo.predict(series)
        expected match {
          case 0 => assert(prediction - expected <= 0.5)
          case expected => {

            val error = Math.abs(prediction - expected) / expected
            println(expected)
            //println(error)
            println("_______________")
            assert(error <= 0.09 || Math.abs(prediction - expected) <=15  )
          }
        }
      }


    }
  }
