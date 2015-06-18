package com.prognos.forecast

import com.prognos.testdata.CrostonTestData
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by ajay.v on 18/06/15.
 */
class CrostonTest extends FlatSpec with Matchers{

    val obj=new Croston()
    val data = CrostonTestData.data
    val result = CrostonTestData.target

    it must "forecast using croston algorithm" in {
      val dataFloat = Array.fill(data.length) {
        1.000
      }

      for (i <- 0 to data.length - 1) dataFloat(i) = data(i).toFloat

      assert(math.abs(obj.predict(dataFloat,1)-result)<=0.05 *result)
    }
  }
