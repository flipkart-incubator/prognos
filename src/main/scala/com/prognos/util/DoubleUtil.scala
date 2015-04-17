package com.prognos.util

object DoubleUtil {
  val DefaultScale = 2

  def round(value:Double, scale:Int):Double = {
    BigDecimal(value).setScale(scale, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  def round(value:Double):Double = round(value, DefaultScale)
}
