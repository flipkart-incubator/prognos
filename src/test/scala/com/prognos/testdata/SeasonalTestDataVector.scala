package com.prognos.testdata

import breeze.linalg.DenseVector

trait SeasonalTestDataVector extends TestDataVector {
  def period:Int

  def seasonalRangeData(from:Int, to:Int):DenseVector[Double] = {
    val indices:List[(Int,Int)] = (for {
      i <- start to end
      p <- 1 to period
    } yield (i,p)).toList
    val filteredData:Array[Double] = indices.toList.zip(data.toArray).filter {case ((index, period),value) => index >= from && index <= to}.map(_._2).toArray
    DenseVector(filteredData)
  }
}
