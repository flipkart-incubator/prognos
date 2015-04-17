package com.prognos.testdata

import breeze.linalg.DenseVector

trait TestDataVector {
  def data:DenseVector[Double]
  def start:Int
  def end:Int

  def rangeData(from:Int, to:Int):DenseVector[Double] = {
    val indexedData:List[(Int,Double)] = (start to end).toList.zip(data.toArray)
    val filteredData:Array[Double] = indexedData.filter{case (index:Int,value:Double) => index >= from && index <= to}.map(_._2).toArray
    DenseVector(filteredData)
  }
}
