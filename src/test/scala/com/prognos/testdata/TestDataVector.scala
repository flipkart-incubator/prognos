package com.prognos.testdata

import breeze.linalg.DenseVector

trait TestDataVector {
  def data:DenseVector[Double]
  def start:Int
  def end:Int
}
