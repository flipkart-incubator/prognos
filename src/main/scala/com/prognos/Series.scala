package com.prognos

import breeze.linalg.DenseVector

case class Series(val data:DenseVector[Double]) {
  def this(a:Array[Double]) {
    this(DenseVector(a))
  }
}
