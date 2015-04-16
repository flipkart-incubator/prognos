package com.prognos.testdata

import breeze.linalg.DenseMatrix

trait TestDataMatrix {
  def data:DenseMatrix[Double]
}
