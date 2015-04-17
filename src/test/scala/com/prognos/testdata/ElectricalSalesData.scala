package com.prognos.testdata

import breeze.linalg.DenseVector

object ElectricalSalesData extends TestDataVector {
  val start = 0
  val end = data.length - 1
  val data:DenseVector[Double] = DenseVector(2354.34,2379.71,2318.52,2468.99,2386.09,2569.47,2575.72,2762.72,2844.50,3000.70,
    3108.10,3357.50,3075.70,3180.60,3221.60,3176.20,3430.60,3527.48,3637.89,3655.0)
  val expected = DenseVector(2381.53,2424.56,2463.76,2552.60,2627.70,2750.62,2858.35,3014.70,3077.30,3144.52,
    3188.70,3202.32,3216.94,3307.30,3398.75,3485.43)
}
