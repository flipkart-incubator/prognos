### Prognos

Prognos is a forecasting libray in Scala similar to [R forecast package](http://cran.r-project.org/web/packages/forecast/index.html).
It's implemented using [breeze](https://github.com/scalanlp/breeze) numerical computing library.

Algorithms in this library can be used from Scalding, Spark and other Java/Scala applications.
Check tests, [prognos-scalding-example](https://bitbucket.org/sathish316/prognos-scalding-example) and [prognos-spark-example](https://bitbucket.org/sathish316/prognos-spark-example) for usage examples.

Forecasting algorithms in this library are based on the book [Forecasting Principles and Practice](https://www.otexts.org/fpp) by Rob Hyndman

For using it from a scala application, add this as dependency in build.sbt, after publishing it to local/artifact repository:

```
libraryDependencies += "com.fk" % "prognos_2.10" % "1.0"
```

#### Forecasting algorithms

Available forecasting algorithms:

* Moving Average
* Simple Exponential Smoothing
* Holt Linear and Exponential
* HoltWinters

#### [TODO] Forecasting algorithms

Please send a pull request for other algorithms:

* Holt Damped Linear/Exponential
* Optimization to find alpha, beta, gamma parameters in Holt's method
* ARIMA
* Auto ARIMA

#### How to contribute

1. Add timeseries data from R fpp package or other sources to testdata package
2. Add unit test for validating forecast output
3. Add forecasting algorithm
4. Send a pull request
5. For publishing locally and testing, use $ sbt publish-local