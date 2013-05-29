package com.iactive.pindaro.examples

/*
 Copyright 2013 IActive IT

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

import collection.mutable.Stack
import org.scalatest._

import breeze.linalg._
import breeze.optimize._

import com.iactive.pindaro.functions._
import com.iactive.pindaro.optimize._
import com.iactive.pindaro.utils._

/**
 * @author lmancera
 */
class RegularizedLogisticRegressionRunSpec extends FlatSpec {

    "RegularizedLogisticRegressionRun" should "perform simple logistic regression" in {
        val data = RegularizedLogisticRegressionRun parseData
        var X = RegularizedLogisticRegressionRun getSampleData(data,2)
        val y = RegularizedLogisticRegressionRun getTrainResult(data)
        val numSamples = X.rows
        assert(numSamples === 118)
        assert(numSamples === y.length)
        assert(X(0,0) === 0.051267)
        assert(y(0) === 1.0)
        assert(X(numSamples-1,1) === -0.030612)
        assert(y(numSamples-1) === 0.0)

        val Xfull = RegularizedLogisticRegressionRun mapFeature(X(::,0).toDenseVector, X(::,1).toDenseVector)
        assert(Xfull.rows === X.rows)
        assert(Xfull.cols === 28)
        var theta = BreezeBuilder zeroVector Xfull.cols
        val lambda = 10.0
        val logisticRegressor = new RegularizedLogisticRegression(Xfull,y,lambda)
        var cost = logisticRegressor.eval(theta)
        var grad = logisticRegressor.grad(theta)
        assert(cost === 0.693147180559945)
        assert(grad.length === 28)
        assert(grad(0) === 0.00847457627118644)

        val limmemorybfgs = new LimMemoryBFGS[RegularizedLogisticRegression](400,3)(logisticRegressor)
        theta = limmemorybfgs.minimize(theta)
        assert(theta.length === 28)
        assert(theta(0) === 0.24971632246225423)
        cost = logisticRegressor.eval(theta)
        grad = logisticRegressor.grad(theta)
        assert(cost === 0.645625914124655)
        assert(grad.length === 28)
        assert(grad(0) === -0.014314464369977846)

        val predictedy = logisticRegressor.predict(Xfull,theta)
        assert(RegularizedLogisticRegressionRun.accuracy(y,predictedy) === 74.57627118644068)
    }

}
