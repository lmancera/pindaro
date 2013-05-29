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
class LogisticRegressionRunSpec extends FlatSpec {

    "LogisticRegressionRun" should "perform simple logistic regression" in {
        val data = LogisticRegressionRun parseData
        var X = LogisticRegressionRun getSampleData(data,2)
        val y = LogisticRegressionRun getTrainResult(data)
        val numSamples = X.rows
        assert(numSamples === 100)
        assert(numSamples === y.length)
        assert(X(0,0) === 34.62365962451697)
        assert(y(0) === 0.0)
        assert(X(numSamples-1,1) === 89.52981289513276)
        assert(y(numSamples-1) === 1.0)

        val X1 = LogisticRegressionRun addColumnOfOnes(X,numSamples)
        assert (X1.cols === 3)
        var theta = BreezeBuilder zeroVector X1.cols

        val logisticRegressor = new LogisticRegression(X1,y)
        var cost = logisticRegressor.eval(theta)
        var grad = logisticRegressor.grad(theta)
        assert(cost === 0.693147180559945)
        assert(grad === DenseVector(-0.1, -12.00921658929115, -11.262842205513591))

        theta = LimMemoryBFGS(logisticRegressor,400,3).minimize(theta)
        assert(theta === DenseVector(-25.161403375352286, 0.20623229961295794, 0.20147216053313552))
        cost = logisticRegressor.eval(theta)
        grad = logisticRegressor.grad(theta)
        assert(cost === 0.20349770159024108)
        assert(grad(0) < 0.0001)
        assert(grad(1) < 0.0001)
        assert(grad(2) < 0.0001)

        val test = DenseVector(1., 45., 85.)
        val pred = logisticRegressor.predict(test.t,theta)
        assert(pred(0) === 1.0)
        
        val predictedy = logisticRegressor.predict(X1,theta)
        assert(LogisticRegressionRun.accuracy(y,predictedy) === 89.0)
    }

}
