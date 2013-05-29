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

import com.iactive.pindaro.utils._
import com.iactive.pindaro.optimize._

/**
 * @author lmancera
 */
class LinearRegressionRunSpec extends FlatSpec {

    "LinearRegressionRun" should "perform simple linear regression" in {
        val data = LinearRegressionRun parseData
        var x = LinearRegressionRun getVectorSampleData (data,0)
        val y = LinearRegressionRun getTrainResult (data)
        val numSamples = x.length
        assert(numSamples === y.length)

        val X = LinearRegressionRun addColumnOfOnesToVector(x,numSamples)
        assert(X.rows === numSamples)
        assert(X.cols === 2)
        assert(X(0,0) === 1.0)
        assert(X(0,1) === x(0))

        var theta = BreezeBuilder zeroVector X.cols
        assert(theta(0) == 0.0)
        assert(theta(1) == 0.0)

        val initCost = GradientDescentNoReg(X, y).computeCost(theta)
        assert(initCost > 0, initCost)

        theta = GradientDescentNoReg(X, y, theta).minimize
        val endCost = GradientDescentNoReg(X, y).computeCost(theta)
		assert(endCost < initCost)

        var predict = DenseVector(1.,3.5).t*theta
        val predict1 = 10000*predict(0)
        assert(predict1 === 4519.7678677017675)
		predict = DenseVector(1.,7).t*theta
        val predict2 = 10000*predict(0)
        assert(predict2 === 45342.45012944714)
       
    }

}