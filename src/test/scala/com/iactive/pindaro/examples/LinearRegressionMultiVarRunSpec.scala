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
class LinearRegressionMultipleVarRunSpec extends FlatSpec {

    "LinearRegressionMultipleVarRun" should "perform multi-variant linear regression" in {
        val data = LinearRegressionMultipleVarRun parseData
        var X = LinearRegressionMultipleVarRun getSampleData (data,2)
        val y = LinearRegressionMultipleVarRun getTrainResult data
        val numSamples = X.rows
        assert(numSamples === y.length)

        val Xnorm = new DenseMatrixDecorator(X).normalize
        assert(Xnorm.rows === X.rows)
        val meanXnorm = new DenseMatrixDecorator(Xnorm).mean
        assert(meanXnorm(0) < 0.00001)
        assert(meanXnorm(1) < 0.00001)
        
        val X1 = LinearRegressionMultipleVarRun addColumnOfOnes(Xnorm,numSamples)
        assert(X1.rows === numSamples)
        assert(X1.cols === 3)
        assert(X1(0,0) === 1.0)
        assert(X1(0,1) === Xnorm(0,0))

        var theta = BreezeBuilder zeroVector X1.cols
        assert(theta(0) == 0.0)
        assert(theta(1) == 0.0)
        assert(theta(2) == 0.0)
        val alpha = 0.3
        val iterations = 400
        val initCost = GradientDescentNoReg(X1, y).computeCost(theta)
        assert(initCost > 0, initCost)
        theta = GradientDescentNoReg(X1, y, theta, alpha, iterations).minimize
        val endCost = GradientDescentNoReg(X1, y).computeCost(theta)
        assert(endCost < initCost)

        val mean1 = new DenseVectorDecorator(X(::,0).toDenseVector).mean
        val stdv1 = new DenseVectorDecorator(X(::,0).toDenseVector).stdv
        val mean2 = new DenseVectorDecorator(X(::,1).toDenseVector).mean
        val stdv2 = new DenseVectorDecorator(X(::,1).toDenseVector).stdv
        var predict = DenseVector(1,(1650-mean1)/stdv1,(3-mean2)/stdv2).t*theta
        assert((predict(0)-293081.464335) < 0.00001)
    }

    it should "perform multi-variant linear regression through normal equations" in {
        val data = LinearRegressionMultipleVarRun parseData
        var X = LinearRegressionMultipleVarRun getSampleData (data,2)
        val y = LinearRegressionMultipleVarRun getTrainResult data
        val numSamples = X.rows
        assert(numSamples === y.length)

        val X1 = LinearRegressionMultipleVarRun addColumnOfOnes(X,numSamples)
        assert(X1.rows === numSamples)
        assert(X1.cols === 3)
        assert(X1(0,0) === 1.0)
        assert(X1(0,1) === X(0,0))

        var theta = BreezeBuilder zeroVector X1.cols
        assert(theta(0) == 0.0)
        assert(theta(1) == 0.0)
        assert(theta(2) == 0.0)
        val alpha = 0.3
        val iterations = 400
        val initCost = GradientDescentNoReg(X1, y).computeCost(theta)
        assert(initCost > 0, initCost)
        theta = GradientDescentNoReg(X1, y, theta, alpha, iterations).normalEquations
        val endCost = GradientDescentNoReg(X1, y).computeCost(theta)
        assert(endCost < initCost)

        var predict = DenseVector(1.,1650.,3.).t*theta
        assert((predict(0)-293081.453860) < 0.00001)
    }

}
