package com.iactive.pindaro.optimize

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

import com.iactive.pindaro.functions.LinearRegression
import com.iactive.pindaro.utils._

/**
 * @author lmancera
 */
class GradientDescentSpec extends FlatSpec {
	"GradientDescent" should "decrease cost at each iteration" in {
        val m = 5
        val n = 3
        val X = BreezeBuilder zeroMatrix (m,n)
        X(0,1) = 5
        X(0,2) = 6
        X(1,1) = 4
        X(1,2) = 5
        X(2,1) = 6
        X(2,2) = 4
        X(3,1) = 28
        X(3,2) = 27
        X(4,1) = 33
        X(4,2) = 30
        X(0,0) = 1
        X(1,0) = 1
        X(2,0) = 1
        X(3,0) = 1
        X(4,0) = 1
        val l = BreezeBuilder zeroVector m
        l(0) = 1.0
        l(1) = 1.0
        l(2) = 1.0
        val initTheta = BreezeBuilder zeroVector n
        val lambda = 0.1
        val alpha = 0.001
        val linearRegressor = new LinearRegression(X,l)
        var lastEval = linearRegressor eval (initTheta, lambda)
        for (iterations <- 1 to 100){
            val theta = GradientDescent(X,l,initTheta,lambda,alpha,iterations).minimize
            val flatTheta = DenseMatrixDecorator(theta).flatten
            val newEval = linearRegressor eval (flatTheta, lambda)
            assert(newEval < lastEval, newEval + " - " + lastEval )
            lastEval = newEval            
        }
    }

    "GradientDescentNoReg" should "foo test: this is tested in LinearRegressionRunSpec" in {
        assert(true == true)
    }

}