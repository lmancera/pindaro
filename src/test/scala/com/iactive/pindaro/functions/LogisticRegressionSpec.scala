package com.iactive.pindaro.functions

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

import scala.util.Random

/**
 * @author lmancera
 */
class LogisticRegressionSpec extends FlatSpec {
	"LogisticRegression" should "evaluate 0" in {
        val X = BreezeBuilder zeroMatrix (5,3)
        val y = BreezeBuilder zeroVector 5
        val theta = BreezeBuilder zeroVector 3
        val regressor = new LogisticRegression(X,y)
        val eval = regressor.eval(theta)
		assert(eval-0.693 < 0.001)
	}

    it should "grad is 0 at 0" in {
        val X = BreezeBuilder zeroMatrix (5,3)
        val y = BreezeBuilder zeroVector 5
        val theta = BreezeBuilder zeroVector 3
        val regressor = new LogisticRegression(X,y)
        val grad = regressor.grad(theta)
        assert(grad(0) === 0.0)
        assert(grad(1) === 0.0)
        assert(grad(2) === 0.0)
    }

    it should "make proper predictions" in {
        val X = DenseMatrix((1.,-1.,-1.),(1.,-2.,-3.),(1.,4.,5.),(1.,6.,7.))
        val y = DenseVector(0.,0.,1.,1.)
        val regressor = new LogisticRegression(X,y)
        val theta = DenseVector(1.,1.,1.)
        val predictions = regressor.predict(X,theta)
        for (i <- 0 to y.length-1)
            assert(y(i) === predictions(i))
    }

}