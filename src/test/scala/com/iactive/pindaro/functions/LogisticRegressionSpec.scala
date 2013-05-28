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
        val X = DenseMatrix.zeros[Double](5,3)
        val y = DenseVector.zeros[Double](5)
        val regressor = new LogisticRegression(X,y)
        val theta = DenseVector.zeros[Double](3)
        def eval = regressor.eval(theta)
		assert(eval === 0)
	}

    it should "grad is 0" in {
        val X = DenseMatrix.zeros[Double](5,3)
        val y = DenseVector.zeros[Double](5)
        val regressor = new LogisticRegression(X,y)
        val theta = DenseVector.zeros[Double](3)
        def grad = regressor.grad(theta)
        assert(grad === theta)
    }

}