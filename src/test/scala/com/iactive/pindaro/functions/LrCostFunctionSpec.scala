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
class LrCostFunctionSpec extends FlatSpec {
	"LrCostFunctionSpec" should "return -.5 when evaluated at 0s" in {
        val X = DenseMatrix.zeros[Double](5,3)
        val y = DenseVector.zeros[Double](5)
        val theta = DenseVector.zeros[Double](3)
        val lambda = 0
		assert(LrCostFunction(X,y,theta,lambda).eval == -0.5)
	}

    it should "return 0.049 when evaluated at 1s" in {
        val X = DenseMatrix.ones[Double](5,3)
        val y = DenseVector.ones[Double](5)
        val theta = DenseVector.ones[Double](3)
        val lambda = 0
        var cost = LrCostFunction(X,y,theta,lambda).eval
        assert(BigDecimal(cost).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble === 0.049)
    }

	it should "return 0 when finding gradient at 0s" in {
        val X = DenseMatrix.zeros[Double](5,3)
        val y = DenseVector.zeros[Double](5)
        val theta = DenseVector.zeros[Double](3)
        val lambda = 0
        assert(LrCostFunction(X,y,theta,lambda).grad == DenseVector.zeros[Double](3))
	}

    it should "return -0.047425873177566635 when finding gradient at 1s" in {
        val X = DenseMatrix.ones[Double](5,3)
        val y = DenseVector.ones[Double](5)
        val theta = DenseVector.ones[Double](3)
        val lambda = 0
        var grad = LrCostFunction(X,y,theta,lambda).grad
        grad foreach { x => assert(x == -0.047425873177566635)}
    }

    // FIXME: This test is not passing: cost function is sometimes neg!!!
    it should "never be negative" in {
        for (i <- 0 to 100){
            val X = DenseMatrix.rand(6,6)
            val y = DenseVector.zeros[Double](6)
            y(Random.nextInt(4)) = 1.0
            val theta = BreezeFacade flatten DenseMatrix.rand(2,3)
            val lambda = Random.nextDouble * 10
            val eval = LrCostFunction(X,y,theta,lambda).eval
            assert(eval >= 0, "Eval is: " + eval)
        }
    }

}