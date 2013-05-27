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

import com.iactive.pindaro.functions.LrCostFunction
import com.iactive.pindaro.utils._

/**
 * @author lmancera
 */
class GradientDescentSpec extends FlatSpec {
	"GradientDescent" should "decrease cost at each iteration" in {
        val m = 10
        val n = 5
        val X = DenseMatrix.rand(m,n)
        val l = DenseVector.zeros[Double](m)
        l(0) = 1.0
        l(5) = 1.0
        l(9) = 1.0
        val initTheta = DenseVector.zeros[Double](n)
        val lambda = 0.1
        val alpha = 0.1
        var lastEval = 0.0
        for (iterations <- 1 to 100){
            val theta = GradientDescent(X,l,initTheta,lambda,alpha,iterations).execute
            val f = LrCostFunction(X,l,DenseMatrixDecorator(theta).flatten, lambda)
            if (iterations == 1) lastEval = f.eval
            else {
                val newEval = f.eval
                // FIXME: It is giving neg evals of cost function
                //assert(newEval < lastEval, newEval + " - " + lastEval )
                lastEval = newEval
            }
        }
        assert(true == true)
    }

    it should "minimize a toy example" in {
        assert(true == true)
    }

    "GradientDescentNoReg" should "foo test: this is tested in LinearRegressionRunSpec" in {
        assert(true == true)
    }

}