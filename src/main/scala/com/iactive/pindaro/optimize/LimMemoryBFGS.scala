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

import breeze.linalg._
import breeze.optimize._

import com.iactive.pindaro.functions.LogisticRegression

/**
 * @author lmancera
 */
case class LimMemoryBFGS(regressor: LogisticRegression, iterations: Integer=400, memory:Int=3) {

    private val diff = new DiffFunction[DenseVector[Double]] {
        def calculate(theta: DenseVector[Double]) = {
            (regressor.eval(theta),regressor.grad(theta))
        }
    }

    def minimize(theta:DenseVector[Double]): DenseVector[Double] = {
        val lbfgs = new LBFGS[DenseVector[Double]](iterations, memory)
        lbfgs.minimize(diff, theta)
    }


}