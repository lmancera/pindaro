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

import breeze.linalg._
import breeze.numerics._
import breeze.optimize._

import com.iactive.pindaro.utils._
import com.iactive.pindaro.math._


/**
 * @author lmancera
 */

trait LogisticRegressable {

	def h(X: DenseMatrix[Double], theta:DenseVector[Double]): DenseVector[Double] = {
		sigmoid(X*theta)
	}

	def eval(theta:DenseVector[Double]): Double

	def grad(theta:DenseVector[Double]): DenseVector[Double]

	def predict(X: DenseMatrix[Double], theta:DenseVector[Double]): DenseVector[Double] = {
        var predictedoutput = BreezeBuilder zeroVector X.rows
        var index = 0
        h(X,theta) foreach { evaluation =>
            if (evaluation >= 0.5){
                predictedoutput(index) = 1.0
            }
            index += 1
        }
		predictedoutput		
	}

}