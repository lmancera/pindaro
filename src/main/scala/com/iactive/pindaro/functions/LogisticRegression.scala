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

/**
 * @author lmancera
 */

class LogisticRegression(X:DenseMatrix[Double], y:DenseVector[Double]) 
	extends LogisticRegressable {

	private val m = y.length

	private val decoratedy = new DenseVectorDecorator(y)

	override def eval(theta:DenseVector[Double]): Double = {
		val decoratedhtheta = new DenseVectorDecorator(h(X,theta))
		sum(-y.t*log(h(X,theta)) - (decoratedy.substractFrom(1).t*log(decoratedhtheta.substractFrom(1))))/m
	}

	override def grad(theta:DenseVector[Double]): DenseVector[Double] = {
		val decoratedhtheta = new DenseVectorDecorator(h(X,theta))
		((decoratedhtheta-decoratedy).t*X).t.toDenseVector * (1./m)
	}

}