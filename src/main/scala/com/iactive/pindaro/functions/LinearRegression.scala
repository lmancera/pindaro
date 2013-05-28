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

import com.iactive.pindaro.math._

/**
 * @author lmancera
 */
class LinearRegression(X:DenseMatrix[Double],y:DenseVector[Double]){

 	def eval(theta:DenseVector[Double],lambda:Double): Double = {
 		val m = y.length
 		val h_theta = sigmoidVector(X*theta)
 		val regterm = theta(1,theta.length-1).toDenseVector map {x => scala.math.pow(x,2)}
 		val onesvec = DenseVector.ones[Double](y.length)
 		val fidterm = -y.t*log(h_theta) - (onesvec - y).t*log(onesvec - h_theta)
 		val cost = (1/m.toDouble) * (sum(fidterm) + (lambda/2)*sum(regterm))
 		cost
 	}

 	def grad(theta:DenseVector[Double],lambda:Double): DenseVector[Double] = {
 		val m = y.length
 		val invm = 1/m.toDouble
 		val h_theta = sigmoidVector(X*theta)
 		val commonterm = ((h_theta - y).t * X).toDenseVector
 		val noregterm = commonterm * invm
 		val grad = (commonterm + theta*lambda.toDouble) * invm
 		grad(1) = noregterm(1)
 		grad
 	}
}