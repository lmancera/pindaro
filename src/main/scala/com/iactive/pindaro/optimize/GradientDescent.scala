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
import breeze.numerics._

import com.iactive.pindaro.functions.LinearRegression
import com.iactive.pindaro.utils._

/**
 * @author lmancera
 */
// TODO: Include options to have different theta initialization strategies
case class GradientDescent(X:DenseMatrix[Double], l:DenseVector[Double], initTheta:DenseVector[Double], lambda:Double, alpha:Double, iterations: Integer){

 	def minimize: DenseMatrix[Double] = {
 		var theta = initTheta
 		for (i <- 1 to iterations){
 			val linearRegressor = new LinearRegression(X,l)
 			theta -= linearRegressor.grad(theta,lambda) * alpha
 		}
 		var output = DenseMatrix.zeros[Double](1,theta.length)
 		for (j <- 0 to theta.length-1) output(0,j) = theta(j)
 		output
 	}

}

case class  GradientDescentNoReg(X:DenseMatrix[Double], y:DenseVector[Double], initTheta:DenseVector[Double]=DenseVector.zeros[Double](1), alpha:Double=0.01, iterations:Int=1500){

	def minimize: DenseVector[Double] = {
		var theta = initTheta
		val m = y.length
		for (i <- 1 to iterations){
			theta -= ((X*theta-y).t*X).t.toDenseVector*(alpha/m)
		}
		theta
	}

	def normalEquations: DenseVector[Double] = {
		pinv(X.t*X)*X.t*y
	}

	def computeCost(theta:DenseVector[Double]): Double = {
		val m = y.length
		val decoratedxtheta = new DenseVectorDecorator(X*theta)
		val decoratedy = new DenseVectorDecorator(y)
		1.toDouble/(2*m)*sum((decoratedxtheta - decoratedy)^2)
	}

}