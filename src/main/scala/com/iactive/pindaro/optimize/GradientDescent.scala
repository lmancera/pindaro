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

import com.iactive.pindaro.functions.LrCostFunction
import com.iactive.pindaro.utils._

/**
 * @author lmancera
 */
// TODO: Include options to have different theta initialization strategies
case class GradientDescent(X:DenseMatrix[Double], l:DenseVector[Double], initTheta:DenseVector[Double], lambda:Double, alpha:Double, iterations: Integer){

 	def execute: DenseMatrix[Double] = {
 		var theta = initTheta
 		for (i <- 1 to iterations){
 			val f = LrCostFunction(X,l,theta,lambda)
 			theta += f.grad * alpha
 		}
 		var output = DenseMatrix.zeros[Double](1,theta.length)
 		for (j <- 0 to theta.length-1) output(0,j) = theta(j)
 		output
 	}

}

case class  GradientDescentNoReg(X:DenseMatrix[Double], y:DenseVector[Double], initTheta:DenseVector[Double]=DenseVector.zeros[Double](1)){

	val iterations = 1500
	val alpha = 0.01

	def execute: DenseVector[Double] = {
		val iterations = 1500
		val alpha = 0.01
		val initTheta = DenseVector.zeros[Double](2)
		var theta = initTheta
		val m = y.length
		for (i <- 1 to iterations){
			theta -= ((X*theta-y).t*X).t.toDenseVector*(alpha/m)
		}
		theta
	}

	def computeCost(theta:DenseVector[Double]): Double = {
		val m = y.length
		1.toDouble/(2*m)*sum( BreezeFacade pow2 ( BreezeFacade minus (X*theta,y) ) )
	}

}


/*for iter = 1:num_iters

    % ====================== YOUR CODE HERE ======================
    % Instructions: Perform a single gradient step on the parameter vector
    %               theta. 
    %
    % Hint: While debugging, it can be useful to print out the values
    %       of the cost function (computeCost) and gradient here.
    %

    theta = theta-(alpha/m)*((X*theta-y)'*X)';


    % ============================================================

    % Save the cost J in every iteration    
    J_history(iter) = computeCost(X, y, theta);

end 	*/