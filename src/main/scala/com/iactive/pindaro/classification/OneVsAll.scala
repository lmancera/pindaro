package com.iactive.pindaro.classification

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

import com.iactive.pindaro.optimize._
import com.iactive.pindaro.utils._

/**
 * @author lmancera
 */
 // TODO: Include options to have different theta initialization/regularization strategies
 case class OneVsAll(X:DenseMatrix[Double],y:DenseVector[Double],numLabels:Integer){

 	// TODO: uses gradient descent, converto to trainToGradientDescent when more methods added
 	def train: DenseMatrix[Double] = {
 		// Method params
        val alpha = 0.1
        val iterations = 150

        // Regularization params
        val lambda = 0.1

        // Initial classification params
   		val initTheta = DenseVector.zeros[Double](X.cols)

        // Perform training
 		val m = X.rows
 		val n = X.cols
 		var allTheta = DenseMatrix.zeros[Double](1,n)
		for(c <- 0 to numLabels-1){
			val l = BreezeFacade whereIsEqual (y,c)
			val theta = GradientDescent(X, l, initTheta, lambda, alpha, iterations).execute
			if (c==1) allTheta = theta
			else allTheta = DenseMatrix vertcat (allTheta, theta)
		}
		allTheta
 	}

 }