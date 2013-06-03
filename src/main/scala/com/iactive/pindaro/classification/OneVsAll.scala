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

import com.iactive.pindaro.functions._
import com.iactive.pindaro.optimize._
import com.iactive.pindaro.utils._

/**
 * @author lmancera
 */
 case class OneVsAll(X:DenseMatrix[Double],y:DenseVector[Double],numLabels:Integer){

 	def train: DenseMatrix[Double] = {
        val iterations = 400
        val lambda = 1
 		val n = X.cols
 		val initTheta = BreezeBuilder zeroVector (n)
 		var allTheta = BreezeBuilder zeroMatrix (n,numLabels)

		for(c <- 0 to numLabels-1){
			val decoratedy = new DenseVectorDecorator(y)
			var value = c
			if (value == 0) value = 10
			val l = decoratedy === value
			val logisticRegressor = new RegularizedLogisticRegression(X,l.get,lambda)
			val limmemorybfgs = new LimMemoryBFGS[RegularizedLogisticRegression](iterations,3)(logisticRegressor)
			val theta = limmemorybfgs.minimize(initTheta)
			allTheta = copyRow(allTheta,c,theta)
		}
		allTheta
 	}

 	private def copyRow(matrix:DenseMatrix[Double],row:Int,vector:DenseVector[Double]):DenseMatrix[Double] = {
 		var output: DenseMatrix[Double] = matrix;
 		for (j <- 0 to output.cols-1)
 			output(row,j) = vector(j)
 		output
 	}

 }