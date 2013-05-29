package com.iactive.pindaro.examples

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

import com.iactive.pindaro.classification._
import com.iactive.pindaro.optimize._
import com.iactive.pindaro.functions._
import com.iactive.pindaro.parsers._
import com.iactive.pindaro.utils._

/**
 * @author lmancera
 */
 // TODO: Use log
object LinearRegressionMultipleVarRun extends ParseTrainingDataFromFile 
                                        with DatasetCalculations
                                        with Executable {

    override val trainingSetDataFilePath = "assets/data/LinearRegressionMultipleVar.txt"

    override def main(args: Array[String]) {

        println("Loading Data...")
        val data = parseData
        var X = getSampleData(data,2)
        val y = getTrainResult(data)
        val numSamples = X.rows
        println("\t Size of X: " + numSamples)
        println("\t Size of y: " + y.length)
        println("\t First element of X: " + X(0,0))
        println("\t First element of y: " + y(0))
        println("\t Last element of X: " + X(numSamples-1,1))
        println("\t Last element of y: " + y(numSamples-1))

        println("Performing feature normalization...")
        val Xnorm = new DenseMatrixDecorator(X).normalize
        println("\t Size of Xnorm: " + numSamples)
        println("\t First element of Xnorm: " + Xnorm(0,0))
        println("\t Last element of Xnorm: " + Xnorm(numSamples-1,1))
        println("\t Mean of Xnorm: " + (new DenseMatrixDecorator(Xnorm)).mean)

        println("Initializing...")
        val X1 = addColumnOfOnes(Xnorm,numSamples)
        println("\t Size of X1: " + X1.rows + " rows, " + X1.cols + " cols")
        println("\t First element of X1: (" + X1(0,0) + ", " + X1(0,1) + ", " + X1(0,2) + ")")

        println("Running Gradient Descent...")
        var theta = BreezeBuilder zeroVector X1.cols
        println("\t Init theta: (" + theta(0) + ", " + theta(1) + ", " + theta(2) + ")")
        val alpha = 0.3
        println("\t Alpha: " + alpha)
        val iterations = 400
        println("\t Num. iterations: " + iterations)
        theta = GradientDescentNoReg(X1, y, theta, alpha, iterations).minimize
        println("\t Final theta: (" + theta(0) + ", " + theta(1) + ", " + theta(2) + ")")
        var cost = GradientDescentNoReg(X1, y).computeCost(theta)
        println("\t Final cost: " + cost)

        println("Running predictions...")
        val mean1 = new DenseVectorDecorator(X(::,0).toDenseVector).mean
        val stdv1 = new DenseVectorDecorator(X(::,0).toDenseVector).stdv
        val mean2 = new DenseVectorDecorator(X(::,1).toDenseVector).mean
        val stdv2 = new DenseVectorDecorator(X(::,1).toDenseVector).stdv
        var predict = DenseVector(1,(1650-mean1)/stdv1,(3-mean2)/stdv2).t*theta
        println("Predicted price of a 1650 sq-ft, 3 br house: " + predict(0))

        println("Running Gradient Descent with normal equations...")
        val X2 = addColumnOfOnes(X,numSamples)
        theta = BreezeBuilder zeroVector X2.cols
        theta = GradientDescentNoReg(X2, y, theta, alpha, iterations).normalEquations
        println("\t Final theta: (" + theta(0) + ", " + theta(1) + ", " + theta(2) + ")")
        cost = GradientDescentNoReg(X2, y).computeCost(theta)
        println("\t Final cost: " + cost)

        println("Running predictions after training using normal equations...")
        predict = DenseVector(1.,1650.,3.).t*theta
        println("Predicted price of a 1650 sq-ft, 3 br house: " + predict(0))
    }

}