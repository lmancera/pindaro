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
object LinearRegressionRun extends ParseTrainingDataFromFile 
                                with DatasetCalculations 
                                with Executable {
    
    override val trainingSetDataFilePath = "assets/data/LinearRegression.txt"

    override def main(args: Array[String]) {

        println("Loading Data...")
        val data = parseData
        var x = getVectorSampleData(data,0)
        val y = getTrainResult(data)
        val numSamples = x.length
        println("\t Size of x: " + numSamples)
        println("\t Size of y: " + y.length)
        println("\t First element of x: " + x(0))
        println("\t First element of y: " + y(0))
        println("\t Last element of x: " + x(numSamples-1))
        println("\t Last element of y: " + y(numSamples-1))

        println("Initializing...")
        val X = addColumnOfOnesToVector(x,numSamples)
        println("\t Size of X: " + X.rows + " rows, " + X.cols + " cols")
        println("\t First element of X: (" + X(0,0) + ", " + X(0,1) + ")")
        var theta = BreezeBuilder zeroVector X.cols
        println("\t Init theta: (" + theta(0) + ", " + theta(1) + ")")
        var cost = GradientDescentNoReg(X, y).computeCost(theta)
        println("\t Initial cost: " + cost)

        println("Running Gradient Descent...")
        theta = GradientDescentNoReg(X, y, theta).minimize
        println("\t Final theta: (" + theta(0) + ", " + theta(1) + ")")
        cost = GradientDescentNoReg(X, y).computeCost(theta)
        println("\t Final cost: " + cost)

        println("Running predictions...")
        var predict = DenseVector(1.,3.5).t*theta
        val predict1 = 10000*predict(0)
        println("\t For population 35,000, we predict a profit of " + predict1)
        predict = DenseVector(1.,7).t*theta
        val predict2 = 10000*predict(0)
        println("\t For population 70,000, we predict a profit of " + predict2)

    }

}