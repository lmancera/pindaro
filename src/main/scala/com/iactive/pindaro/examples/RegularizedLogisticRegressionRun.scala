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
import breeze.optimize._

import com.iactive.pindaro.classification._
import com.iactive.pindaro.optimize._
import com.iactive.pindaro.functions._
import com.iactive.pindaro.parsers._
import com.iactive.pindaro.utils._
import com.iactive.pindaro.math._

/**
 * @author lmancera
 */
object RegularizedLogisticRegressionRun extends ParseTrainingDataFromFile 
                                with DatasetCalculations  
                                with Executable {

    override val trainingSetDataFilePath = "assets/data/RegularizedLogisticRegression.txt"

    override def main(args: Array[String]) {

        println("Loading Data...")
        val data = parseData
        val X = getSampleData(data,2)
        val y = getTrainResult(data)
        val numSamples = X.rows
        println("\t Size of X: " + numSamples)
        println("\t Size of y: " + y.length)
        println("\t First element of X: " + X(0,0))
        println("\t First element of y: " + y(0))
        println("\t Last element of X: " + X(numSamples-1,1))
        println("\t Last element of y: " + y(numSamples-1))

        println("Adding more features...")
        val Xfull = mapFeature(X(::,0).toDenseVector, X(::,1).toDenseVector)
        println("\t Dimension of Xfull: " + Xfull.rows + " rows, " + Xfull.cols + " cols")

        println("Initializing...")
        var theta = BreezeBuilder zeroVector Xfull.cols
        println("\t Initial theta: (" + theta(0) + ", " + theta(1) + ", " + theta(2) + ")")
        val lambda = 10.0
        println("\t Lambda: " + lambda)
        println("Computing initial cost...")
        val logisticRegressor = new RegularizedLogisticRegression(Xfull,y,lambda)
        var cost = logisticRegressor.eval(theta)
        var grad = logisticRegressor.grad(theta)
        println("\t Initial cost is: " + cost)
        println("\t Initial grad is: " + grad)

        println("Running Learning Algorithm...")
        val limmemorybfgs = new LimMemoryBFGS[RegularizedLogisticRegression](400,3)(logisticRegressor)
        theta = limmemorybfgs.minimize(theta)
        println("\t Theta at convergence: " + theta)
        cost = logisticRegressor.eval(theta)
        grad = logisticRegressor.grad(theta)
        println("\t Cost at theta: " + cost)
        println("\t Gradient at theta: " + grad)

        println("Making predictions...")
        val predictedy = logisticRegressor.predict(Xfull,theta)
        println("\t Train Accuracy: " + accuracy(y,predictedy))

    }

}