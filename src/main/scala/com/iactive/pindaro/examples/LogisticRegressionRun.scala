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
object LogisticRegressionRun extends ParseTrainingDataFromFile 
                                with DatasetModifiable 
                                with Executable {

    override val trainingSetDataFilePath = "assets/data/LogisticRegression.txt"

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

        println("Initializing...")
        println("\t Setup the data matrix")
        val m = X.rows //// used???
        val n = X.cols //// used???
        val X1 = addColumnOfOnes(X,numSamples)
        println("\t Size of X1: " + X1.rows + " rows, " + X1.cols + " cols")
        println("\t First element of X1: (" + X1(0,0) + ", " + X1(0,1) + ", " + X1(0,2) + ")")
        var theta = BreezeBuilder zeroVector X1.cols
        println("\t Init theta: (" + theta(0) + ", " + theta(1) + ", " + theta(2) + ")")

        println("Computing initial cost...")
        //[cost, grad] = costFunction(initial_theta, X, y); for logistic regression!!

    }

}