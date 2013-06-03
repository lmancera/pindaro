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

import com.iactive.pindaro.classification._
import com.iactive.pindaro.functions._
import com.iactive.pindaro.parsers._
import com.iactive.pindaro.utils._
import com.iactive.pindaro.math._

/**
 * @author lmancera
 */
object HandWritingRecognitionRun extends ParseTrainingDataFromFile 
                                with DatasetCalculations  
                                with Executable {

    def main(args: Array[String]) {
        val numLabels = 10
        val separator = ' '

        println("Loading Data...")
        val trainingSetDataFilePath = "assets/data/HandWrittenDigitsExamples.txt"
        val X = DataFileParser(trainingSetDataFilePath,separator).toDenseMatrix
        val numSamples = X.rows
        val inputLayerSize = X.cols
        println("\t Number of Samples in Training Set: " + numSamples)
        println("\t Length of each Sample (input layer size): " + inputLayerSize)

        val supervisedResultsDataFilePath = "assets/data/HandWrittenSupervisedResults.txt"
        val y = DataFileParser(supervisedResultsDataFilePath,separator).toDenseVector
        println("\t Number of results should be " + numSamples + " and it is: " + y.length)

        println("Visualizing Data... (skipped)")
        val matrix = DenseMatrixDecorator(X)
        matrix.plotAt(100)

        println("Initializing...")
        println("\t Setup the data matrix")
        val X1 = addColumnOfOnes(X,numSamples)
        println("\t Size of X1: " + X1.rows + " rows, " + X1.cols + " cols")
        println("\t First elements of X1: (" + X1(0,0) + ", " + X1(0,1) + ", " + X1(0,2) + ")")
        
        println("One-vs-All Logistic Regression...")
        println("\t Training...")
        val allTheta = OneVsAll(X1,y,numLabels).train

        println("\t Showing results...")
        println("\t \t parameters learned: " + allTheta(0,0 to numLabels-1))

        println("Making predictions...")
        val predictionMatrix = sigmoidMatrix(X1*allTheta)
        var hits = 0
        for (i <- 0 to numSamples-1){
            val predictionVector = predictionMatrix(i,::).toDenseVector
            if (y(i).toInt == predictionVector.argmax) hits += 1
        }
        val ratioOfSuccess: Double = 100*hits.toDouble/numSamples.toDouble
        println("\t The ratio of success is: " + ratioOfSuccess + "% having " + hits + " hits")

    }

}