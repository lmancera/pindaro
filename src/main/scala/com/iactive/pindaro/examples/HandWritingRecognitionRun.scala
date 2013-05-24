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

import org.slf4j.LoggerFactory

import com.iactive.pindaro.classification._
import com.iactive.pindaro.functions._
import com.iactive.pindaro.parsers._
import com.iactive.pindaro.utils._

/**
 * @author lmancera
 */
 // TODO: Use log
object HandWritingRecognitionRun {
    private val log = LoggerFactory.getLogger(this.getClass)

    /**
    * 20x20 input images of handwritten digits
    * 10 labels, from 1 to 10 (output 10 means 0)
    */
    //def main(args: Array[String]) {
        /*val numLabels = 2
        val X = DenseMatrix((0.,0.),(0.,1.),(1.,1.),(1.,0.),(0.5,0.25),(10.,10.),(11.,10.),(10.,11.),(11.,11.),(10.5,10.25))
        val y = DenseVector(0.,0.,0.,0.,0.,1.,1.,1.,1.,1.)
        val theta = OneVsAll(X,y,numLabels).train
        var newInput = DenseVector(0.75,0.5)
        def predicted(v:DenseVector[Double]) = (Sigmoid applyToVector v).argmax
        println(predicted(theta*newInput))
        newInput = DenseVector(10.75,10.5)
        println(predicted(theta*newInput))*/

/*        val numLabels = 10
        val separator = ' '

        println("Loading Data...")

        val trainingSetDataFilePath = "assets/data/HandWrittenDigitsExamples.txt"
        val X = DataFileParser(trainingSetDataFilePath,separator).toDenseMatrix
        val numSamplesInTrainingSet = X.rows
        val inputLayerSize = X.cols
        println("\t Number of Samples in Training Set: " + numSamplesInTrainingSet)
        println("\t Length of each Sample (input layer size): " + inputLayerSize)

        val supervisedResultsDataFilePath = "assets/data/HandWrittenSupervisedResults.txt"
        val y = DataFileParser(supervisedResultsDataFilePath,separator).toDenseVector
        println("\t Number of results should be " + numSamplesInTrainingSet + " and it is: " + y.length)

        println("Visualizing Data... (skipped)")
        BreezeFacade.plotAt(X,1)
        
        println("One-vs-All Logistic Regression...")

        println("\t Training...")
        val allTheta = OneVsAll(X,y,numLabels).train

        println("\t Showing results...")
        println("\t \t parameters learned: " + allTheta(0,0 to 4))

        println("Making predictions...")
        val predictionMatrix = Sigmoid applyToMatrix X*allTheta.t
        var hits = 0
        for (i <- 0 to numSamplesInTrainingSet-1){
            val predictionVector = predictionMatrix(i,::).toDenseVector
            if (y(i).toInt == predictionVector.argmax) hits += 1
        }
        val ratioOfSuccess: Double = 100*hits.toDouble/numSamplesInTrainingSet.toDouble
        println("\t The ratio of success is: " + ratioOfSuccess + "% having " + hits + " hits")
*/        
    //}

}