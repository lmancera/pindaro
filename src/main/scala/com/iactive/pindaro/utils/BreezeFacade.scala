package com.iactive.pindaro.utils

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
import breeze.plot._
import breeze.numerics._

/**
 * @author lmancera
 */
 object BreezeFacade{

	val figure = Figure()

    def plotAt(matrix:DenseMatrix[Double],row:Int): Unit = {
        val n = sqrt(matrix.cols.toDouble).toInt
        var vectorToShow = matrix(row,::).toDenseVector
        vectorToShow = adjustContrast(vectorToShow)
        val reshaped = reshape(vectorToShow,n,n)
        plot(reshaped)
    }

    def plot(matrix:DenseMatrix[Double]): Unit = {
        figure.subplot(0) += image(matrix)
    }

    def adjustContrast(vector:DenseVector[Double]):DenseVector[Double] = {
        var otherVector = vector - vector.min
        (otherVector/otherVector.max)*255.0
    }

    def reshape(vector:DenseVector[Double],rows:Integer,cols:Integer):DenseMatrix[Double] = {
        var pos = 0
        var matrix = DenseMatrix.zeros[Double](rows,cols)
        for(i <- 0 to rows-1){
            for (j <- 0 to cols-1){
                matrix(i,j) = vector(pos)
                pos += 1
            }
        }
        matrix
    }

    def whereIsEqual(vector:DenseVector[Double], scalar:Double):DenseVector[Double] = {
        var output = DenseVector.zeros[Double](vector.length)
        for (i <- 0 to vector.length-1){
            if (vector(i)==scalar){
                output(i) = 1.0
            }
        }
        output
    }

    def flatten(matrix:DenseMatrix[Double]):DenseVector[Double] = {
        DenseVector(matrix.copy.data)
    }

 }