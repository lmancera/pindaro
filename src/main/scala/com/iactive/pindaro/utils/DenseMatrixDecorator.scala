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

case class DenseMatrixDecorator(var matrix: DenseMatrix[Double]) {

    val figure = Figure()

    def get:DenseMatrix[Double] = matrix

    def apply(row: Int, column: Int) = matrix(row,column)

    def update(row: Int, col: Int, value: Double) = {matrix(row,col) = value}

    def plotAt(row:Int) = {
        val n = sqrt(matrix.cols.toDouble).toInt
        var vectorToShow = new DenseVectorDecorator(matrix(row,::).toDenseVector)
        new DenseMatrixDecorator(vectorToShow.adjustContrast.reshape(n,n)).plot
    }

    def plot = {
        figure.subplot(0) += image(matrix, GradientPaintScale(matrix.min,matrix.max,PaintScale.BlackToWhite))
    }

    def adjustContrast: DenseMatrixDecorator = {
        var vector = new DenseVectorDecorator(flatten)
        matrix = vector.adjustContrast.reshape(matrix.rows,matrix.cols)
        this
    }

    def ===(scalar:Double):DenseMatrixDecorator = {
        var vector = new DenseVectorDecorator(flatten)
        new DenseMatrixDecorator((vector === scalar).reshape(matrix.rows,matrix.cols))
    }

    def flatten:DenseVector[Double] = {
        DenseVector(matrix.copy.data)
    }

    def ^(s:Double) = matrix map {x => scala.math.pow(x,s)}

    def -(otherMatrix:DenseMatrixDecorator):DenseMatrixDecorator = {
        var vector = new DenseVectorDecorator(flatten)
        var otherVector = new DenseVectorDecorator(otherMatrix.flatten)
        val output = vector - otherVector
        new DenseMatrixDecorator(output.reshape(matrix.rows,matrix.cols))
    }

    def mean: DenseVector[Double] = {
        var meanv = BreezeBuilder zeroVector (matrix.cols)
        for (j <- 0 to matrix.cols-1)
            meanv(j) = (new DenseVectorDecorator(matrix(::,j).toDenseVector)).mean
        meanv
    }

    def stdv: DenseVector[Double] = {
        var sigmav = BreezeBuilder zeroVector (matrix.cols)
        for (j <- 0 to matrix.cols-1){
            sigmav(j) = (new DenseVectorDecorator(matrix(::,j).toDenseVector)).stdv
        }
        sigmav
    }

    def normalize: DenseMatrix[Double] = {
        var normalized = BreezeBuilder zeroMatrix (matrix.rows, matrix.cols)
        for (j <- 0 to matrix.cols-1){
            val normcol = (new DenseVectorDecorator(matrix(::,j).toDenseVector)).normalize
            for (i <- 0 to matrix.rows-1)
                normalized(i,j) = normcol(i)
        }        
        normalized
    }

}

object DenseMatrixDecorator {
    def fromFlatArray(rows:Integer,flat:Array[Double]):DenseMatrix[Double] = {
        val cols = (flat.length/rows).toInt       
        val A = DenseMatrix.zeros[Double](rows, cols)
        var k = 0
        for (i <- 0 to rows-1) {
            for (j <- 0 to cols-1) {
                A(i,j) = flat(k)
                k += 1
            }
        }
        A
    }   
}