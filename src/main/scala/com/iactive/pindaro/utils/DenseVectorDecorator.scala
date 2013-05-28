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
class DenseVectorDecorator(var vector: DenseVector[Double]) {

    def get:DenseVector[Double] = vector

    def apply(index: Int) = vector(index)

    def update(index: Int, value: Double) = {vector(index) = value}

    def adjustContrast: DenseVectorDecorator = {
        vector -= vector.min
        vector *= 255.0/vector.max
        this
    }

    def reshape(rows:Integer,cols:Integer):DenseMatrix[Double] = {
        var pos = 0
        var matrix = DenseMatrix.zeros[Double](rows,cols)
        for (j <- 0 to cols-1){
            for(i <- 0 to rows-1){
                matrix(i,j) = vector(pos)
                pos += 1
            }
        }
        matrix
    }

    def ===(scalar:Double):DenseVectorDecorator = {
        var output = DenseVector.zeros[Double](vector.length)
        for (i <- 0 to vector.length-1){
            if (vector(i)==scalar){
                output(i) = 1.0
            }
        }
        new DenseVectorDecorator(output)
    }

    def -(otherVector:DenseVectorDecorator):DenseVectorDecorator = {
        var output = DenseVector.zeros[Double](vector.length)
        for (i <- 0 to vector.length-1) output(i) = vector(i) - otherVector(i)
        new DenseVectorDecorator(output)
    }

    def ^(s:Double) = vector map {x => scala.math.pow(x,s)}

    def mean = {
        sum(vector)/vector.length
    }

    def stdv = {
        scala.math.sqrt(sum(new DenseVectorDecorator(vector - this.mean)^2)/(vector.length-1))
    }

    def norm = {
        sqrt(sum(vector.t*vector))
    }

    def normalize: DenseVectorDecorator = {
        new DenseVectorDecorator((vector - this.mean)/this.stdv)
    }
}