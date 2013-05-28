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

    def slice(scalar:Double): DenseVector[Double] = {
        val howManyLeft = vector.length - this.countOf(scalar)
        if (howManyLeft == vector.length) return vector
        var output = BreezeBuilder zeroVector howManyLeft
        var index = 0
        vector foreach {element =>
            if (element != scalar){
                output(index) = element
                index += 1
            }
        }
        output
    }

    def countOf(scalar: Double): Int = {
        var found = 0
        vector foreach {element => 
            if (element == scalar) found += 1
        }
        found
    }

    def l0norm: Int = {
        val zeros = this.countOf(0.0)
        vector.length - zeros
    }

    def substractFrom(scalar:Double):DenseVector[Double] = {
        var output = BreezeBuilder zeroVector vector.length
        for (i <- 0 to vector.length-1){
            output(i) = scalar - vector(i)
        }
        output
    }

    def t: DenseMatrix[Double] = vector.t
}