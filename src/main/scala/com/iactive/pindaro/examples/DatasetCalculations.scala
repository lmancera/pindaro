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

import com.iactive.pindaro.utils._

/**
 * @author lmancera
 */
trait DatasetCalculations {

    def addColumnOfOnes(x:DenseMatrix[Double],rows:Int) = {
        DenseMatrix.horzcat(BreezeBuilder oneMatrix (rows,1),x)
    }

    def addColumnOfOnesToVector(x:DenseVector[Double],rows:Int) = {
        val decoratedx = new DenseVectorDecorator(x)
        DenseMatrix.horzcat(DenseMatrix.ones[Double](rows,1),decoratedx reshape (rows,1))
    } 

    def accuracy(y:DenseVector[Double], predy: DenseVector[Double]): Double = {
        var wellPredicted = 0.0
        for (i <- 0 to y.length-1)
            if (y(i) == predy(i))
                wellPredicted += 1
        100*wellPredicted./(y.length)
    }

    def mapFeature(x1:DenseVector[Double],x2:DenseVector[Double]): DenseMatrix[Double] = {
        val degree = 6
        var output = BreezeBuilder oneMatrix (x1.length,1)
        val decoratedx1 = new DenseVectorDecorator(x1)
        val decoratedx2 = new DenseVectorDecorator(x2)
        for(i <- 1 to degree){
            for(j <- 0 to i){
                val featureCol = (decoratedx1^(i-j)) :* (decoratedx2^j)
                output = DenseMatrix.horzcat(output,featureCol.t.t)
            }            
        }
        output
    }    

}