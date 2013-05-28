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
trait DatasetModifiable {

    def addColumnOfOnes(x:DenseMatrix[Double],rows:Int) = {
        DenseMatrix.horzcat(BreezeBuilder oneMatrix (rows,1),x)
    }

    def addColumnOfOnesToVector(x:DenseVector[Double],rows:Int) = {
        val decoratedx = new DenseVectorDecorator(x)
        DenseMatrix.horzcat(DenseMatrix.ones[Double](rows,1),decoratedx reshape (rows,1))
    } 

}