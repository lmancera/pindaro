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

/**
 * @author lmancera
 */
object BreezeBuilder {

   def zeroVector(n:Int) = DenseVector.zeros[Double](n)

   def oneVector(n:Int) = DenseVector.ones[Double](n)

   def randVector(n:Int) = DenseVector.rand(n)

   def zeroMatrix(rows:Int, cols:Int) = DenseMatrix.zeros[Double](rows,cols)

   def oneMatrix(rows:Int, cols:Int) = DenseMatrix.ones[Double](rows,cols)

   def randMatrix(rows:Int, cols:Int) = DenseMatrix.rand(rows,cols)

}
