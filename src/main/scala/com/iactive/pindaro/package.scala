package com.iactive.pindaro

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

/**
 *
 * @author lmancera
 */
package object math {

	def sigmoidScalar(x: Double) = 1.0 / (1.0 + exp(-x))

	def sigmoidVector(v:DenseVector[Double]): DenseVector[Double] = 
    	exp(-v) mapValues {x => 1 / (1 + x) }

	def sigmoidMatrix(A:DenseMatrix[Double]): DenseMatrix[Double] = 
		exp(-A) mapValues {x => 1 / (1 + x) }
  
}

