package com.iactive.pindaro.functions

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

import collection.mutable.Stack
import org.scalatest._

import breeze.linalg._

/**
 * @author lmancera
 */
class SigmoidSpec extends FlatSpec {
	"Sigmoid" should "return 0.5 when applied to a 0" in {
		assert((Sigmoid applyToScalar 0) === 0.5)
	}

	it should "return positive and > 0.5 when applied to a positive number" in {
		assert((Sigmoid applyToScalar 1) > 0)
		assert((Sigmoid applyToScalar 1) > 0.5)
	}

	it should "return postive and < 0.5 when applied to a negative number" in {
		assert((Sigmoid applyToScalar -1) > 0)
		assert((Sigmoid applyToScalar -1) < 0.5)
	}

	it should "return 1 when applied to a very large number" in {
		assert((Sigmoid applyToScalar 100) === 1)
	}

	it should "return 0 when applied to a very large negative number" in {
		assert((Sigmoid applyToScalar -100) < 1E-10)
	}

    it should "return .5 vector when applied to a zero vector" in {
    	assert((Sigmoid applyToVector DenseVector.zeros[Double](5)) == DenseVector(.5,.5,.5,.5,.5))
    }
    
	it should "return .5 matrix when applied to a zero matrix" in {
    	assert((Sigmoid applyToMatrix DenseMatrix.zeros[Double](2,2)) == DenseMatrix((.5,.5),(.5,.5)))
    }

    it should "return positive and > 0.5 when applied to positive vector elements" in {
    	val v = Sigmoid applyToVector DenseVector.ones[Double](5)
    	v foreach { x => assert(x > 0)}
    	v foreach { x => assert(x > 0.5)}
    }

    it should "return positive and < 0.5 when applied to negative vector elements" in {
    	var v = Sigmoid applyToVector -DenseVector.ones[Double](5)
    	v foreach { x => assert(x > 0)}
    	v foreach { x => assert(x < 0.5)}
    }

    it should "return 1 when applied to large positive vector elements" in {
    	var v = Sigmoid applyToVector DenseVector(100,200,300)
    	v foreach { x => assert(x === 1)}
    }

    it should "return 0 when applied to large negative vector elements" in {
    	var v = Sigmoid applyToVector DenseVector(-100,-200,-300)
    	v foreach { x => assert(x < 1E-10)}
    }

}