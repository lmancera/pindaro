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

import collection.mutable.Stack
import org.scalatest._
import breeze.numerics._

import breeze.linalg._

import com.iactive.pindaro.utils._

/**
 * @author lmancera
 */
class DenseVectorDecoratorSpec extends FlatSpec {
	"DenseVectorDecorator" should "adjust contrast" in {
        val randVector = new DenseVectorDecorator(DenseVector.rand(200))
        val ajustedVector = randVector.adjustContrast.get
        assert(ajustedVector.min === 0.0)
        assert((ajustedVector.max-255.0) < 0.001)
	}

    it should "reshape a vector" in {
        val m = 400
        val n = sqrt(m).toInt
        val vector = new DenseVectorDecorator(DenseVector.rand(m))
        val matrix = vector.reshape(n,n)
        assert(matrix.rows === n)
        assert(matrix.cols === n)
    }

    it should "perform logical comparison of a vector and a scalar" in {
        val vector = new DenseVectorDecorator(DenseVector.zeros[Double](5))
        vector(2) = 4.0
        vector(4) = 5.0
        val scalar = 4.0
        val booleanVector = (vector === scalar).get
        for (i <- 0 to booleanVector.length-1){
            if (i==2) assert(booleanVector(i) === 1.0, "position: " + i)
            else assert(booleanVector(i) === 0.0, "position: " + i)
        }
    }

    it should "be able to build a zero vector" in {
        val n = 10
        val zeros = BreezeBuilder zeroVector n
        for (i <- 0 to n-1)
            assert(zeros(i) === 0.0)
    }

    it should "perform product by scalar" in {
        val n = 2
        val ones = new DenseVectorDecorator(BreezeBuilder oneVector n)
        val twos = ones * 2.0
        for (i <- 0 to twos.length-1)
             assert(twos(i) === 2.0)
    }

    it should "perform addition to scalar" in {
        val n = 2
        val ones = new DenseVectorDecorator(BreezeBuilder oneVector n)
        val threes = ones + 2.0
        for (i <- 0 to threes.length-1)
            assert(threes(i) === 3.0)
    }

    it should "perform difference between vectors" in {
        val v1 = new DenseVectorDecorator(DenseVector(1.0,5.0))
        val v2 = new DenseVectorDecorator(DenseVector(4.0,2.0))
        val v3 = (v1 - v2).get
        assert(v3(0) === -3.0)
        assert(v3(1) === 3.0)
    }

}