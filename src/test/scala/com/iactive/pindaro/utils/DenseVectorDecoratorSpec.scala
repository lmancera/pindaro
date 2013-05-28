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
        val ones = BreezeBuilder oneVector n
        val twos = ones * 2.0
        for (i <- 0 to twos.length-1)
             assert(twos(i) === 2.0)
    }

    it should "perform addition to scalar" in {
        val n = 2
        val ones = BreezeBuilder oneVector n
        val threes = ones + 2.0
        for (i <- 0 to threes.length-1)
            assert(threes(i) === 3.0)
    }

    it should "power a vector" in {
        val n = 2
        val ones = BreezeBuilder oneVector n
        val twos = ones * 2.0
        val fours = new DenseVectorDecorator(twos)^2.0
        for (i <- 0 to fours.length-1)
            assert(fours(i) === 4.0)
    }

    it should "perform difference between vectors" in {
        val v1 = new DenseVectorDecorator(DenseVector(1.0,5.0))
        val v2 = new DenseVectorDecorator(DenseVector(4.0,2.0))
        val v3 = (v1 - v2).get
        assert(v3(0) === -3.0)
        assert(v3(1) === 3.0)
    }

    it should "calculate vector euclidean norm" in {
        val v1 = new DenseVectorDecorator(DenseVector(0.25,0.25,0.25,0.25))
        assert (v1.norm === 0.5)
        val v2 = new DenseVectorDecorator(DenseVector(1,1,1,1))
        assert (v2.norm === 2)
    }

    it should "perform feature normalization" in {
        val v1 = new DenseVectorDecorator(DenseVector(1,2))
        val normalized = v1.normalize
        assert (normalized(0) + 0.70710 < 0.0001)
        assert (normalized(1) - 0.70710 < 0.0001)
    }

    it should "count how many like some scalar" in {
        val vector = BreezeBuilder zeroVector (10)
        val decorated = new DenseVectorDecorator(vector)
        assert(decorated.countOf(0.0) === 10)
        decorated(1) = -1
        decorated(3) = 2
        decorated(4) = 2
        assert(decorated.countOf(0.0) === 7)
        assert(decorated.countOf(-1.0) === 1)
        assert(decorated.countOf(2.0) === 2)
    }

    it should "calculate l0 norm" in {
        val vector = BreezeBuilder zeroVector (10)
        val decorated = new DenseVectorDecorator(vector)
        assert(decorated.l0norm === 0)
        decorated(1) = 1
        decorated(3) = 2
        decorated(4) = -1
        assert(decorated.l0norm === 3)
    }

    it should "slice a vector" in {
        val vector = BreezeBuilder zeroVector 10
        val decorated = new DenseVectorDecorator(vector)
        decorated(0) = 2
        decorated(1) = 1
        decorated(3) = 2
        decorated(4) = -1
        decorated(6) = 1
        decorated(8) = 2
        assert(decorated.countOf(0.0) == 4)
        assert(vector.length - decorated.countOf(0.0) == 6)
        assert(decorated.slice(-1.0) === DenseVector(2,1,0,2,0,1,0,2,0))
        assert(decorated.slice(0.0) === DenseVector(2,1,2,-1,1,2))
        assert(decorated.slice(1.0) === DenseVector(2,0,2,-1,0,0,2,0))
        assert(decorated.slice(2.0) === DenseVector(1,0,-1,0,1,0,0))
    }

    it should "substract a vector from a scalar" in {
        val scalar = 1
        val vector = DenseVector(-1,0,0.25,0.5,0.75,1)
        val decorated = new DenseVectorDecorator(vector)
        val output = decorated.substractFrom(scalar)
        assert(output(0) === 2)
        assert(output(1) === 1)
        assert(output(2) === 0.75)
        assert(output(3) === 0.5)
        assert(output(4) === 0.25)
        assert(output(5) === 0)
    }

    it should "calculate the transpose of a vector" in {
        val vector = DenseVector(-1,0,0.25)
        val decorated = new DenseVectorDecorator(vector)
        val transpose = decorated.t
        assert(transpose === vector.t)
    }

}