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

/**
 * @author lmancera
 */
class BreezeFacadeSpec extends FlatSpec {
	"BreezeFacadeSpec" should "adjust contrast" in {
        val randVector = DenseVector.rand(200)
        val ajustedVector = BreezeFacade adjustContrast randVector
        assert(ajustedVector.min === 0.0)
        assert(ajustedVector.max === 255.0)
	}

    it should "adjustMatrixContrast" in {
        val randMatrix = DenseMatrix.rand(200,200)
        val adjustedMatrix = BreezeFacade adjustMatrixContrast randMatrix
        assert(adjustedMatrix.min === 0.0)
        assert(adjustedMatrix.max === 255.0)
    }

    it should "reshape a vector" in {
        val m = 400
        val n = sqrt(m).toInt
        val vector = DenseVector.rand(m)
        val matrix = BreezeFacade.reshape(vector,n,n)
        assert(matrix.rows === n)
        assert(matrix.cols === n)
    }

    it should "perform logical comparison of a vector and a scalar" in {
        val vector = DenseVector.zeros[Double](5)
        vector(2) = 4.0
        vector(4) = 5.0
        val scalar = 4.0
        val booleanVector = BreezeFacade whereIsEqual (vector,scalar)
        for (i <- 0 to booleanVector.length-1){
            if (i==2) assert(booleanVector(i) === 1.0, "position: " + i)
            else assert(booleanVector(i) === 0.0, "position: " + i)
        }
    }

    it should "flatten a matrix" in {
        val matrix = DenseMatrix.rand(5,5)
        val vector = BreezeFacade flatten matrix
        assert(vector.length === 25)
    }

    it should "plot a figure" in {
        val n = 20
        BreezeFacade plot (BreezeFacade adjustMatrixContrast DenseMatrix.rand(n,n))
        assert(true === true)
    }

    it should "plot a figure from a matrix row" in {
        val n = 25
        BreezeFacade.plotAt(DenseMatrix.rand(n,n),1)
        assert(true === true)
    }

    it should "be able to build a zero vector" in {
        val n = 10
        val zeros = BreezeFacade zeroVector n
        for (i <- 0 to n-1)
            assert(zeros(i) === 0.0)
    }

    it should "perform product by scalar" in {
        val m = 2
        val n = 2
        val ones = DenseMatrix.ones[Double](m,n)
        val twos = BreezeFacade prod (ones,2.0)
        for (i <- 0 to twos.rows-1)
            for (j <- 0 to twos.cols-1)
                assert(twos(i,j) === 2.0)
    }

    it should "perform addition to scalar" in {
        val m = 2
        val n = 2
        val ones = DenseMatrix.ones[Double](m,n)
        val threes = BreezeFacade add (ones,2.0)
        for (i <- 0 to threes.rows-1)
            for (j <- 0 to threes.cols-1)
                assert(threes(i,j) === 3.0)
    }

    it should "perform difference between vectors" in {
        val v1 = DenseVector(1.0,5.0)
        val v2 = DenseVector(4.0,2.0)
        val v3 = BreezeFacade minus (v1,v2)
        assert(v3(0) === -3.0)
        assert(v3(1) === 3.0)
    }

}