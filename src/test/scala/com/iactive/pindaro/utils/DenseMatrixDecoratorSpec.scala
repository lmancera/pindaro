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
class DenseMatrixDecoratorSpec extends FlatSpec {
	"DenseMatrixDecorator" should "adjust contrast" in {
        val rand = new DenseMatrixDecorator(DenseMatrix.rand(200,200))
        val ajusted = rand.adjustContrast.get
        assert(ajusted.min === 0.0)
        assert(ajusted.max === 255.0)
	}

    it should "perform logical comparison of a matrix and a scalar" in {
        val m = 10
        val n = 10
        val matrix = new DenseMatrixDecorator(BreezeBuilder zeroMatrix (m,n))
        matrix(2,0) = 4.0
        matrix(4,6) = 5.0
        val scalar = 4.0
        val booleanMatrix = (matrix === scalar).get
        for (i <- 0 to booleanMatrix.rows-1){
            for (j <- 0 to booleanMatrix.cols-1){
                if ((i==2) && (j==0)) assert(booleanMatrix(i,j) === 1.0, "position: " + i)
                else assert(booleanMatrix(i,j) === 0.0, "position: (" + i + ", " + j + ")")
            }  
        }
    }

    it should "flatten a matrix" in {
        val matrix = new DenseMatrixDecorator(DenseMatrix.rand(5,5))
        val vector = matrix.flatten
        assert(vector.length === 25)
    }

    it should "plot a figure" in {
        val n = 20
        val matrix = new DenseMatrixDecorator(DenseMatrix.rand(n,n))
        matrix.adjustContrast.plot
        assert(true === true)
    }

    it should "plot a figure from a matrix row" in {
        val n = 25
        val matrix = new DenseMatrixDecorator(DenseMatrix.rand(n,n))
        matrix.adjustContrast.plotAt(1)
        assert(true === true)
    }

    it should "perform product by scalar" in {
        val m = 4
        val n = 2
        val ones = BreezeBuilder oneMatrix (m,n)
        val twos = ones * 2.0
        for (i <- 0 to twos.rows-1)
            for (j <- 0 to twos.cols-1)
                assert(twos(i,j) === 2.0)
    }

    it should "perform addition to scalar" in {
        val m = 4
        val n = 2
        val ones = BreezeBuilder oneMatrix (m,n)
        val threes = ones + 2.0
        for (i <- 0 to threes.rows-1)
            for (j <- 0 to threes.cols-1)
                assert(threes(i,j) === 3.0)
    }

    it should "power a vector" in {
        val m = 3
        val n = 2
        val ones = BreezeBuilder oneMatrix (m,n)
        val twos = ones * 2.0
        val fours = new DenseMatrixDecorator(twos)^2.0
        for (i <- 0 to fours.rows-1)
            for (j <-0 to fours.cols-1)
                assert(fours(i,j) === 4.0)
    }

    it should "perform difference between matrices" in {
        val v1 = new DenseMatrixDecorator(DenseMatrix((1.0,5.0),(2.0,7.0)))
        val v2 = new DenseMatrixDecorator(DenseMatrix((4.0,2.0),(3.0,6.0)))
        val v3 = (v1 - v2).get
        assert(v3(0,0) === -3.0, "position: (0,0): " + v1(0,0) + " - " + v2(0,0))
        assert(v3(0,1) === 3.0, "position: (0,1): " + v1(0,1) + " - " + v2(0,1))
        assert(v3(1,0) === -1.0, "position: (1,0): " + v1(1,0) + " - " + v2(1,0))
        assert(v3(1,1) === 1.0, "position: (1,1): " + v1(1,1) + " - " + v2(1,1))
    }

    it should "calculate the mean of a matrix" in {
        val v1 = new DenseMatrixDecorator(DenseMatrix((1.0,5.0),(2.0,7.0)))
        assert (v1.mean === DenseVector(1.5,6.0))
    }

    it should "calculate the standard deviation of a matrix" in {
        val v1 = new DenseMatrixDecorator(DenseMatrix((1.0,5.0),(2.0,7.0)))
        assert (abs(v1.stdv(0) - 0.70711) < 0.0001)
        assert (abs(v1.stdv(1) - 1.41412) < 0.0001)
    }

    it should "normalize by columns" in {
        val normalized = new DenseMatrixDecorator(DenseMatrix((1.0,5.0),(2.0,7.0))).normalize
        assert (normalized(0,0) < 0.0, normalized(0,0) + "- (0,0) < 0")
        assert (abs(abs(normalized(0,0)) - 0.70711) < 0.0001, normalized(0,0) + "- (0,0)")
        assert (normalized(1,0) > 0.0, normalized(1,0) + "- (1,0) > 0")
        assert (abs(abs(normalized(0,1)) - 0.70711) < 0.0001, normalized(1,0) + "- (0,1)")
        assert (normalized(0,1) < 0.0, normalized(0,1) + "- (0,1) < 0")
        assert (abs(abs(normalized(1,0)) - 0.70711) < 0.0001, normalized(0,1) + "- (1,0)")
        assert (normalized(1,1) > 0.0, normalized(1,1) + "- (1,1) > 0")
        assert (abs(abs(normalized(1,1)) - 0.70711) < 0.0001, normalized(1,1) + "- (1,1)")
        for (j <- 0 to normalized.cols-1){
            val col = normalized(::,j).toDenseVector
            val norm = new DenseVectorDecorator(col).norm
            assert(norm - 1 < 0.0001, norm)
        }
    }

}