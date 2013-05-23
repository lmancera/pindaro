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

    /* Commented because there is no way to close the figure
    it should "plot a figure" in {
        val n = 20
        BreezeFacade.plot(DenseMatrix.rand(n,n))
        assert(true === true)
    }

    it should "plot a figure from a matrix row" in {
        val n = 25
        BreezeFacade.plotAt(DenseMatrix.rand(n,n),1)
        assert(true === true)
    }*/

}