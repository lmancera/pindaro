package com.iactive.pindaro.classification

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

import com.iactive.pindaro.functions._

/**
 * @author lmancera
 */
class OneVsAllSpec extends FlatSpec {
    // FIXME!!!

/*	"OneVsAllSpec" should "return 0.0s when zero input" in {
        val m = 5
        val n = 3
        val X = DenseMatrix.zeros[Double](m,n)
        val y = DenseVector.zeros[Double](m)
        val numLabels = 10
        val lambda = 0
		assert(OneVsAll(X,y,numLabels).train === DenseMatrix.zeros[Double](numLabels,n))
	}

    it should "perform simple classification task" in {
        val m = 10
        val n = 2
        val numLabels = 2
        val X = DenseMatrix((0.,0.),(0.,1.),(1.,1.),(1.,0.),(0.5,0.25),(10.,10.),(11.,10.),(10.,11.),(11.,11.),(10.5,10.25))
        val y = DenseVector(0.,0.,0.,0.,0.,1.,1.,1.,1.,1.)
        assert(X.rows === m)
        assert(X.cols === 2)
        assert(y.length === m)
        val lambda = 0.5
        val theta = OneVsAll(X,y,numLabels).train
        var newInput = DenseVector(0.75,0.5)
        def predicted(v:DenseVector[Double]) = (Sigmoid applyToVector v).argmax
        assert(predicted(theta*newInput) === 0)
        newInput = DenseVector(10.75,10.5)
        assert(predicted(theta*newInput) === 1)
        
    }*/

}