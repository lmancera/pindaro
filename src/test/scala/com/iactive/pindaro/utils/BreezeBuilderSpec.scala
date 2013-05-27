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

/**
 * @author lmancera
 */
class BreezeBuilderSpec extends FlatSpec {

    "BreezeBuilder" should "be able to build a zero vector" in {
        val n = 10
        val zeros = BreezeBuilder zeroVector n
        for (i <- 0 to n-1)
            assert(zeros(i) === 0.0)
    }

    it should "be able to build a one vector" in {
        val n = 10
        val zeros = BreezeBuilder oneVector n
        for (i <- 0 to n-1)
            assert(zeros(i) === 1.0)
    }

    it should "be able to build a zero matrix" in {
        val m = 15
        val n = 10
        val zeros = BreezeBuilder zeroMatrix (m,n)
        for (i <- 0 to m-1)
            for (j <- 0 to n-1)
                assert(zeros(i,j) === 0.0)
    }

    it should "be able to build a one matrix" in {
        val m = 15
        val n = 10
        val zeros = BreezeBuilder oneMatrix (m,n)
        for (i <- 0 to m-1)
            for (j <- 0 to n-1)
                assert(zeros(i,j) === 1.0)
    }

}