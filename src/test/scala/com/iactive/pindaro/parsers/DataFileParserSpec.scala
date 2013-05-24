package com.iactive.pindaro.parsers

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
import java.io._

/**
 * @author lmancera
 */
class DataFileParserSpec extends FlatSpec {

	def testFilePath = "test.txt"
	def separator = ' '
	def contentArray = Array('1', separator, '2', separator, '3', '\n', '4', separator, '5', separator, '6', separator, '\n')
	def contentVector = Array('1', separator, '2', separator, '3', separator, '4', separator, '\n')

    "DataFileParserSpec" should "load from file a matrix" in {
    	writeTestArray
    	checkMatrixAssertions
		deleteTestFile
    }

    it should "load from file a vector" in {
    	writeTestVector
    	checkVectorAssertions
		deleteTestFile
    }

    def writeTestArray = writeTestFile(contentArray)

    def writeTestVector = writeTestFile(contentVector)

    def writeTestFile(content:Array[Char]):Unit = {
		val out = new FileWriter(testFilePath)
		out.write(content)
		out.close    	
    }

    def deleteTestFile = new File(testFilePath).delete

    def checkMatrixAssertions = {
		val matrix = DataFileParser(testFilePath,separator).toDenseMatrix
        assert(matrix(0,0) === 1.0, matrix(0,0))
        assert(matrix(0,1) === 2.0, matrix(0,1))
        assert(matrix(0,2) === 3.0, matrix(0,2))
        assert(matrix(1,0) === 4.0, matrix(1,0))
        assert(matrix(1,1) === 5.0, matrix(1,1))
        assert(matrix(1,2) === 6.0, matrix(1,2))
    }

    def checkVectorAssertions = {
		val vector = DataFileParser(testFilePath,separator).toDenseVector
        assert(vector(0) === 1.0)
        assert(vector(1) === 2.0)
        assert(vector(2) === 3.0)
        assert(vector(3) === 4.0)
    }

}
