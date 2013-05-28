package com.iactive.pindaro.examples

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

import org.slf4j.LoggerFactory

import com.iactive.pindaro.parsers._

/**
 * @author lmancera
 */
trait ParseTrainingDataFromFile {

    private val log = LoggerFactory.getLogger(this.getClass)

    val separator = ','
    
    val trainingSetDataFilePath = ""

    def parseData = DataFileParser(trainingSetDataFilePath,separator).toDenseMatrix

    def getSampleData(data:DenseMatrix[Double],columns:Int) = data(::,0 until columns)

	def getVectorSampleData(data:DenseMatrix[Double], column: Int) = data(::,column until column+1).toDenseVector

    def getTrainResult(data:DenseMatrix[Double]) = data(::,data.cols-1).toDenseVector

}