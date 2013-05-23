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
import breeze.linalg._
import scala.io.Source

/**
 * @author lmancera
 */
 case class DataFileParser(path:String,separator:Char){

 	def toDenseVector: DenseVector[Double] = {
		toDenseMatrix.toDenseVector
 	}

 	def toDenseMatrix: DenseMatrix[Double] = {
		val lines = scala.io.Source.fromFile(path).getLines.size
		var inputArray = scala.io.Source.fromFile(path).getLines.toArray.flatMap(_.split(separator)).map(_.toDouble)
		new DenseMatrix(lines, inputArray)
 	}

 }

