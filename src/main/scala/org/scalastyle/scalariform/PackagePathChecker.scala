// Copyright (C) 2011-2012 the original author or authors.
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package org.scalastyle.scalariform

import _root_.scalariform.lexer.Token
import _root_.scalariform.lexer.Tokens.{DOT, PACKAGE, VARID}
import _root_.scalariform.parser.CompilationUnit
import org.scalastyle.{FilePathChecker, FileSpec, PositionError, ScalariformChecker, ScalastyleError}


class PackagePathChecker extends FilePathChecker {
  val DefaultRegex = "^[a-z]+$"
  val errorKey = "package.define"

  override def verify[T <: FileSpec](ast: CompilationUnit, file: T): List[ScalastyleError] = {

    def isPartOfPackageName(t: Token): Boolean = (t.tokenType == DOT) || (t.tokenType == VARID)

    @annotation.tailrec
    def getNextPackageName(tokens: List[Token]): (List[Token], List[Token]) = tokens match {
      case Nil => (Nil, Nil)
      case hd :: tail if hd.tokenType == PACKAGE => tail.span(isPartOfPackageName)
      case l: Any => getNextPackageName(l.dropWhile(tok => tok.tokenType != PACKAGE))
    }

    @annotation.tailrec
    def getPackageNameLoop(tokens: List[Token], myAccumulator: List[List[Token]]): List[List[Token]] =
      getNextPackageName(tokens) match {
        case (Nil, Nil) => myAccumulator.reverse  // Return the result, but reverse since we gathered backward
        case (Nil, remainder) => getPackageNameLoop(remainder, myAccumulator) // Found package object - try again
        case (l, remainder) =>  // add match to results, go look again
          val pkgName = l.filter(tok => tok.tokenType != DOT) // Strip out the dots between varids
          getPackageNameLoop(remainder, pkgName :: myAccumulator)
      }

    val packageNames = getPackageNameLoop(ast.tokens, Nil)
    println(packageNames)
    val errors: List[PositionError] = for {
        pkgNameList <- packageNames
        pkgName <- pkgNameList.map(_.text).mkString("/")
        if !file.name.contains(pkgName)
      } yield {
        println(pkgName)
        PositionError(ast.tokens.head.offset)
      }
      errors
    }

  override def verify(ast: CompilationUnit): List[ScalastyleError] = ???
}

