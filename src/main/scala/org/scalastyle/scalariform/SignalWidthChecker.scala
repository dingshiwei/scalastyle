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

import org.scalastyle.PositionError
import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError
import _root_.scalariform.parser.{AstNode, BlockExpr, CompilationUnit}


class SignalWidthChecker extends ScalariformChecker {
  val errorKey = "signal.width"
  val maxWidth = 32
  // 重写 verify 方法
  override def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val SignalPattern = Set("UInt", "SInt", "FixedPoint") // 支持的信号类型

    val errors = for {
      List(left, _, right) <- ast.tokens.sliding(3) // 每三个连续 Token 一组
      if SignalPattern.contains(left.text)         // 检查是否是支持的类型
      width = parseWidth(right.text)               // 尝试解析宽度
      if width.exists(_ > maxWidth)                // 如果宽度超过 maxWidth
    } yield {
      PositionError(right.offset, List(s"Signal width exceeds maximum allowed width $maxWidth."))
    }
    errors.toList
  }

  private def parseWidth(text: String): Option[Int] = {
    try {
      Some(text.toInt) // 尝试将文本转换为整数
    } catch {
      case _: NumberFormatException => None // 如果转换失败，返回 None
    }
  }
}

