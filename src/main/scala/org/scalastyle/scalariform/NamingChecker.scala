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
import _root_.scalariform.lexer.Tokens.CLASS
import _root_.scalariform.lexer.Tokens.OBJECT
import _root_.scalariform.lexer.Tokens.VAL
import _root_.scalariform.lexer.Tokens.VARID
import _root_.scalariform.parser.{AstNode, BlockExpr, CompilationUnit}

// scalastyle:off multiple.string.literals

class NamingChecker extends ScalariformChecker {
  val errorKey = "class.naming"
  val namingPattern = "^[A-Z][a-zA-Z0-9]*$".r

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val it = for {
      List(left, right) <- ast.tokens.sliding(2)
      if left.tokenType == CLASS && namingPattern.findFirstIn(right.text).isEmpty
    } yield {
      PositionError(right.offset, List(s"class name should use uppercase and underscores"))
    }
    it.toList
  }
}


class ConstantNamingChecker extends ScalariformChecker {
  val errorKey = "constant.naming"

  // 定义常量命名的正则表达式（全大写字母，用下划线分隔单词）
  val namingPattern = "^[A-Z][A-Z0-9_]*$".r

  override def verify(ast: CompilationUnit): List[ScalastyleError] = {
    // 滑动窗口遍历 tokens，检查模式是否匹配
    val it = for {
      token <- ast.tokens // 遍历所有 tokens
      if token.tokenType == VAL // 只检查变量名
      if namingPattern.findFirstIn(token.text).isEmpty // 如果变量名不匹配全大写规则
    } yield {
      // 返回一个错误，记录偏移量和提示信息
      PositionError(token.offset, List(s"Constant '$token.text' should use uppercase and underscores"))
    }
    it.toList // 将结果转换为列表返回
  }
}

class ModuleNamingChecker extends ScalariformChecker {
  val errorKey = "module.naming"

  // PascalCase 命名规则的正则表达式
  val namingPattern = "^[A-Z][a-zA-Z0-9]*$".r

  override def verify(ast: CompilationUnit): List[ScalastyleError] = {
    // 遍历所有 tokens，筛选出模块定义并检查命名规则
    val it = for {
      token <- ast.tokens
      if token.tokenType == OBJECT // 筛选出模块 (object 定义)
      val nextToken = ast.tokens.dropWhile(_ != token).tail.headOption// 获取模块名
      if nextToken.exists(t => namingPattern.findFirstIn(t.text).isEmpty) // 检查模块名是否符合 PascalCase
    } yield {
      PositionError(nextToken.get.offset, List(s"Module '${nextToken.get.text}' should use PascalCase naming"))
    }

    it.toList // 将结果转换为列表返回
  }
}

class SignalNamingChecker extends ScalariformChecker {
  val errorKey = "signal.naming"

  // 定义信号命名规则的正则表达式（小写字母加下划线分隔单词）
  val namingPattern = "^[a-z][a-z0-9_]*$".r

  override def verify(ast: CompilationUnit): List[ScalastyleError] = {
    // 遍历所有 tokens，筛选出信号定义并检查命名规则
    val it = for {
      token <- ast.tokens
      if token.tokenType == VARID // 筛选出变量（var）或信号定义
      if namingPattern.findFirstIn(token.text).isEmpty // 如果名称不符合规则
    } yield {
      // 记录错误信息
      PositionError(token.offset, List(s"Signal '${token.text}' should use lowercase letters with underscores"))
    }

    it.toList // 将结果转换为列表返回
  }
}

class PortDefinitionChecker extends ScalariformChecker {
  val errorKey = "port.definition"
  // 重写 verify 方法
  override def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val it = for {
      List(left, right) <- ast.tokens.sliding(2)
      if right.text == "UInt" && left.text != "Input" && left.text != "Output"
    } yield {
      PositionError(right.offset, List(s"port.definition"))
    }
    it.toList
  }
}

