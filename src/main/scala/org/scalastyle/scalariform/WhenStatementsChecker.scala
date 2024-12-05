package org.scalastyle.scalariform

import _root_.scalariform.parser.CompilationUnit
import org.scalastyle.PositionError
import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError
import _root_.scalariform.lexer.Token

class WhenStatementsChecker extends ScalariformChecker {
  val errorKey = "when.statements"
  val maxWhenCount: Int = 4

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val tokens = ast.tokens
    // 找到所有 Module 的定义区域
    val moduleRanges = tokens.sliding(3).collect {
      case List(classToken, nameToken, extendsToken)
        if classToken.text == "class" && extendsToken.text == "extends" =>
        (nameToken.offset, findEndOffset(tokens, nameToken.offset))
    }.toList
    // 遍历每个模块，检测 when 的数量
    moduleRanges.flatMap { case (startOffset, endOffset) =>
      val moduleTokens = tokens.filter(t => t.offset >= startOffset && t.offset <= endOffset)
      val whenCount = moduleTokens.count(_.text == "when")

      if (whenCount > maxWhenCount) {
        List(PositionError(endOffset, List(s"Too many `when` statements in module. Limit: $maxWhenCount")))
      } else {
        List()
      }
    }
  }

  // 寻找模块结束的位置（根据大括号匹配）
  def findEndOffset(tokens: List[Token], startOffset: Int): Int = {
    val (_, _, endOffset) = tokens.dropWhile(_.offset < startOffset).foldLeft((false, 0, -1)) {
      case ((started, depth, result), token) =>
        if (result != -1) (started, depth, result)
        else token.text match {
          case "{" =>
            val newDepth = depth + 1
            (true, newDepth, if (newDepth == 0 && started) token.offset else -1)
          case "}" =>
            val newDepth = depth - 1
            (started, newDepth, if (newDepth == 0 && started) token.offset else -1)
          case _ => (started, depth, -1)
        }
    }
    endOffset
  }
}

