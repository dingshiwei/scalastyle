package org.scalastyle.scalariform

import org.scalastyle._
import org.scalastyle.PositionError
import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError
import _root_.scalariform.parser.{AstNode, BlockExpr, CompilationUnit}

class SignalInitializationChecker extends ScalariformChecker {
  val errorKey = "register.uninitialized"

  // 该规则默认会检查所有 `Reg` 和 `RegInit` 类型的信号
  val allowedTypes = Set("Reg", "RegInit")

  // 检查信号是否明确初始化
  override def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val errors = for {
      List(left, _, right) <- ast.tokens.sliding(3)
      if allowedTypes.contains(left.text) // 检查是否是 `Reg` 或 `RegInit`
      if !isInitialized(right.text) // 检查是否有初始化值
    } yield {
      PositionError(right.offset, List(s"Register ${left.text} is not initialized explicitly."))
    }
    
    errors.toList
  }

  private def isInitialized(text: String): Boolean = {
    // 初始化值通常以 " := " 或其他形式出现
    if (text.matches("""\d+(\.\d+)?""")) {
      return true
    }
    val InitPattern = """\s*:=\s*""".r
    InitPattern.findFirstIn(text).isDefined
  }
}
