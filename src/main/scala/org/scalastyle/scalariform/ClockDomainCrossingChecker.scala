package org.scalastyle.scalariform

import scalariform.parser.CompilationUnit
import org.scalastyle._
import _root_.scalariform.lexer.Token

class ClockDomainCrossingChecker extends ScalariformChecker {
  val errorKey = "clock.domain.crossing"

  override def verify(ast: CompilationUnit): List[ScalastyleError] = {
    // 遍历语法树，找到需要检查的节点
    val violations = for {
      signal <- detectCrossClockSignals(ast)       // 找到跨时钟域的信号
      if !hasSynchronization(ast, signal)         // 检查是否有同步逻辑
    } yield {
      PositionError(signal.offset, List(s"Signal '${signal.text}' lacks synchronization"))
    }

    violations.toList
  }

  private def detectCrossClockSignals(ast: CompilationUnit): List[Token] = {
    // 实现跨时钟域信号检测逻辑（示例中通过信号命名约定区分）
    ast.tokens.filter(token => token.text.contains("_to_"))
  }

  private def hasSynchronization(ast: CompilationUnit, signal: Token): Boolean = {
    // 检查信号是否有同步逻辑
    ast.tokens.sliding(3).exists {
      case List(_, mid, _) if mid.text == "RegNext" || mid.text == "Queue" => true
      case _ => false
    }
  }
}
