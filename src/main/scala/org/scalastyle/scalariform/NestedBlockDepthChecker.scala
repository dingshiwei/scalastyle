package org.scalastyle.scalariform

import _root_.scalariform.parser.CompilationUnit
import org.scalastyle.PositionError
import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError

class NestedBlockDepthChecker extends ScalariformChecker {
  val errorKey = "block.depth"
  val maxDepth: Int = 4
  val minDepth: Int = 0

   def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val tokens = ast.tokens
    var currentDepth = 0
    var errors: List[ScalastyleError] = List()

    tokens.foreach { token =>
      token.text match {
        case "{" =>
          currentDepth += 1
          if (currentDepth > maxDepth) {
            errors :+= PositionError(token.offset, List(s"Nested block is too long"))
          }
        case "}" =>
          currentDepth -= 1
          if (currentDepth < minDepth) {
            errors :+= PositionError(token.offset, List(s"Nested block is too short"))
          }
        case _ =>
      }
    }

    // 返回检测到的所有错误
    errors
  }
}

