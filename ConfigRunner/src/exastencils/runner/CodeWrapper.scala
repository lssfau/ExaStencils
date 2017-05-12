package exastencils.runner

import exastencils.util.Evaluator

/// CodeWrapper

case class CodeWrapper(var expression : String) {
  val defImports = "import exastencils.config.Knowledge._\n" + "import exastencils.config.Settings._\n" + "import exastencils.config.Platform._\n"

  def print() : String = "CodeInjection ( " + expression + ")"
  def eval[T]() : T = Evaluator[T](defImports + expression)
}
