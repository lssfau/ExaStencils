package exastencils.core

import exastencils.prettyprinting.MakefileGenerator

object Settings {
  var outputPath : String = "E:/tmp/ExaStencils/"
  var buildfileGenerator = MakefileGenerator
  var binary = "exastencils"
  var basePathPrefix = ".."
}