package exastencils.core

import exastencils.prettyprinting.MakefileGenerator

object Settings {
  var outputPath : String = "/tmp/"
  var buildfileGenerator = MakefileGenerator
  var binary = "exastencils"
  var basePathPrefix = ".."
}
