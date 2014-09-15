package exastencils.core

import scala.collection.mutable.ListBuffer

import exastencils.prettyprinting._

object Settings {
  var outputPath : String = "/tmp/"
  var buildfileGenerator: BuildfileGenerator = MakefileGenerator
  var binary : String = "exastencils"
  var basePathPrefix : String = ".."
  var l4file : String = null
  def getL4file : String = if (l4file != null) l4file else basePathPrefix + "/Compiler/dsl/Layer4.exa"
  var additionalIncludes : ListBuffer[String] = ListBuffer()
  var additionalFiles : ListBuffer[String] = ListBuffer()
}
