package exastencils.core

import scala.collection.mutable.ListBuffer

import exastencils.prettyprinting._

object Settings {
  var outputPath : String = "/tmp/"
  var buildfileGenerator = MakefileGenerator
  var binary = "exastencils"
  var basePathPrefix = ".."
  var additionalIncludes : ListBuffer[String] = ListBuffer()
  var additionalFiles : ListBuffer[String] = ListBuffer()
}
