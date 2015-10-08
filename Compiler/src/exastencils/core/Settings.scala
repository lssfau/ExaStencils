package exastencils.core

import scala.collection.mutable.ListBuffer
import exastencils.prettyprinting._
import exastencils.knowledge.Knowledge

object Settings {
  var user : String = "guest" // allows triggering user-specific code

  /// input
  var basePathPrefix : String = ""
  def getBasePath = if (basePathPrefix.isEmpty || basePathPrefix.endsWith("/") || basePathPrefix.endsWith("\\")) basePathPrefix else basePathPrefix + "/"

  var l4file : String = ""
  def defL4file : String = if ("" == basePathPrefix) "../Compiler/dsl/Layer4.exa" else getBasePath + "Compiler/dsl/Layer4.exa"
  def getL4file : String = if (l4file.isEmpty) defL4file else getBasePath + l4file

  var l3file : String = ""
  def defL3file : String = if ("" == basePathPrefix) "../Compiler/dsl/Layer3.exa" else getBasePath + "Compiler/dsl/Layer3.exa"
  def getL3file : String = if (l3file.isEmpty) defL3file else getBasePath + l3file

  /// output
  var outputPath : String = ""
  def defOutputPath : String = if ("" == basePathPrefix) { if ("MSVC" == Knowledge.targetCompiler) "../generated" else "/tmp/" } else getBasePath + "generated/"
  def getOutputPath : String = if (outputPath.isEmpty) defOutputPath else getBasePath + outputPath
  var cancelIfOutFolderExists : Boolean = false

  /// management
  var configName : String = ""

  /// output for fragment data file
  var fragmentFile_config_output = 2 // 0:binary, 1:readable, 2:both
  def fragmentFile_config_path_readable = outputPath + "Domains/DomainConfiguration.cfg"
  def fragmentFile_config_path_domainConfig = outputPath + "Domains/config.dat"
  def fragmentFile_config_path_binary = outputPath + "Domains/fragments.dat"

  /// buildfile
  var buildfileGenerator : BuildfileGenerator = MakefileGenerator
  var binary : String = "exastencils"

  /// external dependencies
  var pathsInc : ListBuffer[String] = ListBuffer()
  var pathsLib : ListBuffer[String] = ListBuffer()

  var additionalIncludes : ListBuffer[String] = ListBuffer()
  var additionalFiles : ListBuffer[String] = ListBuffer()
  var additionalLibs : ListBuffer[String] = ListBuffer()

  var additionalDefines : ListBuffer[String] = ListBuffer()

  /// generation process
  var failOnConstraint : Boolean = false

  /// logging
  var produceHtmlLog : Boolean = false
  var htmlLogFile : String = ""
  def getHtmlLogFile : String = getBasePath + (if (htmlLogFile != "") htmlLogFile else "Compiler/log/log.html")

  var timeStrategies : Boolean = false
  var timeStratPercentThreshold : Int = 5 // five percent threshold by default -> measurements with less than 5.0 % share are not displayed
  var printNodeCountAfterTransformation : Boolean = false // print number of nodes after each transformation
  var printNodeCountAfterStrategy : Boolean = false // print number of nodes after each strategy
  var printTransformationTime : Boolean = false
}
