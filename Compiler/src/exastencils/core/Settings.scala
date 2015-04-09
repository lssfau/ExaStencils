package exastencils.core

import scala.collection.mutable.ListBuffer

import exastencils.prettyprinting._

object Settings {
  /// user; allows triggering user-specific code
  var user : String = "guest"

  /// input
  var basePathPrefix : String = ".."
  var l4file : String = ""
  def getL4file : String = if (l4file != "") l4file else basePathPrefix + "/Compiler/dsl/Layer4.exa"

  var l3file : String = ""
  def getL3file : String = if (l3file != "") l3file else basePathPrefix + "/Compiler/dsl/Layer3.exa"

  /// output
  var outputPath : String = "/tmp/"
  var cancelIfOutFolderExists : Boolean = false

  //// ouput for fragment data file
  var fragmentFile_config_output = 2 // 0:binary, 1:readable, 2:both
  def fragmentFile_config_path_readable = outputPath + "Domains/DomainConfiguration.cfg"
  def fragmentFile_config_path_domainConfig = outputPath + "Domains/config.dat"
  def fragmentFile_config_path_binary = outputPath + "Domains/fragments.dat"

  /// buildfile
  var buildfileGenerator : BuildfileGenerator = MakefileGenerator
  var binary : String = "exastencils"

  /// external dependecies
  var additionalIncludes : ListBuffer[String] = ListBuffer()
  var additionalFiles : ListBuffer[String] = ListBuffer()

  /// generation process
  var failOnConstraint : Boolean = false

  /// logging
  var produceHtmlLog : Boolean = false
  var htmlLogFile : String = ""
  def getHtmlLogFile : String = if (htmlLogFile != "") htmlLogFile else basePathPrefix + "/Compiler/log/log.html"

  var timeStrategies : Boolean = false
  var timeStratPercentThreshold : Int = 5 // five percent threshold by default -> measurements with less than 5.0 % share are not displayed  
  var printNodeCountAfterTransformation : Boolean = false // print number of nodes after each transformation
  var printNodeCountAfterStrategy : Boolean = true // print number of nodes after each strategy
}
