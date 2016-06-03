package exastencils.core

import scala.collection.mutable.ListBuffer

import exastencils.knowledge._
import exastencils.prettyprinting._

object Settings {
  var user : String = "guest" // allows triggering user-specific code

  /// input
  var basePathPrefix : String = ""
  def getBasePath = if (basePathPrefix.isEmpty || basePathPrefix.endsWith("/") || basePathPrefix.endsWith("\\")) basePathPrefix else basePathPrefix + "/"
  def inputFromJson : Boolean = false

  var l3file : String = ""
  def defL3file : String = if ("" == basePathPrefix) "../Compiler/dsl/Layer3.exa" else getBasePath + "Compiler/dsl/Layer3.exa"
  def getL3file : String = if (l3file.isEmpty) defL3file else getBasePath + l3file

  var l4file : String = ""
  def defL4file : String = if ("" == basePathPrefix) "../Compiler/dsl/Layer4.exa" else getBasePath + "Compiler/dsl/Layer4.exa"
  def getL4file : String = if (l4file.isEmpty) defL4file else getBasePath + l4file

  var poly_explorationConfig : String = "" // config file for polyhedral search space exploration

  /// output
  var outputPath : String = ""
  def defOutputPath : String = if ("" == basePathPrefix) { if ("MSVC" == Platform.targetCompiler) "../generated/" else "/tmp/" } else getBasePath + "generated/"
  def getOutputPath : String = if (outputPath.isEmpty) defOutputPath else getBasePath + (if (outputPath.endsWith("/") || outputPath.endsWith("\\")) outputPath else outputPath + "/")
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

  var additionalMacros : ListBuffer[String] = ListBuffer() // which are available in all generated code parts

  /// generation process
  var failOnConstraint : Boolean = false

  /// logging
  var produceHtmlLog : Boolean = false
  var htmlLogFile : String = ""
  def defHtmlLogFile : String = if ("" == basePathPrefix) "../Compiler/log/log.html" else getBasePath + "Compiler/log/log.html"
  def getHtmlLogFile : String = if (htmlLogFile.isEmpty) defHtmlLogFile else getBasePath + htmlLogFile

  /// debug output
  var printClonedObjects : Boolean = false

  var timeStrategies : Boolean = false
  var timeStratPercentThreshold : Int = 5 // five percent threshold by default -> measurements with less than 5.0 % share are not displayed
  var printNodeCountAfterTransformation : Boolean = false // print number of nodes after each transformation
  var printNodeCountAfterStrategy : Boolean = false // print number of nodes after each strategy
  var printTransformationTime : Boolean = false

  def update() : Unit = {
    // handle CUDA
    if (Knowledge.experimental_cuda_enabled) {
      Platform.targetOS match {
        case "Windows" =>
          if (!additionalLibs.contains("cuda.lib")) additionalLibs += "cuda.lib"
          if (!additionalLibs.contains("cudart.lib")) additionalLibs += "cudart.lib"
          if (!pathsInc.contains("$(CUDA_INC_PATH)")) pathsInc += "$(CUDA_INC_PATH)"
          if (!pathsLib.contains("$(CUDA_LIB_PATH)")) pathsLib += "$(CUDA_LIB_PATH)"
        case "Linux" | "OSX" =>
          if (!additionalLibs.contains("cuda")) additionalLibs += "cuda"
          if (!additionalLibs.contains("cudart")) additionalLibs += "cudart"
      }
    }
    if (Platform.simd_mathLibrary == "mass_simd")
      additionalLibs += "mass_simd"
  }
}
