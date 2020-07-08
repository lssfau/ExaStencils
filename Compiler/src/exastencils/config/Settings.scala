//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.config

import scala.collection.mutable.ListBuffer

import exastencils.prettyprinting._

object Settings {
  // allows triggering user-specific code
  var user : String = "guest"

  /// input

  var basePathPrefix : String = ""

  def getBasePath = {
    if (basePathPrefix.isEmpty || basePathPrefix.endsWith("/") || basePathPrefix.endsWith("\\"))
      basePathPrefix
    else
      basePathPrefix + "/"
  }

  // experimental domain file parameter
  var experimental_domain_file : String = "."

  /// layer 1

  var l1file : ListBuffer[String] = ListBuffer()
  def getL1file : ListBuffer[String] = l1file.map(getBasePath + _)

  var debugL1File : String = ""
  def getDebugL1file : String = if (debugL1File.isEmpty) debugL1File else getBasePath + debugL1File

  /// layer 2

  var l2file : ListBuffer[String] = ListBuffer()
  def getL2file : ListBuffer[String] = l2file.map(getBasePath + _)

  var debugL2File : String = ""
  def getDebugL2file : String = if (debugL2File.isEmpty) debugL2File else getBasePath + debugL2File

  /// layer 3

  var l3file : ListBuffer[String] = ListBuffer()
  def getL3file : ListBuffer[String] = l3file.map(getBasePath + _)

  var debugL3File : String = ""
  def getDebugL3file : String = if (debugL3File.isEmpty) debugL3File else getBasePath + debugL3File

  /// layer 4

  var l4file : ListBuffer[String] = ListBuffer()
  def getL4file : ListBuffer[String] = l4file.map(getBasePath + _)

  var debugL4File : String = ""
  def getDebugL4file : String = if (debugL4File.isEmpty) debugL4File else getBasePath + debugL4File

  def minLayerFileProvided : Int = {
    if (getL1file.nonEmpty) 1
    else if (getL2file.nonEmpty) 2
    else if (getL3file.nonEmpty) 3
    else if (getL4file.nonEmpty) 4
    else 5
  }

  /// config file for polyhedral search space exploration

  var poly_explorationConfig : String = ""

  // specifies if the poly exploration config ids should be appended to the output path (one after the other)
  var poly_exploration_appendID2path : Boolean = true

  /// output

  var outputPath : String = ""
  var outputPath_suffix : String = ""

  def defOutputPath : String = {
    if (basePathPrefix.isEmpty) {
      if ("MSVC" == Platform.targetCompiler)
        "../generated"
      else
        "/tmp"
    } else {
      getBasePath + "generated"
    }
  }

  def getOutputPath : String = {
    val path =
      if (outputPath.isEmpty)
        defOutputPath
      else
        getBasePath + (
          if (outputPath.endsWith("/") || outputPath.endsWith("\\"))
            outputPath.substring(0, outputPath.length() - 1)
          else
            outputPath
          )
    path + outputPath_suffix + '/'
  }

  var cancelIfOutFolderExists : Boolean = false

  /// management

  var configName : String = ""

  /// execution path for cluster jobs

  var executionPath : String = "~/"

  /// output for fragment data file

  var fragmentFile_config_output = 2
  // 0:binary, 1:readable, 2:both
  def fragmentFile_config_path_readable = outputPath + "Domains/DomainConfiguration.cfg"
  def fragmentFile_config_path_domainConfig = outputPath + "Domains/config.dat"
  def fragmentFile_config_path_binary = outputPath + "Domains/fragments.dat"

  /// buildfile

  // may be any subset of 'MakefileGenerator', 'CMakeGenerator', 'ProjectGenerator'
  // this list is parsed by parseBuildfileGenerators()
  var buildfileGenerators : ListBuffer[String] = ListBuffer()

  var binary : String = "exastencils"

  var makefile_makeLibs : Boolean = true
  var makefile_additionalCFlags : ListBuffer[String] = ListBuffer()
  var makefile_additionalLDFlags : ListBuffer[String] = ListBuffer()
  // Additional flags for CUDA compiler
  var makefile_additionalCudaFlags : ListBuffer[String] = ListBuffer()
  var makefile_additionalObjFiles : ListBuffer[String] = ListBuffer()

  /// performance estimates (experimental)

  var performanceEstimateOutputFile : String = "Compiler/performanceEstimate.csv"

  /// Separator used in CSV output, ';' and '\t' are valid choices.

  var csvSeparator : String = ";"

  /** Returns csvSeparator as C++ string literal. */
  def csvSeparatorEscaped() : String = {
    csvSeparator match {
      case ";"  => ";"
      case "\t" => "\\t"
      case _    => throw new Exception("bad csvSeparator in Settings")
    }
  }

  /// files emitted by the generated code

  var timerOutputFile : String = "timings.csv"
  var characteristicsFile : String = "characteristics.csv"

  /// external dependencies

  var pathsInc : ListBuffer[String] = ListBuffer()
  var pathsLib : ListBuffer[String] = ListBuffer()

  var additionalIncludes : ListBuffer[String] = ListBuffer()
  var additionalNamespaces : ListBuffer[String] = ListBuffer()
  var additionalFiles : ListBuffer[String] = ListBuffer()
  var additionalLibs : ListBuffer[String] = ListBuffer()

  var additionalDefines : ListBuffer[String] = ListBuffer()

  var additionalMacros : ListBuffer[String] = ListBuffer() // which are available in all generated code parts

  /// generation process
  var failOnConstraint : Boolean = false

  /// logging
  var produceHtmlLog : Boolean = false
  var htmlLogFile : String = ""

  def defHtmlLogFile : String = if (basePathPrefix.isEmpty) "../Compiler/log/log.html" else getBasePath + "Compiler/log/log.html"
  def getHtmlLogFile : String = if (htmlLogFile.isEmpty) defHtmlLogFile else getBasePath + htmlLogFile

  /// debug output

  var printClonedObjects : Boolean = false

  var timeStrategies : Boolean = false
  // five percent threshold by default -> measurements with less than 5.0 % share are not displayed
  var timeStratPercentThreshold : Int = 5
  var printNodeCountAfterTransformation : Boolean = false
  // print number of nodes after each transformation
  var printNodeCountAfterStrategy : Boolean = false
  // print number of nodes after each strategy
  var printTransformationTime : Boolean = false
  var logStrategyResults : Boolean = true // Debug log strategy results
  // time the steps performed by the IR_PolyOpt strategy
  var timePolyOptSteps : Boolean = false

  def update() : Unit = {
    // Settings parser does not parse escapes in string literals.
    if (csvSeparator == "\\t")
      csvSeparator = "\t"

    // check if CSV separator is valid
    csvSeparatorEscaped()

    // parse here to fail early
    BuildfileGenerator.parseGenerators(buildfileGenerators)

    // handle CUDA
    if (Knowledge.cuda_enabled) {
      Platform.targetOS match {
        case "Windows"         =>
          if (!additionalLibs.contains("cuda.lib")) additionalLibs += "cuda.lib"
          if (!additionalLibs.contains("cudart.lib")) additionalLibs += "cudart.lib"
          if (!pathsInc.contains("$(CUDA_INC_PATH)")) pathsInc += "$(CUDA_INC_PATH)"
          if (!pathsLib.contains("$(CUDA_LIB_PATH)")) pathsLib += "$(CUDA_LIB_PATH)"
        case "Linux" | "macOS" =>
          if (!additionalLibs.contains("cuda")) additionalLibs += "cuda"
          if (!additionalLibs.contains("cudart")) additionalLibs += "cudart"
      }
    }
    Knowledge.benchmark_backend match {
      case "likwid" =>
        if (!additionalIncludes.contains("likwid.h")) additionalIncludes += "likwid.h"
        if (!additionalLibs.contains("likwid")) additionalLibs += "likwid"
        if (!additionalDefines.contains("LIKWID_PERFMON")) additionalDefines += "LIKWID_PERFMON"
      case _ =>
    }
    if (Platform.simd_mathLibrary == "mass_simd")
      additionalLibs += "mass_simd"
  }
}
