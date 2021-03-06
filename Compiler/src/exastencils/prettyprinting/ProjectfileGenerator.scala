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

package exastencils.prettyprinting

import exastencils.config._

object ProjectfileGenerator extends BuildfileGenerator {
  override def write() : Unit = {
    val projectPrinter = PrettyprintingManager.getPrinter("exastencils.vcxproj")
    val solutionPrinter = PrettyprintingManager.getPrinter("exastencils.sln")

    val filesToConsider = PrettyprintingManager.getFiles ++ Settings.additionalFiles
    val hFileNames = filesToConsider.filter(file => file.endsWith(".h")).toList.sorted
    val cppFileNames = filesToConsider.filter(file => file.endsWith(".cpp")).toList.sorted
    val cuFileNames = filesToConsider.filter(file => file.endsWith(".cu")).toList.sorted

    /// project file

    // header
    projectPrinter <<< "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
    projectPrinter <<< "<Project DefaultTargets=\"Build\" ToolsVersion=\"4.0\" xmlns=\"http://schemas.microsoft.com/developer/msbuild/2003\">"

    // project configurations
    projectPrinter <<< "\t<ItemGroup Label=\"ProjectConfigurations\">"
    projectPrinter <<< "\t\t<ProjectConfiguration Include=\"Release|x64\">"
    projectPrinter <<< "\t\t\t<Configuration>Release</Configuration>"
    projectPrinter <<< "\t\t\t<Platform>x64</Platform>"
    projectPrinter <<< "\t\t</ProjectConfiguration>"
    projectPrinter <<< "\t</ItemGroup>"

    // release config
    projectPrinter <<< "\t<PropertyGroup Condition=\"'$(Configuration)|$(Platform)'=='Release|x64'\" Label=\"Configuration\">"
    projectPrinter <<< "\t\t<ConfigurationType>Application</ConfigurationType>"
    projectPrinter <<< "\t\t<UseDebugLibraries>false</UseDebugLibraries>"
    projectPrinter <<< s"\t\t<PlatformToolset>v${ Platform.targetCompilerVersion }${ Platform.targetCompilerVersionMinor }</PlatformToolset>"
    projectPrinter <<< "\t\t<WholeProgramOptimization>false</WholeProgramOptimization>"
    projectPrinter <<< "\t\t<CharacterSet>MultiByte</CharacterSet>"
    projectPrinter <<< "\t</PropertyGroup>"

    // globals
    projectPrinter <<< "\t<PropertyGroup Label=\"Globals\">"
    projectPrinter <<< "\t\t<ProjectGuid>{435911DB-0E04-462A-A3D6-00054AE0DB84}</ProjectGuid>" // TODO: guid
    projectPrinter <<< "\t\t<Keyword>Win32Proj</Keyword>"
    projectPrinter <<< "\t</PropertyGroup>"

    // import projects
    projectPrinter <<< "\t<Import Project=\"$(VCTargetsPath)\\Microsoft.Cpp.Default.props\" />"
    projectPrinter <<< "\t<Import Project=\"$(VCTargetsPath)\\Microsoft.Cpp.props\" />"

    // extensions
    projectPrinter <<< "\t<ImportGroup Label=\"ExtensionSettings\">"
    if (Knowledge.cuda_enabled) {
      val cudaString = s"CUDA ${ Platform.sw_cuda_version }.${ Platform.sw_cuda_versionMinor }"
      projectPrinter <<< "\t\t<Import Project=\"$(VCTargetsPath)\\BuildCustomizations\\" + cudaString + ".props\" />"
      projectPrinter <<< "\t\t<Import Project=\"$(VCTargetsPath)\\BuildCustomizations\\" + cudaString + ".targets\" />"
    }
    projectPrinter <<< "\t</ImportGroup>"

    // properties
    projectPrinter <<< "\t<ImportGroup Condition=\"'$(Configuration)|$(Platform)'=='Release|x64'\" Label=\"PropertySheets\">"
    projectPrinter <<< "\t\t<Import Project=\"$(UserRootDir)\\Microsoft.Cpp.$(Platform).user.props\" Condition=\"exists('$(UserRootDir)\\Microsoft.Cpp.$(Platform).user.props')\" Label=\"LocalAppDataPlatform\" />"
    projectPrinter <<< "\t</ImportGroup>"

    // macros
    projectPrinter <<< "\t<PropertyGroup Label=\"UserMacros\" />"

    // other settings
    projectPrinter <<< "\t<PropertyGroup Condition=\"'$(Configuration)|$(Platform)'=='Release|x64'\">"
    projectPrinter <<< "\t\t<LinkIncremental>false</LinkIncremental>"
    projectPrinter <<< "\t\t<OutDir>$(ProjectDir)</OutDir>"
    projectPrinter <<< "\t\t<IncludePath>" + Settings.pathsInc.mkString(";") + ";$(ProjectDir);$(IncludePath)</IncludePath>"
    projectPrinter <<< "\t\t<LibraryPath>" + Settings.pathsLib.mkString(";") + ";$(LibraryPath)</LibraryPath>"
    projectPrinter <<< "\t</PropertyGroup>"

    // compiler
    projectPrinter <<< "\t<ItemDefinitionGroup Condition=\"'$(Configuration)|$(Platform)'=='Release|x64'\">"

    // compile step
    projectPrinter <<< "\t\t<ClCompile>"
    projectPrinter <<< "\t\t\t<WarningLevel>Level3</WarningLevel>"
    projectPrinter <<< "\t\t\t<Optimization>MaxSpeed</Optimization>"
    projectPrinter <<< "\t\t\t<IntrinsicFunctions>true</IntrinsicFunctions>"
    projectPrinter <<< s"\t\t\t<PreprocessorDefinitions>${ Settings.additionalDefines.mkString(";") };WIN32;NDEBUG;_CONSOLE;%(PreprocessorDefinitions)</PreprocessorDefinitions>"
    projectPrinter <<< "\t\t\t<MultiProcessorCompilation>true</MultiProcessorCompilation>"
    if (Knowledge.omp_enabled)
      projectPrinter <<< "\t\t\t<OpenMPSupport>true</OpenMPSupport>"
    Platform.simd_instructionSet match {
      case "AVX"  => projectPrinter <<< "\t\t\t<EnableEnhancedInstructionSet>AdvancedVectorExtensions</EnableEnhancedInstructionSet>"
      case "AVX2" => projectPrinter <<< "\t\t\t<EnableEnhancedInstructionSet>AdvancedVectorExtensions2</EnableEnhancedInstructionSet>"
      case _      => // TODO: add other options: StreamingSIMDExtensions, StreamingSIMDExtensions2, NoExtensions
    }
    projectPrinter <<< "\t\t</ClCompile>"

    // cuda compile step
    if (Knowledge.cuda_enabled) {
      projectPrinter <<< "\t\t<CudaCompile>"
      projectPrinter <<< "\t\t\t<TargetMachinePlatform>64</TargetMachinePlatform>"
      projectPrinter <<< s"\t\t\t<CodeGeneration>compute_${ Platform.hw_cuda_capability }${ Platform.hw_cuda_capabilityMinor },sm_${ Platform.hw_cuda_capability }${ Platform.hw_cuda_capabilityMinor }</CodeGeneration>"
      projectPrinter <<< "\t\t\t<FastMath>true</FastMath>"
      projectPrinter <<< "\t\t</CudaCompile>"
    }

    // link step
    projectPrinter <<< "\t\t<Link>"
    projectPrinter <<< "\t\t\t<SubSystem>Console</SubSystem>"
    projectPrinter <<< s"\t\t\t<AdditionalDependencies>${ Settings.additionalLibs.mkString(";") };%(AdditionalDependencies)</AdditionalDependencies>"
    projectPrinter <<< "\t\t</Link>"

    // post build step

    if (Knowledge.cuda_enabled) {
      projectPrinter <<< "\t\t<PostBuildEvent>"
      projectPrinter <<< "\t\t\t<Command>echo copy \"$(CudaToolkitBinDir)\\cudart*.dll\" \"$(OutDir)\""
      projectPrinter <<< "\t\t\t\tcopy \"$(CudaToolkitBinDir)\\cudart*.dll\" \"$(OutDir)\"</Command>"
      projectPrinter <<< "\t\t</PostBuildEvent>"
    }

    projectPrinter <<< "\t</ItemDefinitionGroup>"

    // header files
    projectPrinter <<< "\t<ItemGroup>"
    for (filename <- hFileNames)
      projectPrinter <<< s"""\t\t<ClInclude Include=\"${ filename.replace('/', '\\') }\" />"""
    projectPrinter <<< "\t</ItemGroup>"

    // source files
    projectPrinter <<< "\t<ItemGroup>"
    for (filename <- cppFileNames)
      projectPrinter <<< s"""\t\t<ClCompile Include=\"${ filename.replace('/', '\\') }\" />"""
    projectPrinter <<< "\t</ItemGroup>"

    // cuda kernels
    if (Knowledge.cuda_enabled) {
      projectPrinter <<< "\t<ItemGroup>"
      for (filename <- cuFileNames)
        projectPrinter <<< s"""\t\t<CudaCompile Include=\"${ filename.replace('/', '\\') }\" />"""
      projectPrinter <<< "\t</ItemGroup>"
    }

    // target information
    projectPrinter <<< "\t<Import Project=\"$(VCTargetsPath)\\Microsoft.Cpp.targets\" />"
    projectPrinter <<< "\t<ImportGroup Label=\"ExtensionTargets\">"
    projectPrinter <<< "\t</ImportGroup>"

    // end of project
    projectPrinter <<< "</Project>"

    /// solution file

    solutionPrinter <<< "Microsoft Visual Studio Solution File, Format Version 12.00"
    solutionPrinter <<< "Project(\"{8BC9CEB8-8B4A-11D0-8D11-00A0C91BC942}\") = \"exastencils\", \"exastencils.vcxproj\", \"{435911DB-0E04-462A-A3D6-00054AE0DB84}\""
    solutionPrinter <<< "EndProject"
    solutionPrinter <<< "Global"
    solutionPrinter <<< "\tGlobalSection(SolutionConfigurationPlatforms) = preSolution"
    solutionPrinter <<< "\t\tRelease|x64 = Release|x64"
    solutionPrinter <<< "\tEndGlobalSection"
    solutionPrinter <<< "\tGlobalSection(ProjectConfigurationPlatforms) = postSolution"
    solutionPrinter <<< "\t\t{435911DB-0E04-462A-A3D6-00054AE0DB84}.Release|x64.ActiveCfg = Release|x64"
    solutionPrinter <<< "\t\t{435911DB-0E04-462A-A3D6-00054AE0DB84}.Release|x64.Build.0 = Release|x64"
    solutionPrinter <<< "\tEndGlobalSection"
    solutionPrinter <<< "\tGlobalSection(SolutionProperties) = preSolution"
    solutionPrinter <<< "\t\tHideSolutionNode = FALSE"
    solutionPrinter <<< "\tEndGlobalSection"
    solutionPrinter <<< "EndGlobal"

    /// finalize

    projectPrinter.finish()
    solutionPrinter.finish()
  }
}
