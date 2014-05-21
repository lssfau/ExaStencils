package exastencils.spl

import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.strategies._
import exastencils.application._
import exastencils.domain._
import exastencils.multiGrid._
import exastencils.primitives._
import exastencils.util._
import exastencils.globals._
import exastencils.prettyprinting.PrettyprintingManager
import harald_dep.Parser._
import harald_dep.dsl._
import harald_dep.Generate._
import harald_dep.ast._
import exastencils.parsers.l4.ParserL4
import exastencils.parsers.l4.ValidationL4
import exastencils.datastructures.l4.ProgressableToIr
import exastencils.languageprocessing.l4.ProgressToIr
import exastencils.mpi._
import exastencils.omp._
import exastencils.polyhedron._

import scala.io.Source
import exastencils.core.Settings
import exastencils.knowledge.Knowledge
import exastencils.core.UniversalSetter

object VariabilityParser {
  def main(args: Array[String]): Unit = {

    val file = Settings.basePathPrefix + "/Compiler/src/exastencils/knowledge/Knowledge.scala"
    readFeaturesL4(file)

    generateVariant()

  }

  /**
   * Splits the given file and adds all features (defined by lines with an // [...]) to the feature model
   *
   */
  def readFeaturesL4(fileName: String) = {
    var knowledgeFile = Source.fromFile(fileName).mkString.split("\n")

    for (i <- 0 until knowledgeFile.length - 1) {
      var currStatement = knowledgeFile(i)
      if (currStatement.contains("//") && currStatement.contains("[")) {
        FeatureModel.addFeatureL4(currStatement)
      }
    }
    FeatureModel.createFeatureDependenciesByFeatureNames()
  }

  def generateVariant() = {
    Knowledge.update()

    var config = FeatureModel.getDefaultConfig()
    useConfiguration(config)

    testMultiVariantGeneration("/bla/")


    config.nfpValues = testMultiVariantGeneration("/blub/")


    
    
  }

  def useConfiguration(configuration: Configuration) = {
    Knowledge.getClass().getDeclaredFields().foreach(f =>
      if (FeatureModel.allFeatures.contains(f.getName())) {
        var feature = FeatureModel.allFeatures(f.getName())
        if (feature.isNumerical)
          UniversalSetter.apply(Knowledge, f.getName(), configuration.getValueOfNumericalFeature(feature.identifier))
        else if (configuration.selectedBoolFeatures.contains(feature)) {
          UniversalSetter.apply(Knowledge, f.getName(), true)
        }
      })
  }

  def testMultiVariantGeneration(location: String) : NonFunctionalProperties = {
    Settings.outputPath = location

    // Hack paths (relative paths should work here, too, if not, reverse this change)
    // ... this obviously depends on the execution path which in my case is the root folder to include configs and scripts
    val DSLpath = Settings.basePathPrefix + "/Compiler/src/harald_dep/testmg/"
    val problem = "testDSL"
    val outputfile = "main.cpp"

    // HACK: this tests the new L4 capabilities
    var parserl4 = new ParserL4
    StateManager.root_ = parserl4.parseFile(Settings.basePathPrefix + "/Compiler/dsl/newDSL4.exa")
    ValidationL4.apply
    ProgressToIr.apply()

    StateManager.root_ = StateManager.root_.asInstanceOf[ProgressableToIr].progressToIr.asInstanceOf[Node]

    // Setup tree
    StateManager.root_.asInstanceOf[Root].nodes ++= List(
      // Application
      new Poisson3D,

      // Domain
      new DomainGenerated,

      // Primitives
      new FragmentClass,
      new CommunicationFunctions,

      // Util
      new Log,
      new Stopwatch,
      new Vector,

      // Globals
      new Globals)

    // Strategies

    FindStencilConvolutions.apply()

    ResolveSpecialFunctions.apply()

    SetupFragmentClass.apply()

    do { ExpandStrategy.apply() }
    while (ExpandStrategy.results.last._2.matches > 0) // FIXME: cleaner code

    do { ExpandStrategy.apply() }
    while (ExpandStrategy.results.last._2.matches > 0) // FIXME: cleaner code

    //    PolyOpt.apply()

    ResolveLoopOverDimensions.apply()

    LinearizeFieldAccesses.apply()

    do { ExpandStrategy.apply() }
    while (ExpandStrategy.results.last._2.matches > 0) // FIXME: cleaner code

    if (!Knowledge.useMPI) {
      RemoveMPIReferences.apply()
    }

    var i = 0
    val s = DefaultStrategy("counting", List(Transformation("bla", { case x => i += 1; x })))
    s.apply()
    Logger.warn(s"counter: i = $i")

    do { SimplifyStrategy.apply() }
    while (SimplifyStrategy.results.last._2.matches > 0) // FIXME: cleaner code

    AddMemberFunctionPrefix.apply()

    if (Knowledge.useOMP) {
      AddOMPPragmas.apply()
    }

    PrintStrategy.apply()
    PrettyprintingManager.finish

    println("Compiation Done!")

    // Execute program
    
    //Runtime.getRuntime().exec(location+"external_program")
    
    
    
    
    return new NonFunctionalProperties()
    
  }

}

