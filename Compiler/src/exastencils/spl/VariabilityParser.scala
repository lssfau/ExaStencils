package exastencils.spl

import scala.io.Source
import scala.util.Random

import exastencils.communication._
import exastencils.core._
import exastencils.data._
import exastencils.datastructures._
import exastencils.domain._
import exastencils.globals._
import exastencils.knowledge._
import exastencils.languageprocessing.l4.ProgressToIr
import exastencils.mpi._
import exastencils.multiGrid._
import exastencils.omp._
import exastencils.parsers.l4._
import exastencils.prettyprinting._
import exastencils.spl.test.PredictionTests
import exastencils.strategies._
import exastencils.util._

object VariabilityParser {
  def main(args : Array[String]) : Unit = {

    //    val file = Settings.basePathPrefix + "/Compiler/src/exastencils/knowledge/Knowledge.scala"
    //    readFeaturesL4(file)
    //
    //    generateVariant()

    testMachineLearningAlgorithms

  }

  /**
    * Splits the given file and adds all features (defined by lines with an // [...]) to the feature model
    *
    */
  def readFeaturesL4(fileName : String) = {
    var knowledgeFile = Source.fromFile(fileName).mkString.split("\n")

    for (i <- 0 until knowledgeFile.length - 1) {
      var currStatement = knowledgeFile(i)
      if (currStatement.contains("//") && currStatement.contains("[")) {
        FeatureModel.addFeature_KnowledgeFile(currStatement)
      }
    }
    FeatureModel.createFeatureDependenciesByFeatureNames()
  }

  def generateVariant() = {
    Knowledge.update()

    var testConfigs : Array[Configuration] = Array()

    var config = FeatureModel.getDefaultConfig()
    useConfiguration(config)

    config.nfpValues = testMultiVariantGeneration("/blub/")

    println("!!!!!!!!!Finished!!!!!!!!")

  }

  val rand : Random = new Random(1)
  /**
    * This method uses the old feature model syntax (FAMA like syntax) and uses the re
    *
    */
  def testMachineLearningAlgorithms() = {

    // interpretieren des alten FeatueModelles
    FeatureModel.FAMASyntax_ReadFeatureModel("./featureModel/model_HSMGP_noCores_numerical.model")
    //    FeatureModel.FAMASyntax_ReadFeatureModel("./featureModel/model_HSMGP_noCores.model")
    var pTest : PredictionTests = new PredictionTests()
    pTest.readFile("./src/exastencils/spl/test/P2D_minimalOnlyAvgTime.txt")
    var allConfigs = pTest.allConfigs.toArray[Configuration]

    // verwenden einige Konfigurationen als Trainigset
    var testConfigs : scala.collection.mutable.Set[Configuration] = scala.collection.mutable.Set()

    for (i <- 1 to 30) {
      var pos = (rand.nextDouble * pTest.allConfigs.size).toInt
      testConfigs.add(allConfigs(pos))
    }

    var forwardFeatureSelection = new ForwardFeatureSelection(15, testConfigs.toArray[Configuration])
    forwardFeatureSelection.apply

    println("-------------------------------------------------------------------------------------")
    println("Overall Error " + forwardFeatureSelection.computeErrorForCombination(forwardFeatureSelection.solutionSet.toArray[scala.collection.mutable.Set[Feature]], allConfigs))

    // predicten of all configs
    println("finished")

  }

  def useConfiguration(configuration : Configuration) = {
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

  def testMultiVariantGeneration(location : String) : NonFunctionalProperties = {
    Settings.outputPath = location

    // HACK: this tests the new L4 capabilities
    var parserl4 = new ParserL4
    StateManager.root_ = parserl4.parseFile(Settings.basePathPrefix + "/Compiler/dsl/newDSL4.exa")
    ValidationL4.apply
    ProgressToIr.apply()

    StateManager.root_ = StateManager.root_.asInstanceOf[l4.ProgressableToIr].progressToIr.asInstanceOf[Node]

    // Setup tree
    StateManager.root_.asInstanceOf[ir.Root].nodes ++= List(
      // FunctionCollections
      new DomainFunctions,
      new CommunicationFunctions,

      // Util
      new Stopwatch,
      new Vector,

      // Globals
      new Globals)

    // Strategies

    FindStencilConvolutions.apply()

    ResolveSpecialFunctions.apply()

    SetupDataStructures.apply()

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

    do { SimplifyStrategy.apply() }
    while (SimplifyStrategy.results.last._2.matches > 0) // FIXME: cleaner code

    AddInternalVariables.apply()

    if (Knowledge.useOMP) {
      AddOMPPragmas.apply()
    }

    { new CountingStrategy("1") }.apply()

    PrintStrategy.apply()
    PrettyprintingManager.finish

    println("Compiation Done!")

    // Execute program

    //Runtime.getRuntime().exec(location+"external_program")

    return new NonFunctionalProperties()

  }

}

