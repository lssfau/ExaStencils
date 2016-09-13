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
import exastencils.languageprocessing.l4.UnfoldLevelSpecifications
import exastencils.mpi._
import exastencils.multiGrid._
import exastencils.omp._
import exastencils.parsers.l4._
import exastencils.prettyprinting._
import exastencils.spl.test.PredictionTests
import exastencils.strategies._
import exastencils.util._
import exastencils.spl.samplingStrategies._
import exastencils.optimization.MergeConditions
import exastencils.polyhedron.PolyOpt
import exastencils.optimization.AddressPrecalculation
import exastencils.optimization.TypeInference
import exastencils.optimization.SimplifyFloatExpressions
import exastencils.optimization.Vectorization
import exastencils.optimization.Unrolling
import scala.reflect.macros.blackbox
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.StandardOpenOption
import exastencils.spl.test.DomainKnowledgeTests
import exastencils.spl.samplingStrategies.doe._
import java.io._
import exastencils.spl.test.DomainKnowledgeTests
import exastencils.spl.samplingStrategies.heuristics.FWHeuristic
import exastencils.spl.learning._

object VariabilityParser {
  def main(args : Array[String]) : Unit = {

    //    var d : DomainKnowledgeTests = new DomainKnowledgeTests();
    //        d.main(null);

    //pollyTimeSizeTests()  

    //        val file = Settings.basePathPrefix + "/Compiler/src/exastencils/knowledge/Knowledge.scala"
    //        parseKnowlegdeFile(file)
    //        FeatureModel.printFeatureTree
    //
    //        
    //        var binaryFeatures : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    //        FeatureModel.allFeatures.values.foreach { x => if(!x.isNumerical) binaryFeatures.add(x)}
    //        
    //        var foo: FWHeuristic = new  FWHeuristic(binaryFeatures)
    //        println(foo.featureWiseConfigurations.size)
    //        foo.featureWiseConfigurations.foreach(f => {
    //            f.foreach(fe => println(fe._1.identifier + " -> "+fe._2))
    //            
    //        }
    //        
    //        )
    //        
    applySampling()

    //        generateVariant()

  }

  def pollyTimeSizeTests() = {

    // interpretieren des alten FeatueModelles
    FeatureModel.FAMASyntax_ReadFeatureModel("./../FeatureModel/PollyTests/model.model")
    //FeatureModel.printFeatureTree

    var nfps : Array[String] = new Array(1)
    nfps(0) = "time_perCycle"

    var pTest : PredictionTests = new PredictionTests()
    pTest.readFile("./../FeatureModel/PollyTests/pollyTileSize_greatMaxValue_all.txt", nfps)
    var allConfigs = pTest.allConfigs
    var specificModels : scala.collection.mutable.Set[Tuple4[Feature, NonFunctionalProperties.Value, Array[FFS_Expression], Jama.Matrix]] = scala.collection.mutable.Set()

    var testConfigs : scala.collection.mutable.Set[Configuration] = scala.collection.mutable.Set()

    var configOptions : scala.collection.mutable.Set[exastencils.spl.Feature] = scala.collection.mutable.Set()
    FeatureModel.allFeatures.foreach(x => configOptions.add(x._2))

    var ccd : CentralCompositeDesign = new CentralCompositeDesign(configOptions)

    ccd.getPoints().foreach(x => {
      x.foreach(y => { println(y._1.identifier + " -> " + y._2) })
      println("---")
    })

    var t = allConfigs.toArray[Configuration]

    var r = Random
    r.setSeed(42)
    for (d <- 1 to 74) {

      var pos = (allConfigs.size * r.nextDouble).toInt
      testConfigs.add(t(pos))

    }

    //    testConfigs = FeatureModel.filterConfigurationsAtLeastOne(pTest.allConfigs,ccd.getPoints())

    println(testConfigs.size)

    var features : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    FeatureModel.allFeatures.foreach(f => features.add(f._2))

    var forwardFeatureSelection = new ForwardFeatureSelection(features, 10, testConfigs.toArray[Configuration], "time_perCycle")
    forwardFeatureSelection.apply(false)

    var x = forwardFeatureSelection.getModelWithConstants(forwardFeatureSelection.solutionSet.toArray[FFS_Expression])

    println(forwardFeatureSelection.printModelWithConstants(x._1, x._2))

    var tuple = new Tuple3[String, Array[FFS_Expression], Jama.Matrix]("time_perCycle", x._2, x._1)

    DomainKnowledgeTests.write("./../FeatureModel/PollyTests/prediction.csv", pTest.predictConfigs(allConfigs, tuple, "time_perCycle"))
    println("-----------------------------------------------------------------")

    //predictConfigs(allConfigs,specificModels,NonFunctionalProperties.time_Overall )

    // predicten of all configs
    println("finished")

  }

  def applySampling() = {
    var numericalFeatures : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    FeatureModel.FAMASyntax_ReadFeatureModel("./../FeatureModel/PollyTests/model.model")
    FeatureModel.allFeatures.filter(x => x._2.identifier.contains("poly")).foreach(x => numericalFeatures.add(x._2))
    FeatureModel.allFeatures.foreach(x => println(x._2.identifier))
    println("all " + FeatureModel.allFeatures.size)
    numericalFeatures.foreach(x => println(x.identifier))

    var samplingDesign = new PlackettBurmanDesign(numericalFeatures);
    samplingDesign.initSeeds()
    samplingDesign.setSeed(7, 49)
    var points = samplingDesign.getPoints();

    points.foreach(x => {
      x.foreach(y => print(y._1.identifier + " -> " + y._2 + " ; "))
      println()
    })

    println(points.size)

    var configs : scala.collection.mutable.MutableList[Configuration] = scala.collection.mutable.MutableList()

    points.foreach(x => { var config = new Configuration(); config.readSolution(x); configs.+=(config) })

    for (a <- 0 to configs.size - 1) {
      configToKnowledgeFile(configs.get(a).get, a);
    }
    generateExecutionScript(configs.size - 1)
  }

  /**
    * Splits the given file and adds all features (defined by lines with an // [...]) to the feature model
    *
    */
  def parseKnowlegdeFile(fileName : String) = {
    var knowledgeFile = Source.fromFile(fileName).mkString.split("\n")

    knowledgeFile.foreach(x => {
      var currStatement = x
      if (currStatement.contains("//")) {
        var commentPart = currStatement.split("//", 2)(1)
        if (commentPart.contains("["))
          FeatureModel.addFeature_KnowledgeFile(currStatement)
        if (commentPart.contains("@constant")) {
          FeatureModel.addConstant(currStatement)
        }
      }
    })

    FeatureModel.createFeatureDependenciesByFeatureNames()
  }

  def useConfiguration(configuration : Configuration) = {
    Knowledge.getClass().getDeclaredFields().foreach(f =>
      if (FeatureModel.allFeatures.contains(f.getName())) {
        var feature = FeatureModel.allFeatures(f.getName())
        if (!feature.isXorFeature) {
          if (feature.isNumerical)
            UniversalSetter.apply(Knowledge, f.getName(), configuration.getValueOfNumericalFeature(feature.identifier))
          else if (configuration.boolFeatures.contains(feature)) {
            UniversalSetter.apply(Knowledge, f.getName(), true)
          }
        }
      })
  }

  def generateVariant() = {
    Knowledge.update()

    var testConfigs : Array[Configuration] = Array()

    var config = FeatureModel.getDefaultConfig()
    //    useConfiguration(config)

    //    println(config.toString)

    //    FeatureModel.printAllConfigurations

    //      FeatureModel.getFeatureWiseVariants_simple

    //    FeatureModel.asPropositionalFormula

    //    config.nfpValues = testMultiVariantGeneration("/blub/")

    //  testMultiVariantGeneration("E:/tmpGenerate")

    println("!!!!!!!!!Finished!!!!!!!!")

  }

  def configToKnowledgeFile(config : Configuration, index : Integer) = {
    var basicFile = "E:/LFA-Tool/ScalaExaStencil/Compiler/src/exastencils/spl/baseKnowledgeFile.knowledge";
    val source = scala.io.Source.fromFile(basicFile)
    val lines = source.mkString

    var newFilelocation = "E:/LFA-Tool/ScalaExaStencil/Compiler/configsCCI/knowlegdeFile_" + index + ".knowledge";

    val writer = new PrintWriter(new File(newFilelocation))

    lines.foreach(x => writer.write(x))

    // add configuration specific selection
    config.numericalFeatureValues.foreach(x => writer.write(x._1.identifier + " = " + x._2.toInt + "\n"))

    writer.flush()
    writer.close()
    source.close()

  }

  def generateExecutionScript(numberOfConfigs : Integer) = {
    var newFilelocation = "E:/LFA-Tool/ScalaExaStencil/Compiler/configs/scriptCCI.sh";

    val writer = new PrintWriter(new File(newFilelocation))

    for (a <- 0 to numberOfConfigs) {
      writer.write("mkdir ./config_" + a + "/\n")
      writer.write("exaGen Main knowlegdeFile_" + a + ".knowledge ./config_" + a + "/\n")
      writer.write("cd ./config_" + a + "\n")
      writer.write("srun -A spl -p chimaira -n 8 -c 10 --hint=nomultithread ./exastencils.exe >> ../result_" + a + ".log\n")
      writer.write("cd ..\n")
      writer.write("rm -r ./config_" + a + "\n")
    }
    writer.flush()
    writer.close()

  }

  def write(filePath : String, contents : String) = {
    Files.write(Paths.get(filePath), contents.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE)
  }

  def testMultiVariantGeneration(location : String) = {

    //    val start : Long = System.nanoTime()
    //    
    //    Settings.outputPath = location
    //
    //    
    //    // HACK: this will setup a dummy L4 DSL file
    //    StateManager.root_ = new l3.Root
    //    StateManager.root_.asInstanceOf[l3.Root].printToL4(Settings.basePathPrefix + "/Compiler/dsl/Layer4.exa")
    //
    //    // HACK: this tests the new L4 capabilities
    //    var parserl4 = new ParserL4
    //    StateManager.root_ = parserl4.parseFile(Settings.basePathPrefix + "/Compiler/dsl/Layer4.exa")
    //    ValidationL4.apply
    //    ProgressToIr.apply()
    //
    //    // TODO: integrate the next line into the ProgressToIr Strategy
    //    StateManager.root_ = StateManager.root_.asInstanceOf[l4.ProgressableToIr].progress.asInstanceOf[Node]
    //
    //    // Setup tree
    //    StateManager.root_.asInstanceOf[ir.Root].nodes ++= List(
    //      // FunctionCollections
    //      new DomainFunctions,
    //      new CommunicationFunctions,
    //
    //      // Util
    //      new Log,
    //      new Stopwatch,
    //      new Vector)
    //
    //    // Strategies
    //
    //    AddDefaultGlobals.apply()
    //
    //    SimplifyStrategy.doUntilDone() // removes (conditional) calls to communication functions that are not possible
    //
    //    SetupDataStructures.apply()
    //    SetupCommunication.apply()
    //
    //    ResolveSpecialFunctions.apply()
    //
    //    ResolveLoopOverPoints.apply()
    //    ResolveIntergridIndices.apply()
    //
    //    var numConvFound = 1
    //    while (numConvFound > 0) {
    //      FindStencilConvolutions.apply()
    //      numConvFound = FindStencilConvolutions.results.last._2.matches
    //      if (Knowledge.useFasterExpand)
    //        ExpandOnePassStrategy.apply()
    //      else
    //        ExpandStrategy.doUntilDone()
    //    }
    //
    //    ResolveContractingLoop.apply()
    //
    //    MapStencilAssignments.apply()
    //    if (Knowledge.useFasterExpand)
    //      ExpandOnePassStrategy.apply()
    //    else
    //      ExpandStrategy.doUntilDone()
    //
    //    MergeConditions.apply()
    //
    //    if (Knowledge.poly_usePolyOpt)
    //      PolyOpt.apply()
    //
    //    ResolveLoopOverDimensions.apply()
    //    ResolveSlotOperationsStrategy.apply()
    //
    //    ResolveIndexOffsets.apply()
    //    LinearizeFieldAccesses.apply()
    //
    //    if (Knowledge.useFasterExpand)
    //      ExpandOnePassStrategy.apply()
    //    else
    //      ExpandStrategy.doUntilDone()
    //
    //    if (!Knowledge.useMPI)
    //      RemoveMPIReferences.apply()
    //
    //    SimplifyStrategy.doUntilDone()
    //
    //    if (Knowledge.opt_useAddressPrecalc)
    //      AddressPrecalculation.apply()
    //
    //    TypeInference.apply()
    //
    //    SimplifyFloatExpressions.apply()
    //    SimplifyStrategy.doUntilDone()
    //
    //    if (Knowledge.opt_vectorize)
    //      Vectorization.apply()
    //
    //    if (Knowledge.opt_unroll > 1)
    //      Unrolling.apply()
    //
    //    AddInternalVariables.apply()
    //
    //    if (Knowledge.useMPI)
    //      AddMPIDatatypes.apply()
    //
    //    if (Knowledge.useOMP)
    //      exastencils.omp.AddOMPPragmas.apply()
    //
    //    // one last time
    //    if (Knowledge.useFasterExpand)
    //      ExpandOnePassStrategy.apply()
    //    else
    //      ExpandStrategy.doUntilDone()
    //    SimplifyStrategy.doUntilDone()
    //
    //    PrintStrategy.apply()
    //    PrettyprintingManager.finish
    //
    //    println("Done!")
    //
    //    println("Runtime: " + ((System.nanoTime() - start) / 1e9))
    //    (new CountingStrategy("number of printed nodes")).apply()

  }

}

