package exastencils.spl.test

import scala.util.Random

import java.nio.charset.StandardCharsets
import java.nio.file._

import exastencils.config._
import exastencils.core._
import exastencils.spl._
import exastencils.spl.learning._
import exastencils.spl.samplingStrategies.doe._

object DomainKnowledgeTests {

  val locationModel : String = "./../FeatureModel/DomainKnowledgeTest/model_HSMGP_withCores_numerical.model"
  val locationMeasurements : String = "./../FeatureModel/DomainKnowledgeTest/PDA_numCoresAll.txt";

  val prefixResultFiles : String = "./../FeatureModel/DomainKnowledgeTest/ResultFiles/"

  def main(args : Array[String]) : Unit = {

    //    testMachineLearningAlgorithms
    testMachineLearningAlgorithmsWithDomainKnowledge
  }

  def testMachineLearningAlgorithms() = {

    // interpretieren des alten FeatueModelles
    FeatureModel.FAMASyntax_ReadFeatureModel(locationModel)
    //   FeatureModel.printFeatureTree

    var pTest : PredictionTests = new PredictionTests()

    var nfps : Array[String] = new Array(3);
    nfps(0) = "numberIterations"
    nfps(1) = "time_Overall"
    nfps(2) = "time_perCycle"

    pTest.readFile(locationMeasurements, nfps)
    var allConfigs = pTest.allConfigs
    allConfigs.foreach { x => {
      x.boolFeatures.put(FeatureModel.get("cgs"), true)
    }
    }

    var t = allConfigs.toArray[Configuration]

    var errorList : scala.collection.mutable.MutableList[Double] = scala.collection.mutable.MutableList()
    var sumErrors : Double = 0.0

    for (i <- 1 to 5) {
      var specificModels : scala.collection.mutable.Set[Tuple4[Feature, String, Array[FFS_Expression], Jama.Matrix]] = scala.collection.mutable.Set()
      var testConfigs : scala.collection.mutable.Set[Configuration] = scala.collection.mutable.Set()

      var r = Random
      r.setSeed(i)
      for (d <- 1 to 86) {

        var pos = (allConfigs.size * r.nextDouble).toInt
        testConfigs.add(t(pos))

      }
      var features : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
      FeatureModel.allFeatures.foreach(f => features.add(f._2))

      var forwardFeatureSelection = new ForwardFeatureSelection(features, 20, testConfigs.toArray[Configuration], "time_perCycle")
      forwardFeatureSelection.apply(false)
      forwardFeatureSelection.perform()

      var x = forwardFeatureSelection.getModelWithConstants(forwardFeatureSelection.solutionSet.toArray[FFS_Expression])

      specificModels.add(new Tuple4[Feature, String, Array[FFS_Expression], Jama.Matrix](FeatureModel.allFeatures("cgs"), "time_perCycle", x._2, x._1))

      forwardFeatureSelection = new ForwardFeatureSelection(features, 20, testConfigs.toArray[Configuration], "numberIterations")
      forwardFeatureSelection.apply(false)
      forwardFeatureSelection.perform()

      x = forwardFeatureSelection.getModelWithConstants(forwardFeatureSelection.solutionSet.toArray[FFS_Expression])

      specificModels.add(new Tuple4[Feature, String, Array[FFS_Expression], Jama.Matrix](FeatureModel.allFeatures("cgs"), "numberIterations", x._2, x._1))

      println("-----------------------------------------------------------------")

      var avgError : Double = getAvgErrorConfigs(allConfigs, specificModels, "time_Overall")
      errorList.+=(avgError)
      sumErrors += avgError

      write(prefixResultFiles + "Random_" + i + ".csv", "predicted;measured\n" + predictConfigs(allConfigs, specificModels, "time_Overall"))
      restartLearning()

    }

    sumErrors = sumErrors / errorList.size

    var minDistToAvg = Double.MaxValue
    var index = -1
    for (a <- 0 to errorList.size - 1) {
      if (Math.abs(sumErrors.toDouble - errorList(a)) < minDistToAvg) {

        index = a
        minDistToAvg = Math.abs(sumErrors.toDouble - errorList(a))
      }
    }

    println("random seet " + index + " has the average error")

    // predicten of all configs
    println("finished")

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

  var configsConsidered : scala.collection.mutable.Set[Configuration] = scala.collection.mutable.Set()
  var specificModels : scala.collection.mutable.Set[Tuple4[Feature, String, Array[FFS_Expression], Jama.Matrix]] = scala.collection.mutable.Set()

  def learnModelSmootherPrePost(allConfigs : scala.collection.mutable.Set[Configuration], nfp : String, useAllConfigsForError : Boolean) : String = {

    var f = FeatureModel.allFeatures

    // for each smoother learn a #influence model considering pre and post 
    FeatureModel.parentChildRelationships(FeatureModel.allFeatures("smoother")).foreach(x => {
      var y = generateModelDependingOnPreAndPost(x, nfp, allConfigs);

      specificModels.add(new Tuple4[Feature, String, Array[FFS_Expression], Jama.Matrix](x, nfp, y._2, y._1))
    })

    var cgs : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    cgs.add(FeatureModel.getSpecificFeatureDkPaperTest("cgs"))

    var configs = FeatureModel.filterConfigurations(allConfigs, cgs)

    var cores : scala.collection.mutable.Map[Feature, Double] = scala.collection.mutable.Map()
    cores.put(FeatureModel.getSpecificFeatureDkPaperTest("cores"), 64)
    var configsCores = FeatureModel.filterConfigurations(configs, cores)

    var returnString : String = ""

    if (nfp.equals("numberIterations"))
      returnString = predictConfigs(configsCores, specificModels, nfp)
    if (nfp.equals("time_perCycle"))
      returnString = predictConfigs(configsCores, specificModels, nfp)

    //    if (useAllConfigsForError) {
    //      if (nfp.equals("numberIterations"))
    //        returnString = predictConfigs(allConfigs, specificModels, nfp)
    //      if (nfp.equals("time_perCycle"))
    //        returnString = predictConfigs(allConfigs, specificModels, nfp)
    //    } else {
    //      if (nfp.equals("numberIterations"))
    //        returnString = predictConfigs(configsCores, specificModels, nfp)
    //      if (nfp.equals("time_perCycle"))
    //        returnString = predictConfigs(configsCores, specificModels, nfp)
    //    }

    println("---------------------" + configsConsidered.size + "--------------------------------------------------------")
    return returnString
  }

  def learnModelSmootherPrePostCgs(allConfigs : scala.collection.mutable.Set[Configuration]) : String = {

    // for each smoother learn a #influence model considering pre and post 
    FeatureModel.parentChildRelationships(FeatureModel.allFeatures("smoother")).foreach(x => {
      var y = generateModelDependingOnPreAndPost(x, "time_perCycle", allConfigs);

      specificModels.add(new Tuple4[Feature, String, Array[FFS_Expression], Jama.Matrix](x, "time_perCycle", y._2, y._1))
    })

    // delta Model for cgs
    var cgsModel = generateModelDependingOnCGS("time_perCycle", allConfigs, specificModels)
    specificModels.add(new Tuple4[Feature, String, Array[FFS_Expression], Jama.Matrix](FeatureModel.allFeatures("cgs"), "time_perCycle", cgsModel._2, cgsModel._1))

    var cores : scala.collection.mutable.Map[Feature, Double] = scala.collection.mutable.Map()
    cores.put(FeatureModel.getSpecificFeatureDkPaperTest("cores"), 64)
    var configs = FeatureModel.filterConfigurations(allConfigs, cores)

    println("---------------------" + configs.size + "--------------------------------------------------------")

    return predictConfigs(configs, specificModels, "time_perCycle")
  }

  def learnModelSmootherPrePostCgsCores(allConfigs : scala.collection.mutable.Set[Configuration]) : String = {

    // for each smoother learn a #influence model considering pre and post 
    FeatureModel.parentChildRelationships(FeatureModel.allFeatures("smoother")).foreach(x => {
      var y = generateModelDependingOnPreAndPost(x, "time_perCycle", allConfigs);

      specificModels.add(new Tuple4[Feature, String, Array[FFS_Expression], Jama.Matrix](x, "time_perCycle", y._2, y._1))
    })

    // delta Model for cgs
    var cgsModel = generateModelDependingOnCGS("time_perCycle", allConfigs, specificModels)
    specificModels.add(new Tuple4[Feature, String, Array[FFS_Expression], Jama.Matrix](FeatureModel.allFeatures("cgs"), "time_perCycle", cgsModel._2, cgsModel._1))

    // consider the number of cores
    FeatureModel.parentChildRelationships(FeatureModel.allFeatures("smoother")).foreach(x => {
      var coresModel = generateModelDependingOnCores(x, "time_perCycle", allConfigs, specificModels)
      specificModels.add(new Tuple4[Feature, String, Array[FFS_Expression], Jama.Matrix](x, "time_perCycle", coresModel._2, coresModel._1))
    })

    println("---------------------" + configsConsidered.size + "--------------------------------------------------------")

    return predictConfigs(allConfigs, specificModels, "time_perCycle")
  }

  def learnModelSmootherPrePostCgsCoresOverallTime(allConfigs : scala.collection.mutable.Set[Configuration]) : String = {

    // for each smoother learn a #influence model considering pre and post 
    FeatureModel.parentChildRelationships(FeatureModel.allFeatures("smoother")).foreach(x => {
      var y = generateModelDependingOnPreAndPost(x, "time_perCycle", allConfigs);

      specificModels.add(new Tuple4[Feature, String, Array[FFS_Expression], Jama.Matrix](x, "time_perCycle", y._2, y._1))
    })

    FeatureModel.parentChildRelationships(FeatureModel.allFeatures("smoother")).foreach(x => {
      var y = generateModelDependingOnPreAndPost(x, "numberIterations", allConfigs);

      specificModels.add(new Tuple4[Feature, String, Array[FFS_Expression], Jama.Matrix](x, "numberIterations", y._2, y._1))
    })

    // delta Model for cgs
    var cgsModel = generateModelDependingOnCGS("time_perCycle", allConfigs, specificModels)
    specificModels.add(new Tuple4[Feature, String, Array[FFS_Expression], Jama.Matrix](FeatureModel.allFeatures("cgs"), "time_perCycle", cgsModel._2, cgsModel._1))

    // consider the number of cores
    FeatureModel.parentChildRelationships(FeatureModel.allFeatures("smoother")).foreach(x => {
      var coresModel = generateModelDependingOnCores(x, "time_perCycle", allConfigs, specificModels)
      specificModels.add(new Tuple4[Feature, String, Array[FFS_Expression], Jama.Matrix](x, "time_perCycle", coresModel._2, coresModel._1))
    })

    println("---------------------" + configsConsidered.size + "--------------------------------------------------------")

    return predictConfigs(allConfigs, specificModels, "time_Overall")
  }

  def restartLearning() = {
    configsConsidered = scala.collection.mutable.Set()
    specificModels = scala.collection.mutable.Set()
  }

  def testMachineLearningAlgorithmsWithDomainKnowledge() = {

    // interpretieren des alten FeatueModelles
    FeatureModel.FAMASyntax_ReadFeatureModel(locationModel)
    //FeatureModel.printFeatureTree

    var nfps : Array[String] = new Array(3);
    nfps(0) = "numberIterations"
    nfps(1) = "time_Overall"
    nfps(2) = "time_perCycle"

    var pTest : PredictionTests = new PredictionTests()
    pTest.readFile(locationMeasurements, nfps)
    var allConfigs = pTest.allConfigs

    allConfigs.foreach { x => {
      x.boolFeatures.put(FeatureModel.get("cgs"), true)
    }
    }

    println("-----------------------------# of Iterations Model for Smoother Pre Post-------")
    write(prefixResultFiles + "NumIt_OnlyOneCGSAndOneCore.csv", "predicted;measured\n" + learnModelSmootherPrePost(allConfigs, "numberIterations", false))
    restartLearning()

    println("-----------------------------# of Iterations Model for Smoother Pre Post-------")
    write(prefixResultFiles + "NumIt_OnlyOneCGSAndOneCore_allConfigs.csv", "predicted;measured\n" + learnModelSmootherPrePost(allConfigs, "numberIterations", true))
    restartLearning()

    println("-----------------------------Time Per Iteration Model for Smoother Pre Post-------")
    write(prefixResultFiles + "TimePerIt.csv", "predicted;measured\n" + learnModelSmootherPrePost(allConfigs, "time_perCycle", false))
    restartLearning()

    println("-----------------------------Time Per Iteration Model for Smoother Pre Post CGS-------")
    write(prefixResultFiles + "TimePerItCGS.csv", "predicted;measured\n" + learnModelSmootherPrePostCgs(allConfigs))
    restartLearning()

    println("-----------------------------Time Per Iteration Model for Smoother Pre Post CGS Cores-------")
    write(prefixResultFiles + "TimePerItCGSCores.csv", "predicted;measured\n" + learnModelSmootherPrePostCgsCores(allConfigs))
    restartLearning()

    println("-----------------------------All-------")
    write(prefixResultFiles + "OverallTime.csv", "predicted;measured\n" + learnModelSmootherPrePostCgsCoresOverallTime(allConfigs))
    restartLearning()

    println(configsConsidered.size)

    println("-----------------------------------------------------------------")

    predictConfigs(allConfigs, specificModels, "time_Overall")

    // predicten of all configs
    println("finished")

  }

  def write(filePath : String, contents : String) = {
    Files.write(Paths.get(filePath), contents.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE)
  }

  def predictConfigs(configurations : scala.collection.mutable.Set[Configuration], models : scala.collection.mutable.Set[Tuple4[Feature, String, Array[FFS_Expression], Jama.Matrix]], nfp : String) : String = {

    var ffs = new ForwardFeatureSelection(null, 20, null, null)

    var sb : StringBuilder = new StringBuilder()

    var sumError = 0.0

    for (config <- configurations) {
      var modelsOfConfig = models.filter(x => config.boolFeatures.contains(x._1))
      println(modelsOfConfig.size)
      var modelsNumIteration = modelsOfConfig.filter(_._2.equals("numberIterations"))
      var modelsTimePerCycle = modelsOfConfig.filter(_._2.equals("time_perCycle"))

      var numIterations = 0.0
      modelsNumIteration.foreach(x => numIterations += ffs.predictConfig(x._4, x._3, config))

      var timePerCycle = 0.0
      modelsTimePerCycle.foreach(x => timePerCycle += ffs.predictConfig(x._4, x._3, config))

      var predictedValue = 0.0
      var measuredValue = 0.0

      if (nfp.equals("time_Overall")) {
        predictedValue = Math.rint(numIterations).toInt * timePerCycle
        measuredValue = config.nfpValues("time_Overall")
      } else if (nfp.equals("time_perCycle")) {
        predictedValue = timePerCycle
        predictedValue = BigDecimal(predictedValue).setScale(5, BigDecimal.RoundingMode.HALF_UP).toDouble
        measuredValue = config.nfpValues("time_perCycle")
      } else if (nfp.equals("numberIterations")) {
        predictedValue = Math.rint(numIterations).toInt
        measuredValue = config.nfpValues("numberIterations")
      }

      //    	sb. append(config.CSV_String() + predictedValue.toString.replace(".", ",") + ";" + measuredValue.toString.replace(".", ",")+"\n")
      //      sb.append(predictedValue.toString + ";" + measuredValue.toString + "\n")
      sb.append(predictedValue.toString + ";" + measuredValue.toString + "\n")
      sumError += Math.abs(predictedValue - measuredValue)

    }
    println("Sum of the errors " + sumError)
    //    sb. append(sumError / configurations.size)
    return sb.toString
  }

  def getAvgErrorConfigs(configurations : scala.collection.mutable.Set[Configuration], models : scala.collection.mutable.Set[Tuple4[Feature, String, Array[FFS_Expression], Jama.Matrix]], nfp : String) : Double = {

    var ffs = new ForwardFeatureSelection(null, 20, null, null)

    var sb : StringBuilder = new StringBuilder()

    var sumError = 0.0

    for (config <- configurations) {
      var modelsOfConfig = models.filter(x => config.boolFeatures.contains(x._1))

      var modelsNumIteration = modelsOfConfig.filter(_._2.equals("numberIterations"))
      var modelsTimePerCycle = modelsOfConfig.filter(_._2.equals("time_perCycle"))

      var numIterations = 0.0
      modelsNumIteration.foreach(x => numIterations += ffs.predictConfig(x._4, x._3, config))

      var timePerCycle = 0.0
      modelsTimePerCycle.foreach(x => timePerCycle += ffs.predictConfig(x._4, x._3, config))

      var predictedValue = 0.0
      var measuredValue = 0.0

      if (nfp.equals("time_Overall")) {
        predictedValue = Math.rint(numIterations).toInt * timePerCycle
        measuredValue = config.nfpValues("time_Overall")
      } else if (nfp.equals("time_perCycle")) {
        predictedValue = timePerCycle
        measuredValue = config.nfpValues("time_perCycle")
      } else if (nfp.equals("numberIterations")) {
        predictedValue = Math.rint(numIterations).toInt
        measuredValue = config.nfpValues("numberIterations")
      }

      //      sb. append(config.CSV_String() + predictedValue.toString.replace(".", ",") + ";" + measuredValue.toString.replace(".", ",")+"\n")
      sb.append(predictedValue.toString.replace(".", ",") + ";" + measuredValue.toString.replace(".", ",") + "\n")
      sumError += Math.abs(predictedValue - measuredValue) / measuredValue

    }
    //    sb. append(sumError / configurations.size)
    return sumError / configurations.size
  }

  def predictConfigs2(configurations : scala.collection.mutable.Set[Configuration], models : scala.collection.mutable.Set[Tuple4[Feature, NonFunctionalProperties.Value, Array[FFS_Expression], Jama.Matrix]]) = {

    var ffs = new ForwardFeatureSelection(null, 20, null, null)

    var sumError = 0.0

    for (config <- configurations) {

      var timeOModels = models.filter(_._2.equals("time_Overall"))
      var timeO = 0.0
      timeOModels.foreach(x => timeO += ffs.predictConfig(x._4, x._3, config))
      //    	numIterations = numIterations/modelsNumIteration.size

      println(timeO + "  " + config.nfpValues("time_Overall"))

      sumError += Math.abs((timeO) - config.nfpValues("time_Overall"))
    }
    println(sumError / configurations.size)
  }

  def predictConfiguration(configuration : Configuration, nfp : String, models : scala.collection.mutable.Set[Tuple4[Feature, String, Array[FFS_Expression], Jama.Matrix]]) : Double = {

    var ffs = new ForwardFeatureSelection(null, 20, null, null)

    var modelsOfConfig = models.filter(x => configuration.boolFeatures.contains(x._1))

    var modelsWithNfp = modelsOfConfig.filter(_._2 == nfp)

    var predictedValue = 0.0
    modelsWithNfp.foreach(x => predictedValue += ffs.predictConfig(x._4, x._3, configuration))

    return predictedValue
  }

  def generateModelDependingOnCGS(nfp : String, allConfigs : scala.collection.mutable.Set[Configuration], learnedModels : scala.collection.mutable.Set[Tuple4[Feature, String, Array[FFS_Expression], Jama.Matrix]]) : Tuple2[Jama.Matrix, Array[FFS_Expression]] = {

    println("------------------------CGS---------------------------------")

    var cgsFeatures : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    var cgsFeaturesWithName : scala.collection.mutable.Map[String, Feature] = scala.collection.mutable.Map()
    FeatureModel.parentChildRelationships(FeatureModel.allFeatures("cgs")).foreach(x => {
      cgsFeatures.add(x)
      cgsFeaturesWithName.put(x.identifier, x)
    })

    var selectedNumFeatures : scala.collection.mutable.Map[Feature, Double] = scala.collection.mutable.Map()
    selectedNumFeatures.put(FeatureModel.getSpecificFeatureDkPaperTest("pre"), 0)
    selectedNumFeatures.put(FeatureModel.getSpecificFeatureDkPaperTest("post"), 1)
    selectedNumFeatures.put(FeatureModel.getSpecificFeatureDkPaperTest("cores"), 64)

    var configsByPrePostAndDomain = FeatureModel.filterConfigurations(allConfigs, selectedNumFeatures)
    //		println("by pre/post/domain  "+configsByPrePostAndDomain.size)

    var selectedSmoother : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    selectedSmoother.add(FeatureModel.getSpecificFeatureDkPaperTest("Smoother"))
    var configsBySmootherAndDomain = FeatureModel.filterConfigurations(configsByPrePostAndDomain, selectedSmoother)
    //		println("by smoother "+configsBySmootherAndDomain.size)

    var configurationOffset : scala.collection.mutable.Map[Configuration, Double] = scala.collection.mutable.Map()
    configsBySmootherAndDomain.foreach(x => configurationOffset.put(x, predictConfiguration(x, nfp, learnedModels)))

    configsBySmootherAndDomain.foreach(x => if (!configsConsidered.contains(x)) configsConsidered.add(x))

    var cgs : scala.collection.mutable.Set[FFS_Expression] = scala.collection.mutable.Set()
    cgs.add(new FFS_Expression(cgsFeaturesWithName, "RED_AMG"))
    //    cgs.add(new FFS_Expression(cgsFeaturesWithName, "IP_CG"))
    cgs.add(new FFS_Expression(cgsFeaturesWithName, "IP_AMG"))

    var cgsName : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()
    cgsName.add("RED_AMG")
    //    cgsName.add("IP_CG")
    cgsName.add("IP_AMG")

    var forwardFeatureSelection = new ForwardFeatureSelection(cgsFeatures, 20, configsBySmootherAndDomain.toArray[Configuration], nfp)
    forwardFeatureSelection.applyWithOffset(configurationOffset)
    //    forwardFeatureSelection.perform(cgsName)

    var x = forwardFeatureSelection.getModelWithConstants(cgs.toArray[FFS_Expression])

    //    var x = forwardFeatureSelection.getModelWithConstants(forwardFeatureSelection.solutionSet.toArray[FFS_Expression])
    println(forwardFeatureSelection.printModelWithConstants(x._1, x._2))

    return x
  }

  def generateModelDependingOnCores(smootherFeature : Feature, nfp : String, allConfigs : scala.collection.mutable.Set[Configuration], learnedModels : scala.collection.mutable.Set[Tuple4[Feature, String, Array[FFS_Expression], Jama.Matrix]]) : Tuple2[Jama.Matrix, Array[FFS_Expression]] = {

    println("------------------------Cores---------------------------------")

    var coresFeature : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    coresFeature.add(FeatureModel.allFeatures("cores"))

    var selectedNumFeatures : scala.collection.mutable.Map[Feature, Double] = scala.collection.mutable.Map()
    selectedNumFeatures.put(FeatureModel.getSpecificFeatureDkPaperTest("pre"), 3)
    selectedNumFeatures.put(FeatureModel.getSpecificFeatureDkPaperTest("post"), 3)

    var configsByPrePostAndDomain = FeatureModel.filterConfigurations(allConfigs, selectedNumFeatures)
    //		println("by pre/post  "+configsByPrePostAndDomain.size)

    var selectedSmoother : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    selectedSmoother.add(smootherFeature)
    var configsBySmootherAndDomain = FeatureModel.filterConfigurations(configsByPrePostAndDomain, selectedSmoother)
    println("by smoother " + configsBySmootherAndDomain.size)

    //    var selectedCGS : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    //    selectedSmoother.add(FeatureModel.getSpecificFeatureDkPaperTest("cgs"))
    //    configsBySmootherAndDomain = FeatureModel.filterConfigurations(configsBySmootherAndDomain, selectedSmoother)
    //    println("by cgs " + configsBySmootherAndDomain.size)

    var configurationOffset : scala.collection.mutable.Map[Configuration, Double] = scala.collection.mutable.Map()
    configsBySmootherAndDomain.foreach(x => configurationOffset.put(x, predictConfiguration(x, nfp, learnedModels)))

    configsBySmootherAndDomain.foreach(x => if (!configsConsidered.contains(x)) configsConsidered.add(x))

    var forwardFeatureSelection = new ForwardFeatureSelection(coresFeature, 20, configsBySmootherAndDomain.toArray[Configuration], nfp)
    forwardFeatureSelection.applyWithOffset(configurationOffset)
    forwardFeatureSelection.perform()

    var x = forwardFeatureSelection.getModelWithConstants(forwardFeatureSelection.solutionSet.toArray[FFS_Expression])
    println(forwardFeatureSelection.printModelWithConstants(x._1, x._2))

    return x
  }

  def generateModelDependingOnPreAndPost(smootherFeature : Feature, nfp : String, allConfigs : scala.collection.mutable.Set[Configuration]) : Tuple2[Jama.Matrix, Array[FFS_Expression]] = {

    var featuresOfInterest : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    featuresOfInterest.add(FeatureModel.getSpecificFeatureDkPaperTest("pre"))
    featuresOfInterest.add(FeatureModel.getSpecificFeatureDkPaperTest("post"))

    var ccd : CentralCompositeDesign = new CentralCompositeDesign(featuresOfInterest)
    var ccdSamples = ccd.getPoints()

    //    	println("all  "+allConfigs.size)

    var selectedBooleanFeatures : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    selectedBooleanFeatures.add(smootherFeature)
    var allConfigsWithGivenSmoother = FeatureModel.filterConfigurations(allConfigs, selectedBooleanFeatures)
    //		println("by smoother  "+allConfigsWithGivenSmoother.size)

    var domain = FeatureModel.getSpecificFeatureDkPaperTest("cores")
    var minDomain : scala.collection.mutable.Map[Feature, Double] = scala.collection.mutable.Map()
    minDomain.put(domain, 64)
    var configsBySmootherAndDomain = FeatureModel.filterConfigurations(allConfigsWithGivenSmoother, minDomain)
    //		println("by domain "+configsBySmootherAndDomain.size)

    var cgsFeature = FeatureModel.getSpecificFeatureDkPaperTest("cgs")
    selectedBooleanFeatures = scala.collection.mutable.Set()
    selectedBooleanFeatures.add(cgsFeature)
    var configs = FeatureModel.filterConfigurations(configsBySmootherAndDomain, selectedBooleanFeatures)
    //		println("by cgs  "+configs.size)

    var ccdSampleBySmoother = FeatureModel.filterConfigurationsAtLeastOne(configs, ccdSamples)
    //		println("by ccd "+ccdSampleBySmoother.size)

    ccdSampleBySmoother.foreach(x => if (!configsConsidered.contains(x)) configsConsidered.add(x))

    var forwardFeatureSelection = new ForwardFeatureSelection(featuresOfInterest, 20, ccdSampleBySmoother.toArray[Configuration], nfp)
    forwardFeatureSelection.apply(false)
    forwardFeatureSelection.perform()

    var x = forwardFeatureSelection.getModelWithConstants(forwardFeatureSelection.solutionSet.toArray[FFS_Expression])
    println(forwardFeatureSelection.printModelWithConstants(x._1, x._2))

    //		println("Overall Error for "+smootherFeature.identifier  + " : " + forwardFeatureSelection.computeErrorForCombination(forwardFeatureSelection.solutionSet.toArray[FFS_Expression], allConfigsWithGivenSmoother.toArray[Configuration]))
    //		println("-------------------------------------------------------------------------------------")

    return x
  }

}

