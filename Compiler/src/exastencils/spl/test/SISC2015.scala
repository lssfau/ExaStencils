package exastencils.spl.test

import exastencils.spl._
import scala.util.Random
import exastencils.spl.samplingStrategies.doe.PlackettBurmanDesign
import exastencils.spl.samplingStrategies.heuristics.FWHeuristic
import exastencils.spl.samplingStrategies.heuristics.PWHeuristic
import java.io.FileWriter
import java.io._
import exastencils.prettyprinting.JobScriptGenerator
import exastencils.knowledge.Knowledge
import exastencils.spl.samplingStrategies.doe.RandomDesign

object SICS2015 {

  var featuresToConsider : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()

  val max_num_blocks_per_dim : Int = 1024
  val max_num_frags_per_block_per_dim : Int = 64
  val max_fragment_length_per_dim : Int = 64

  import exastencils.core.Settings

  def main(args : Array[String]) : Unit = {

    featureToConsider2D()

    val file = Settings.basePathPrefix + "/Compiler/src/exastencils/knowledge/Knowledge.scala"
    VariabilityParser.parseKnowlegdeFile(file)

    print("for filter ")
    println(FeatureModel.allFeatures.size)
    FeatureModel.filter(featuresToConsider)

    print("after filter ")
    println(FeatureModel.allFeatures.size)

    var features : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    FeatureModel.allFeatures.foreach(x => features.add(x._2))

    //    samplingWithPartitionedFeatureSpace()
    //    orgTest()

    var configs = createConfigsBasedOnMeasurements("E:\\Fallstudien\\SISC_Paper\\2dCC_NoRandom").toArray

    var nfps : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()

    //    nfps.add("preSmoothing")
    //    nfps.add("correction")
    //    nfps.add("cycle")
    //    nfps.add("postSmoothing")
    //
    //    nfps.add("residualUpdate")
    //    nfps.add("restriction")
    //    nfps.add("settingSolution")
    //    nfps.add("setup")
    //    nfps.add("timeToSolve")

    nfps.add("allTime")

    nfps.foreach { x =>
      {
        var ffs : ForwardFeatureSelection = new ForwardFeatureSelection(features, 50, configs, x)
        println(x)
        ffs.apply()
        var y = ffs.getModelWithConstants(ffs.solutionSet.toArray[FFS_Expression])
        println(ffs.printModelWithConstants(y._1, y._2))
      }
    }

  }

  def createConfigsBasedOnMeasurements(basicDir : String) : scala.collection.mutable.Set[Configuration] = {

    var configurations : scala.collection.mutable.Set[Configuration] = scala.collection.mutable.Set()

    var basicPath : File = new File(basicDir)

    var knowledgeFilesPath : File = new File(basicDir + "/knowledgeFiles")
    var resultFilePath : File = new File(basicDir + "/measurements")

    var featuresConsidered : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()
    FeatureModel.allFeatures.foreach(x => featuresConsidered.add(x._1))

    var measurements : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()
    resultFilePath.listFiles().foreach(x => measurements.add(x.getName))

    knowledgeFilesPath.listFiles().foreach { x =>
      var featureValues = parseKnowledgeFile(x.getAbsolutePath)
      var featureValuesConsidered = parseKnowledgeFile(x.getAbsolutePath, featuresConsidered)
      if (measurements.contains(featureValues("l3tmp_timerOuputFile").replaceAll("\"", ""))) {
        var measurement : scala.collection.mutable.Map[String, Double] = parseMeasurement(basicDir + "/measurements/" + featureValues("l3tmp_timerOuputFile").replaceAll("\"", ""))
        configurations.add(createConfiguration(featureValuesConsidered, measurement))

      }

    }

    return configurations
  }

  def samplingWithPartitionedFeatureSpace() : Boolean = {

    //  splitting feature in binary and numeric (numeric features with less than 5 values are used in the binary samplings)  
    var featuresBinary : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    var featuresNumeric : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    FeatureModel.allFeatures.foreach(x => {
      if (x._2.isXorFeature || !x._2.isNumerical)
        featuresBinary.add(x._2)
      else featuresNumeric.add(x._2)
    });

    println("numeric:::: " + featuresNumeric.size)
    featuresNumeric.foreach(x => { println(x.identifier) })
    println("--------------------------------")

    println("binary:::: " + featuresBinary.size)
    featuresBinary.foreach(x => { println(x.identifier) })
    println("--------------------------------")

    // partition numeric options to domain_ALL,maxLevel and other features
    var numericOptionsFirstSampling : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    var numericOptionsSecondSampling : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    featuresNumeric.foreach(x =>
      {
        if (x.identifier.startsWith("domain") || x.identifier.equals("maxLevel") || x.identifier.equals("minLevel"))
          numericOptionsFirstSampling.add(x)
        else
          numericOptionsSecondSampling.add(x)
      })
    println("Numeric First  Set size " + numericOptionsFirstSampling.size)
    println("Numeric Second Set size " + numericOptionsSecondSampling.size)

    // end numeric option splitting
    var binaryConfigs : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature, String]] = scala.collection.mutable.Set()
    // binary Sampling
    var fwSampling = new FWHeuristic(featuresBinary)
    binaryConfigs ++= fwSampling.getPoints()
    println("binary FW " + binaryConfigs.size)

    //    var pwSampling = new PWHeuristic(featuresBinary)
    //    binaryConfigs ++= pwSampling.getPoints()
    //    println("binary FW & PW "+binaryConfigs.size)

    var problemDefinition : scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map()
    problemDefinition2D_ConstCoeff(problemDefinition)
    //    problemDefinition2D_VarCoeff(problemDefinition)
    //    problemDefinition3D_ConstCoeff(problemDefinition)
    //    problemDefinition3D_VarCoeff(problemDefinition)

    // numeric Sampling for domain partition
    var pbd = new PlackettBurmanDesign(numericOptionsFirstSampling)
    pbd.initSeeds()
    pbd.setSeed(7, 49)
    var numericSamplings = pbd.getPoints()

    var validPartitionsWithKey : scala.collection.mutable.Map[String, scala.collection.mutable.Map[Feature, Double]] = scala.collection.mutable.Map()
    numericSamplings.foreach(f => {
      if (isValidConfigDomainPartition(f, problemDefinition)) {
        var key = ""
        f.foreach(f => key += (f._1.identifier + ":" + f._2 + " "))
        validPartitionsWithKey.put(key, f)
        print("#")
      } else {
        var x = getValidConfiguration(f, problemDefinition, validPartitionsWithKey.size)
        var key = ""
        x.foreach(f => key += (f._1.identifier + ":" + f._2 + " "))
        if (!validPartitionsWithKey.contains(key))
          validPartitionsWithKey.put(key, x)
        print("#")
      }
    })
    println("")
    println("binary " + binaryConfigs.size)
    println("numeric " + validPartitionsWithKey.size)

    var validPartitions : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature, Double]] = scala.collection.mutable.Set()
    validPartitionsWithKey.foreach(x => validPartitions.add(x._2))

    var configurationsPreFiltered : scala.collection.mutable.Set[Configuration] = (new Configuration).generateConfigurations(binaryConfigs, validPartitions)
    println("configsBeforeFiltering " + configurationsPreFiltered.size)
    //       
    var configurationsFiltered : scala.collection.mutable.Set[Configuration] = scala.collection.mutable.Set()
    configurationsPreFiltered.foreach(x =>
      {
        this.problemDefinition2D_ConstCoeff(x.partialBaseConfig)
        this.derivedParameters(x)
        if (isValidConfigDomainPartition(x))
          configurationsFiltered.add(x)
      })

    println("configsAfterFiltering  " + configurationsFiltered.size)

    pbd = new PlackettBurmanDesign(numericOptionsSecondSampling)
    pbd.initSeeds()
    pbd.setSeed(7, 49)
    numericSamplings = pbd.getPoints()
    println("second sampling combinations " + numericSamplings.size)

    var configurationsWithSecondSampling : scala.collection.mutable.Set[Configuration] = scala.collection.mutable.Set()
    configurationsFiltered.foreach { x =>
      {
        numericSamplings.foreach(y => {
          var configCopy = x.copy();
          configCopy.addNumericOptions(y)
          if (isValidConfigSecondStep(configCopy))
            configurationsWithSecondSampling.add(configCopy)

        })

      }
    }
    println("configsCombined  " + configurationsWithSecondSampling.size)

    ////////////////////////////////////// end feature wise sampling in combination with pdb

    var pwSampling = new PWHeuristic(featuresBinary)
    binaryConfigs = pwSampling.getPoints()
    println("----------binary PW " + binaryConfigs.size)

    // numeric Sampling for domain partition
    pbd = new PlackettBurmanDesign(numericOptionsFirstSampling)
    pbd.initSeeds()
    pbd.setSeed(5, 25)
    numericSamplings = pbd.getPoints()

    validPartitionsWithKey = scala.collection.mutable.Map()
    numericSamplings.foreach(f => {
      if (isValidConfigDomainPartition(f, problemDefinition)) {
        var key = ""
        f.foreach(f => key += (f._1.identifier + ":" + f._2 + " "))
        print("#")
        validPartitionsWithKey.put(key, f)
      } else {
        var x = getValidConfiguration(f, problemDefinition, validPartitionsWithKey.size)
        var key = ""
        x.foreach(f => key += (f._1.identifier + ":" + f._2 + " "))
        print("#")
        if (!validPartitionsWithKey.contains(key))
          validPartitionsWithKey.put(key, x)
      }
    })
    println("")
    println("binary " + binaryConfigs.size)
    println("numeric " + validPartitionsWithKey.size)

    validPartitions = scala.collection.mutable.Set()
    validPartitionsWithKey.foreach(x => validPartitions.add(x._2))

    configurationsPreFiltered = (new Configuration).generateConfigurations(binaryConfigs, validPartitions)
    println("configsBeforeFiltering " + configurationsPreFiltered.size)
    //       
    configurationsFiltered = scala.collection.mutable.Set()
    configurationsPreFiltered.foreach(x =>
      {
        this.problemDefinition2D_ConstCoeff(x.partialBaseConfig)
        this.derivedParameters(x)
        if (isValidConfigDomainPartition(x))
          configurationsFiltered.add(x)
      })

    println("configsAfterFiltering  " + configurationsFiltered.size)

    pbd = new PlackettBurmanDesign(numericOptionsSecondSampling)
    pbd.initSeeds()
    pbd.setSeed(5, 25)
    numericSamplings = pbd.getPoints()
    println("second sampling combinations " + numericSamplings.size)

    var configurationsWithSecondSamplingPW : scala.collection.mutable.Set[Configuration] = scala.collection.mutable.Set()
    configurationsFiltered.foreach { x =>
      {
        numericSamplings.foreach(y => {
          var configCopy = x.copy();
          configCopy.addNumericOptions(y)
          if (isValidConfigSecondStep(configCopy))
            configurationsWithSecondSamplingPW.add(configCopy)

        })

      }
    }
    println("configsCombined PW  " + configurationsWithSecondSamplingPW.size)

    configurationsWithSecondSampling ++= configurationsWithSecondSamplingPW

    println("configsCombined FW & PW " + configurationsWithSecondSampling.size)

    /////////////////////////////////////////////////////// RANDOM ////////////

    fwSampling = new FWHeuristic(featuresBinary)
    binaryConfigs = fwSampling.getPoints()
    println("----------binary FW " + binaryConfigs.size)

    // numeric Sampling for domain partition
    var rDesign = new RandomDesign(numericOptionsFirstSampling)
    numericSamplings = rDesign.getPoints()

    validPartitionsWithKey = scala.collection.mutable.Map()
    numericSamplings.foreach(f => {
      if (isValidConfigDomainPartition(f, problemDefinition)) {
        var key = ""
        f.foreach(f => key += (f._1.identifier + ":" + f._2 + " "))
        print("#")
        validPartitionsWithKey.put(key, f)
      } else {
        var x = getValidConfiguration(f, problemDefinition, validPartitionsWithKey.size)
        var key = ""
        x.foreach(f => key += (f._1.identifier + ":" + f._2 + " "))
        print("#")
        if (!validPartitionsWithKey.contains(key))
          validPartitionsWithKey.put(key, x)
      }
    })
    println("")
    println("binary " + binaryConfigs.size)
    println("numeric " + validPartitionsWithKey.size)

    validPartitions = scala.collection.mutable.Set()
    validPartitionsWithKey.foreach(x => validPartitions.add(x._2))

    configurationsPreFiltered = (new Configuration).generateConfigurations(binaryConfigs, validPartitions)
    println("configsBeforeFiltering " + configurationsPreFiltered.size)
    //       
    configurationsFiltered = scala.collection.mutable.Set()
    configurationsPreFiltered.foreach(x =>
      {
        this.problemDefinition2D_ConstCoeff(x.partialBaseConfig)
        this.derivedParameters(x)
        if (isValidConfigDomainPartition(x))
          configurationsFiltered.add(x)
      })

    println("configsAfterFiltering  " + configurationsFiltered.size)

    rDesign = new RandomDesign(numericOptionsSecondSampling)
    numericSamplings = rDesign.getPoints()
    println("second sampling combinations " + numericSamplings.size)

    configurationsWithSecondSamplingPW = scala.collection.mutable.Set()
    configurationsFiltered.foreach { x =>
      {
        numericSamplings.foreach(y => {
          var configCopy = x.copy();
          configCopy.addNumericOptions(y)
          if (isValidConfigSecondStep(configCopy))
            configurationsWithSecondSamplingPW.add(configCopy)

        })

      }
    }
    println("configsCombined Random  " + configurationsWithSecondSamplingPW.size)

    configurationsWithSecondSampling ++= configurationsWithSecondSamplingPW

    println("configsCombined FW & PW & Random " + configurationsWithSecondSampling.size)

    // print random configs
    //    printConfigurations(configurationsWithSecondSamplingPW)

    // print all 
    printConfigurations(configurationsWithSecondSampling)

    return true
  }

  def printConfigurations(configurations : scala.collection.mutable.Set[Configuration]) = {
    var configsByMpiOmp : scala.collection.mutable.Map[String, scala.collection.mutable.Set[Configuration]] = scala.collection.mutable.Map()
    configurations.foreach(x => {
      var mpiOmpKey = x.partialBaseConfig("mpi_numThreads").asInstanceOf[Int] + "_" + x.partialBaseConfig("omp_numThreads").asInstanceOf[Int]
      if (configsByMpiOmp.contains(mpiOmpKey)) {
        configsByMpiOmp(mpiOmpKey).add(x)
      } else {
        var newList : scala.collection.mutable.Set[Configuration] = scala.collection.mutable.Set()
        newList.add(x)
        configsByMpiOmp.put(mpiOmpKey, newList)
      }
    })
    println("different MPI OMP threads " + configsByMpiOmp.size)
    var newFilelocation = "E://ScalaExaStencil/configsSiSC/script_all.sh"
    var newFilelocationMake = "E://ScalaExaStencil/configsSiSC/makeScript.sh"
    val writer = new PrintWriter(new File(newFilelocation))
    var makeWriter = new PrintWriter(new File(newFilelocationMake))
    configsByMpiOmp.foreach(x => {
      var mpi_numThreads = x._2.head.partialBaseConfig("mpi_numThreads").asInstanceOf[Int]
      var omp_numThreads = x._2.head.partialBaseConfig("omp_numThreads").asInstanceOf[Int]

      println("NumNodes-----------" + x._2.size)
      generateJobScript(x._1, x._2)
      var index = 1

      var scriptBuilder = new StringBuilder()

      x._2.foreach(y => {
        var configKey = mpi_numThreads + "_" + omp_numThreads + "_" + index
        configToKnowledgeFile(y, configKey)
        index += 1
        generateShFileChimaira(configKey)
        writer.append("sbatch -A spl -p chimaira -n 1 -c 1 /home/grebhahn/ScalaCodegenSISC/config/script_" + configKey + ".sh\n")

        makeWriter.append("cd config_" + configKey + "/\n")
        makeWriter.append("make -j\n")
        makeWriter.append("cd ..\n")

      })

    })

    makeWriter.flush()
    makeWriter.close()

    writer.flush()
    writer.close()
  }

  def generateShFileChimaira(configKey : String) {
    var newFilelocation = "E://ScalaExaStencil/configsSiSC/script_" + configKey + ".sh"
    val writer = new PrintWriter(new File(newFilelocation))
    writer.append("#!/bin/sh\n")
    writer.append("cd /home/grebhahn/ScalaCodegenSISC/knowledgeFiles\n")
    writer.append("mkdir ../config/config_" + configKey + "/\n")
    writer.append("srun exaGen Main knowledgeFile_" + configKey + ".knowledge ../config/config_" + configKey + "/\n")
    writer.flush()
    writer.close()
  }

  def generateJobScript(mpiOmpKey : String, configs : scala.collection.mutable.Set[Configuration]) = {
    Settings.user = "alex"
    Knowledge.targetCompiler = "IBMBG"
    var mpi_numThreads = configs.head.partialBaseConfig("mpi_numThreads").asInstanceOf[Int]
    var omp_numThreads = configs.head.partialBaseConfig("omp_numThreads").asInstanceOf[Int]
    var number = 0

    var numNodes = (mpi_numThreads * omp_numThreads) / 64

    if (numNodes <= 64) {
      // splitting in multiple jobs ( because one the max timeout of one job is 30 minutes)       
      var sourcePathes : Array[String] = new Array(20)
      var config = 0
      var index = 0
      configs.foreach { x =>
        {
          sourcePathes(index) = ("config_" + mpiOmpKey + "_" + (config + 1))
          config += 1
          index += 1
          if (index == 20) {
            JobScriptGenerator.write(mpi_numThreads, omp_numThreads, sourcePathes, number)
            index = 0;
            number += 1
            sourcePathes = new Array(20)
          }
        }
      }
      JobScriptGenerator.write(mpi_numThreads, omp_numThreads, sourcePathes, number)

    } else {
      var sourcePathes : Array[String] = new Array(configs.size)
      var index = 0
      configs.foreach { x =>
        {
          sourcePathes(index) = ("config_" + mpiOmpKey + "_" + (index + 1))
          index += 1
        }
      }
      JobScriptGenerator.write(mpi_numThreads, omp_numThreads, sourcePathes, number)
    }
  }

  def configToKnowledgeFile(config : Configuration, index : String) = {

    var newFilelocation = "E://ScalaExaStencil/configsSiSC/knowledgeFile_" + index + ".knowledge";

    val writer = new PrintWriter(new File(newFilelocation))

    var configContent = config.getKnowledgeFileContent()

    configContent.foreach { x => writer.write(x) }
    var partialBase = partialBaseConfigInformation()
    writer.write(partialBase)

    writer.write("l3tmp_timerOuputFile = \"timings_" + index + ".csv\"\n")

    writer.flush()
    writer.close()

  }

  def featureToConsider2D() = {

    featuresToConsiderDimIndependent()

    featuresToConsider.add("poly_tileSize_x")

    featuresToConsider.add("domain_fragmentLength_x")
    featuresToConsider.add("domain_fragmentLength_y")
    featuresToConsider.add("domain_rect_numFragsPerBlock_x")
    featuresToConsider.add("domain_rect_numFragsPerBlock_y")

  }

  def featureToConsider3D() = {

    featuresToConsiderDimIndependent()

    // TODO Upper bound and stepsize
    featuresToConsider.add("poly_tileSize_x")
    featuresToConsider.add("poly_tileSize_y")

    featuresToConsider.add("domain_fragmentLength_x")
    featuresToConsider.add("domain_fragmentLength_y")
    featuresToConsider.add("domain_fragmentLength_z")
    featuresToConsider.add("domain_rect_numFragsPerBlock_x")
    featuresToConsider.add("domain_rect_numFragsPerBlock_y")
    featuresToConsider.add("domain_rect_numFragsPerBlock_z")
  }

  def featuresToConsiderDimIndependent() = {

    featuresToConsider.add("minLevel")
    featuresToConsider.add("maxLevel")

    featuresToConsider.add("opt_useAddressPrecalc")
    featuresToConsider.add("opt_vectorize")
    featuresToConsider.add("l3tmp_smoother")
    featuresToConsider.add("l3tmp_numRecCycleCalls")
    featuresToConsider.add("l3tmp_numPre")
    featuresToConsider.add("l3tmp_numPost")

    featuresToConsider.add("l3tmp_useSlotsForJac")

    featuresToConsider.add("poly_optLevel_fine")
    //featuresToConsider.add("poly_optLevel_coarse")
    //featuresToConsider.add("poly_numFinestLevels")

    //featuresToConsider.add("poly_tileOuterLoop")

    featuresToConsider.add("opt_unroll")
    featuresToConsider.add("opt_unroll_interleave")
    //featuresToConsider.add("comm_strategyFragment")
    featuresToConsider.add("comm_useFragmentLoopsForEachOp")

    //TODO: upper bound & sampling (Sebastian)
    //featuresToConsider.add("omp_minWorkItemsPerThread")

    featuresToConsider.add("mpi_useCustomDatatypes")
  }

  def partialBaseConfigInformation() : String = {
    var sb : StringBuilder = new StringBuilder()

    sb ++= "targetCompiler = \"IBMBG\" \n"
    sb ++= "targetCompilerVersion = 12\n"
    sb ++= "targetCompilerVersionMinor = 1\n"

    sb ++= "useDblPrecision = true\n"

    sb ++= "simd_instructionSet = \"QPX\"\n"
    sb ++= "simd_avoidUnaligned = true\n"
    sb ++= "timer_type = \"MPI_TIME\"\n"

    sb ++= "domain_readFromFile = false\n"
    sb ++= "domain_onlyRectangular = true\n"
    sb ++= "domain_rect_generate = true\n"

    sb ++= "ir_genSepLayoutsPerField = true\n"

    sb ++= "comm_sepDataByFragment = true\n"
    sb ++= "comm_sepDataByDomain = false\n"
    sb ++= "comm_sepDataByField = false\n"
    sb ++= "comm_sepDataByLevel = false\n"
    sb ++= "comm_sepDataByNeighbor = true\n"
    sb ++= "comm_useFragmentArrays = true\n"
    sb ++= "comm_useDomainArrays = true\n"
    sb ++= "comm_useFieldArrays = false\n"
    sb ++= "comm_useLevelArrays = false\n"
    sb ++= "comm_useNeighborArrays = true\n"

    sb ++= "data_initAllFieldsWithZero = true\n"
    sb ++= "data_useFieldNamesAsIdx = false\n"

    sb ++= "mpi_defaultCommunicator = \"MPI_COMM_WORLD\"\n"
    sb ++= "mpi_enabled = true\n"
    sb ++= "mpi_useLoopsWherePossible = true\n"

    sb ++= "omp_useCollapse = false\n"

    sb ++= "poly_scheduleAlgorithm = \"isl\"\n"
    sb ++= "poly_optimizeDeps = \"raw\"\n"
    sb ++= "poly_filterDeps = true\n"
    sb ++= "poly_simplifyDeps = true\n"
    sb ++= "poly_fusionStrategy = \"max\"\n"
    sb ++= "poly_maximizeBandDepth = false\n"
    sb ++= "poly_maxConstantTerm = -1\n"
    sb ++= "poly_maxCoefficient = -1\n"

    sb ++= "l3tmp_generateL4 = true\n"

    sb ++= "l3tmp_cgs = \"CG\"\n"

    sb ++= "l3tmp_useConditionsForRBGS = true\n"
    sb ++= "l3tmp_useSlotVariables = true\n"
    sb ++= "l3tmp_genHDepStencils = true\n"

    sb ++= "l3tmp_genTimersPerFunction = true\n"
    sb ++= "l3tmp_genTimersPerLevel = false\n"
    sb ++= "l3tmp_genTimersForComm = false\n"
    sb ++= "l3tmp_genCommTimersPerLevel = false\n"

    sb ++= "l3tmp_printAllTimers = false\n"
    sb ++= "l3tmp_printTimersToFile = true\n"

    sb ++= "l3tmp_genNonZeroRhs = true\n"

    sb ++= "l3tmp_genExtFields = false\n"
    sb ++= "l3tmp_genGlobalOmega = false\n"
    sb ++= "l3tmp_genSetableStencil = false\n"
    sb ++= "l3tmp_genVectorFields = false\n"
    sb ++= "l3tmp_numVecDims = 1\n"
    sb ++= "l3tmp_genEmbeddedDomain = false\n"
    sb ++= "l3tmp_useMaxNorm = false\n"
    sb ++= "l3tmp_genCellBasedDiscr = false\n"

    sb ++= "l3tmp_printFieldAtEnd = false\n"
    sb ++= "l3tmp_initSolWithRand = false\n"
    sb ++= "l3tmp_genForAutoTests = true\n"
    sb ++= "l3tmp_printError = false\n"
    sb ++= "l3tmp_useMaxNormForError = true\n"

    sb ++= "l3tmp_sisc = true\n"
    sb ++= "l3tmp_kelvin = false\n"

    sb ++= "l3tmp_genStencilStencilConv = false\n"
    sb ++= "l3tmp_genAsyncCommunication = false\n"
    sb ++= "l3tmp_genFragLoops = false\n"

    sb ++= "experimental_useLevelIndepFcts = false\n"
    sb ++= "experimental_Neumann = false\n"
    sb ++= "experimental_timerEnableCallStacks = false\n"

    sb ++= "data_alignTmpBufferPointers = false\n"
    return sb.toString()
  }

  def problemDefinition2D_ConstCoeff(config : scala.collection.mutable.Map[String, Any]) = {
    config.put("dimensionality", 2)
    config.put("l3tmp_genStencilFields", false)
    config.put("l3tmp_exactSolution", "\"Kappa\"\n")
    config.put("num_points_per_dim", 32768) // 32*1024
  }

  def problemDefinition2D_VarCoeff(config : scala.collection.mutable.Map[String, Any]) = {
    config.put("dimensionality", 2)
    config.put("l3tmp_genStencilFields", true)
    config.put("l3tmp_exactSolution", "\"Kappa_VC\"\n")
    config.put("num_points_per_dim", 32768) // 32*1024
  }

  def problemDefinition3D_ConstCoeff(config : scala.collection.mutable.Map[String, Any]) = {
    config.put("dimensionality", 3)
    config.put("l3tmp_genStencilFields", false)
    config.put("l3tmp_exactSolution", "\"Kappa\"\n")
    config.put("num_points_per_dim", 1024)
  }

  def problemDefinition3D_VarCoeff(config : scala.collection.mutable.Map[String, Any]) = {
    config.put("dimensionality", 3)
    config.put("l3tmp_genStencilFields", true)
    config.put("l3tmp_exactSolution", "\"Kappa_VC\"\n")
    config.put("num_points_per_dim", 1024)
  }

  def isValidConfigDomainPartition(config : Configuration) : Boolean = {
    var num_unit_frags_x : Long = 1
    var num_unit_frags_y : Long = 1
    var num_unit_frags_z : Long = 1

    var frag_volume = 1.0

    //config.partialBaseConfig.put("domain_rect_numBlocks_x", domain_rect_numBlocks_x)

    if (config.partialBaseConfig.apply("dimensionality").asInstanceOf[Int] == 3) {
      num_unit_frags_x = config.partialBaseConfig.apply("domain_rect_numBlocks_x").asInstanceOf[Double].toLong * (config.getNumericFeatureValue("domain_rect_numFragsPerBlock_x") * config.getNumericFeatureValue("domain_fragmentLength_x")).toLong
      num_unit_frags_y = config.partialBaseConfig.apply("domain_rect_numBlocks_y").asInstanceOf[Double].toLong * (config.getNumericFeatureValue("domain_rect_numFragsPerBlock_y") * config.getNumericFeatureValue("domain_fragmentLength_y")).toLong
      num_unit_frags_z = config.partialBaseConfig.apply("domain_rect_numBlocks_z").asInstanceOf[Double].toLong * (config.getNumericFeatureValue("domain_rect_numFragsPerBlock_z") * config.getNumericFeatureValue("domain_fragmentLength_z")).toLong
      frag_volume =
        config.getNumericFeatureValue("domain_fragmentLength_x") * config.getNumericFeatureValue("domain_fragmentLength_y") * config.getNumericFeatureValue("domain_fragmentLength_z")
    } else {
      num_unit_frags_x = config.partialBaseConfig.apply("domain_rect_numBlocks_x").asInstanceOf[Int].toLong * (config.getNumericFeatureValue("domain_rect_numFragsPerBlock_x") * config.getNumericFeatureValue("domain_fragmentLength_x")).toLong
      num_unit_frags_y = config.partialBaseConfig.apply("domain_rect_numBlocks_y").asInstanceOf[Int].toLong * (config.getNumericFeatureValue("domain_rect_numFragsPerBlock_y") * config.getNumericFeatureValue("domain_fragmentLength_y")).toLong
      frag_volume =
        config.getNumericFeatureValue("domain_fragmentLength_x") * config.getNumericFeatureValue("domain_fragmentLength_y")
    }

    if (!(getNumNodes(config) > 8))
      return false

    var mem_per_node = (8.0 * 4.0 * 4.0 / 3.0 * Math.pow(config.partialBaseConfig.apply("num_points_per_dim").asInstanceOf[Int], config.partialBaseConfig.apply("dimensionality").asInstanceOf[Int])) / getNumNodes(config)
    var memory : Double = 12L * 1024L * 1024L * 1024L
    if (!(mem_per_node <= memory))
      return false

    if (config.partialBaseConfig("dimensionality").asInstanceOf[Int] == 3) {
      if (!(num_unit_frags_x == num_unit_frags_y) || !(num_unit_frags_y == num_unit_frags_z))
        return false

    } else {
      if (!(num_unit_frags_x == num_unit_frags_y))
        return false
    }

    if (config.partialBaseConfig.apply("domain_numBlocks").asInstanceOf[Int] < 8)
      return false

    if (!(frag_volume <= 64.0 && config.partialBaseConfig.apply("domain_numFragmentsPerBlock").asInstanceOf[Int] <= 64))
      return false

    if (!(frag_volume == 1.0 || config.partialBaseConfig.apply("domain_numFragmentsPerBlock").asInstanceOf[Int] == 1.0))
      return false

    return true;
  }

  def isValidConfigDomainPartition(config : scala.collection.mutable.Map[Feature, Double], problemDefinition : scala.collection.mutable.Map[String, Any]) : Boolean = {

    // calculation of dependent features 
    var domain_rect_numBlocks_x = ((problemDefinition("num_points_per_dim").asInstanceOf[Int] / (
      config(FeatureModel.get("domain_fragmentLength_x")) * Math.pow(2, config(FeatureModel.get("maxLevel")))))
      / config(FeatureModel.get("domain_rect_numFragsPerBlock_x")))

    var domain_rect_numBlocks_y = ((problemDefinition("num_points_per_dim").asInstanceOf[Int] / (
      config(FeatureModel.get("domain_fragmentLength_y")) * Math.pow(2, config(FeatureModel.get("maxLevel")))))
      / config(FeatureModel.get("domain_rect_numFragsPerBlock_y")))

    var domain_rect_numBlocks_z : Double = 1.0

    if (problemDefinition("dimensionality").asInstanceOf[Int] == 3) {
      domain_rect_numBlocks_z = ((problemDefinition("num_points_per_dim").asInstanceOf[Int] / (
        config(FeatureModel.get("domain_fragmentLength_z")) * Math.pow(2, config(FeatureModel.get("maxLevel")))))
        / config(FeatureModel.get("domain_rect_numFragsPerBlock_z")))
    }

    var num_frags_per_block_total : Double = 1.0
    var frag_volume = 1.0
    var num_blocks_total = 1.0
    if (problemDefinition("dimensionality").asInstanceOf[Int] == 3) {
      num_frags_per_block_total =
        config(FeatureModel.get("domain_rect_numFragsPerBlock_x")) * config(FeatureModel.get("domain_rect_numFragsPerBlock_y")) * config(FeatureModel.get("domain_rect_numFragsPerBlock_z"))
      frag_volume =
        config(FeatureModel.get("domain_fragmentLength_x")) * config(FeatureModel.get("domain_fragmentLength_y")) * config(FeatureModel.get("domain_fragmentLength_z"))
      num_blocks_total =
        domain_rect_numBlocks_x * domain_rect_numBlocks_y * domain_rect_numBlocks_z
    } else {
      num_frags_per_block_total =
        config(FeatureModel.get("domain_rect_numFragsPerBlock_x")) * config(FeatureModel.get("domain_rect_numFragsPerBlock_y"))
      frag_volume =
        config(FeatureModel.get("domain_fragmentLength_x")) * config(FeatureModel.get("domain_fragmentLength_y"))
      num_blocks_total =
        domain_rect_numBlocks_x * domain_rect_numBlocks_y
    }

    var domain_numBlocks = num_blocks_total
    var domain_numFragmentsPerBlock = num_frags_per_block_total
    var mpi_numThreads = num_blocks_total

    var omp_enabled = false
    var omp_numThreads = 1.0
    var omp_parallelizeLoopOverFragments = false
    var omp_parallelizeLoopOverDimensions = false

    if (num_frags_per_block_total == 1.0 && frag_volume == 1.0) {
      omp_enabled = false
      omp_numThreads = 1.0
      omp_parallelizeLoopOverFragments = false
      omp_parallelizeLoopOverDimensions = false
    } else if (num_frags_per_block_total > frag_volume) {
      omp_enabled = true
      omp_numThreads = num_frags_per_block_total
      omp_parallelizeLoopOverFragments = true
      omp_parallelizeLoopOverDimensions = false
    } else {
      omp_enabled = true
      omp_numThreads = frag_volume
      omp_parallelizeLoopOverFragments = false
      omp_parallelizeLoopOverDimensions = true
    }

    var num_unit_frags_x : Long = 1
    var num_unit_frags_y : Long = 1
    var num_unit_frags_z : Long = 1

    if (problemDefinition("dimensionality").asInstanceOf[Int] == 3) {
      num_unit_frags_x = domain_rect_numBlocks_x.toLong * (config(FeatureModel.get("domain_rect_numFragsPerBlock_x")) * config(FeatureModel.get("domain_fragmentLength_x"))).toLong
      num_unit_frags_y = domain_rect_numBlocks_y.toLong * (config(FeatureModel.get("domain_rect_numFragsPerBlock_y")) * config(FeatureModel.get("domain_fragmentLength_y"))).toLong
      num_unit_frags_z = domain_rect_numBlocks_z.toLong * (config(FeatureModel.get("domain_rect_numFragsPerBlock_z")) * config(FeatureModel.get("domain_fragmentLength_z"))).toLong
    } else {
      num_unit_frags_x = domain_rect_numBlocks_x.toLong * (config(FeatureModel.get("domain_rect_numFragsPerBlock_x")) * config(FeatureModel.get("domain_fragmentLength_x"))).toLong
      num_unit_frags_y = domain_rect_numBlocks_y.toLong * (config(FeatureModel.get("domain_rect_numFragsPerBlock_y")) * config(FeatureModel.get("domain_fragmentLength_y"))).toLong
    }

    var numNodes : Double = getNumNodes(mpi_numThreads.toInt, omp_numThreads.toInt)

    if (!(numNodes > 8.0))
      // println("Not enough nodes in use")
      return false

    // new Constraint
    if (numNodes > 4096.0)
      return false

    //    println(numNodes)  

    var mem_per_node = (8.0 * 4.0 * 4.0 / (3.0 * Math.pow(problemDefinition("num_points_per_dim").asInstanceOf[Int], problemDefinition("dimensionality").asInstanceOf[Int]))) / numNodes
    var memory : Double = 12L * 1024L * 1024L * 1024L
    if (!(mem_per_node <= memory))
      //println("Memory requirements for each node are too high")
      return false

    if (problemDefinition("dimensionality").asInstanceOf[Int] == 3) {
      if (!(num_unit_frags_x == num_unit_frags_y) || !(num_unit_frags_y == num_unit_frags_z))
        return false

    } else {
      if (!(num_unit_frags_x == num_unit_frags_y))
        //println("Not square")
        return false
    }

    if (!(domain_numBlocks >= 8))
      //println("Not enough blocks to distribute :%s" % num_blocks_total)
      return false

    if (!(frag_volume <= 64.0 && domain_numFragmentsPerBlock <= 64))
      //println("Too many omp threads :%s" % self.get_value("omp_numThreads", 1))
      return false

    if (!(frag_volume == 1.0 || domain_numFragmentsPerBlock == 1))
      //print("Two different omp parallelization strategies chosen concurrently")
      return false

    if (config(FeatureModel.get("minLevel")).toInt >= config(FeatureModel.get("maxLevel")).toInt)
      return false

    var domain_x = domain_rect_numBlocks_x * config(FeatureModel.get("domain_rect_numFragsPerBlock_x")) * config(FeatureModel.get("domain_fragmentLength_x")) * Math.pow(2, config(FeatureModel.get("minLevel")))
    if (domain_x > 64.0)
      return false;

    var domain_y = domain_rect_numBlocks_y * config(FeatureModel.get("domain_rect_numFragsPerBlock_y")) * config(FeatureModel.get("domain_fragmentLength_y")) * Math.pow(2, config(FeatureModel.get("minLevel")))
    if (domain_y > 64.0)
      return false;

    if (problemDefinition("dimensionality").asInstanceOf[Int] == 3) {
      var domain_z = domain_rect_numBlocks_z * config(FeatureModel.get("domain_rect_numFragsPerBlock_z")) * config(FeatureModel.get("domain_fragmentLength_z")) * Math.pow(2, config(FeatureModel.get("minLevel")))
      if (domain_z > 64.0)
        return false;
    }

    return true;
  }

  def isValidConfigSecondStep(config : Configuration) : Boolean = {

    if (config.getNumericFeatureValue("l3tmp_numPre").toInt + config.getNumericFeatureValue("l3tmp_numPost").toInt < 1)
      return false

    if (config.getNumericFeatureValue("l3tmp_numPre").toInt + config.getNumericFeatureValue("l3tmp_numPost").toInt > 12)
      return false

    // select l3tmp_useSlotsForJac only if Jac is the Smoother
    if (config.boolFeatures(FeatureModel.allFeatures("l3tmp_useSlotsForJac")) &&
      config.xorFeatureValues(FeatureModel.allFeatures("l3tmp_smoother")).asInstanceOf[String].equals("Jac"))
      return false

    // un-splotted Jac
    if (config.xorFeatureValues(FeatureModel.allFeatures("l3tmp_smoother")).asInstanceOf[String].equals("Jac") &&
      !config.boolFeatures(FeatureModel.allFeatures("l3tmp_useSlotsForJac"))) {

      if (!(config.getNumericFeatureValue("l3tmp_numPre").toInt % 2 == 0) || !(config.getNumericFeatureValue("l3tmp_numPre").toInt % 2 == 0))
        return false
    }

    if (config.boolFeatures(FeatureModel.allFeatures("opt_unroll_interleave")) && config.getNumericFeatureValue("opt_unroll").toInt == 1) //config.xorFeatureValues(FeatureModel.allFeatures("opt_unroll")).toInt == 1)
      return false

    return true;
  }

  def derivedParameters(config : Configuration) = {
    var domain_rect_numBlocks_x : Int = (((config.partialBaseConfig("num_points_per_dim").asInstanceOf[Int] / (
      config.getNumericFeatureValue("domain_fragmentLength_x") * Math.pow(2, config.getNumericFeatureValue("maxLevel")))))
      / config.getNumericFeatureValue("domain_rect_numFragsPerBlock_x")).toInt

    config.partialBaseConfig.put("domain_rect_numBlocks_x", domain_rect_numBlocks_x)

    var domain_rect_numBlocks_y : Int = (((config.partialBaseConfig("num_points_per_dim").asInstanceOf[Int] / (
      config.getNumericFeatureValue("domain_fragmentLength_y") * Math.pow(2, config.getNumericFeatureValue("maxLevel")))))
      / config.getNumericFeatureValue("domain_rect_numFragsPerBlock_y")).toInt

    config.partialBaseConfig.put("domain_rect_numBlocks_y", domain_rect_numBlocks_y)

    if (config.partialBaseConfig("dimensionality").asInstanceOf[Int] == 3) {

      var domain_rect_numBlocks_z : Int = (((config.partialBaseConfig("num_points_per_dim").asInstanceOf[Int] / (
        config.getNumericFeatureValue("domain_fragmentLength_z") * Math.pow(2, config.getNumericFeatureValue("maxLevel")))))
        / config.getNumericFeatureValue("domain_rect_numFragsPerBlock_z")).toInt

      config.partialBaseConfig.put("domain_rect_numBlocks_z", domain_rect_numBlocks_z)
    }

    var num_frags_per_block_total = 1
    var frag_volume = 1
    var num_blocks_total = 1
    if (config.partialBaseConfig("dimensionality").asInstanceOf[Int] == 3) {
      num_frags_per_block_total =
        (config.getNumericFeatureValue("domain_rect_numFragsPerBlock_x") * config.getNumericFeatureValue("domain_rect_numFragsPerBlock_y") * config.getNumericFeatureValue("domain_rect_numFragsPerBlock_z")).toInt
      frag_volume =
        (config.getNumericFeatureValue("domain_fragmentLength_x") * config.getNumericFeatureValue("domain_fragmentLength_y") * config.getNumericFeatureValue("domain_fragmentLength_z")).toInt
      num_blocks_total =
        (config.partialBaseConfig("domain_rect_numBlocks_x").asInstanceOf[Int] * config.partialBaseConfig("domain_rect_numBlocks_y").asInstanceOf[Int] * config.partialBaseConfig("domain_rect_numBlocks_z").asInstanceOf[Int])
    } else {
      num_frags_per_block_total =
        (config.getNumericFeatureValue("domain_rect_numFragsPerBlock_x") * config.getNumericFeatureValue("domain_rect_numFragsPerBlock_y")).toInt
      frag_volume =
        (config.getNumericFeatureValue("domain_fragmentLength_x") * config.getNumericFeatureValue("domain_fragmentLength_y")).toInt
      num_blocks_total =
        (config.partialBaseConfig("domain_rect_numBlocks_x").asInstanceOf[Int] * config.partialBaseConfig("domain_rect_numBlocks_y").asInstanceOf[Int])
    }

    config.partialBaseConfig.put("domain_numBlocks", num_blocks_total)
    config.partialBaseConfig.put("domain_numFragmentsPerBlock", num_frags_per_block_total.toInt)
    config.partialBaseConfig.put("mpi_numThreads", num_blocks_total)

    if (num_frags_per_block_total == 1 && frag_volume == 1) {
      config.partialBaseConfig.put("omp_enabled", false)
      config.partialBaseConfig.put("omp_numThreads", 1)
      config.partialBaseConfig.put("omp_parallelizeLoopOverFragments", false)
      config.partialBaseConfig.put("omp_parallelizeLoopOverDimensions", false)
    } else if (num_frags_per_block_total > frag_volume) {
      config.partialBaseConfig.put("omp_enabled", true)
      config.partialBaseConfig.put("omp_numThreads", num_frags_per_block_total)
      config.partialBaseConfig.put("omp_parallelizeLoopOverFragments", true)
      config.partialBaseConfig.put("omp_parallelizeLoopOverDimensions", false)
    } else {
      config.partialBaseConfig.put("omp_enabled", true)
      config.partialBaseConfig.put("omp_numThreads", frag_volume)
      config.partialBaseConfig.put("omp_parallelizeLoopOverFragments", false)
      config.partialBaseConfig.put("omp_parallelizeLoopOverDimensions", true)
    }

    if (config.partialBaseConfig("dimensionality").asInstanceOf[Int] == 2) {
      if (config.xorFeatureValues.contains(FeatureModel.allFeatures("l3tmp_smoother"))) {
        config.partialBaseConfig.put("l3tmp_omega ", 1.16)
      } else {
        config.partialBaseConfig.put("l3tmp_omega ", 0.79)
      }
    } else {
      if (config.xorFeatureValues.contains(FeatureModel.allFeatures("l3tmp_smoother"))) {
        config.partialBaseConfig.put("l3tmp_omega ", 1.19)
      } else {
        config.partialBaseConfig.put("l3tmp_omega ", 0.85)
      }

    }

  }

  def getNumNodes(mpi_numThreads : Int, omp_numThreads : Int) : Double = {
    return (mpi_numThreads.toDouble * omp_numThreads.toDouble) / 64
  }

  def getNumNodes(config : scala.collection.mutable.Map[Feature, Double]) : Double = {
    return (config(FeatureModel.allFeatures("mpi_numThreads")) * config(FeatureModel.allFeatures("omp_numThreads"))) / 64
  }

  def getNumNodes(config : Configuration) : Double = {
    return (config.partialBaseConfig("mpi_numThreads").asInstanceOf[Int] * config.partialBaseConfig("omp_numThreads").asInstanceOf[Int]).toDouble / 64
  }

  def identifyValidConfigurations(nonValidConfigs : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature, Double]]) : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature, Double]] = {

    return null
  }

  def getValidConfiguration(nonValidConfig : scala.collection.mutable.Map[Feature, Double], problemDefinition : scala.collection.mutable.Map[String, Any], seed : Int) : scala.collection.mutable.Map[Feature, Double] = {
    // nearest neighbor search
    var config : scala.collection.mutable.Map[Feature, Double] = scala.collection.mutable.Map()
    var featureList : scala.collection.mutable.Map[Int, Feature] = scala.collection.mutable.Map()
    nonValidConfig.foreach(x => {
      config.put(x._1, x._2)
      featureList.put(featureList.size, x._1)
    })

    var r : Random = new Random(seed)

    do {
      // random step
      var idx = r.nextInt(config.size)

      var featureToChange : Feature = featureList(idx)

      var signum = r.nextBoolean()
      if (signum) {

        if (featureToChange.identifier.equals("maxLevel") || featureToChange.identifier.equals("minLevel")) {
          if (config(featureToChange) + featureToChange.stepsize <= featureToChange.maxValue)
            config.update(featureToChange, config(featureToChange) + featureToChange.stepsize)
        } else if (config(featureToChange) * 2 <= featureToChange.maxValue)
          config.update(featureToChange, (config(featureToChange) * 2))
      } else {
        if (featureToChange.identifier.equals("maxLevel") || featureToChange.identifier.equals("minLevel")) {
          if (config(featureToChange) - featureToChange.stepsize >= featureToChange.minValue)
            config.update(featureToChange, config(featureToChange) - featureToChange.stepsize)
        } else if (config(featureToChange) / 2 >= featureToChange.minValue)
          config.update(featureToChange, (config(featureToChange) / 2))
      }
    } while (!isValidConfigDomainPartition(config, problemDefinition))

    return config
  }

  def parseKnowledgeFile(file : String) : scala.collection.mutable.Map[String, String] = {
    var features : scala.collection.mutable.Map[String, String] = scala.collection.mutable.Map()
    var lines = io.Source.fromFile(file).getLines.toList

    lines.foreach { x =>
      if (x.size > 0) {
        var splitted = x.split("=")
        features.put(splitted(0).trim(), splitted(1).trim())
      }
    }
    return features
  }

  def parseKnowledgeFile(file : String, featureNames : scala.collection.mutable.Set[String]) : scala.collection.mutable.Map[String, String] = {
    var features : scala.collection.mutable.Map[String, String] = scala.collection.mutable.Map()
    var lines = io.Source.fromFile(file).getLines.toList

    lines.foreach { x =>
      if (x.size > 0) {
        var splitted = x.split("=")
        if (featureNames.contains(splitted(0).trim()))
          features.put(splitted(0).trim(), splitted(1).trim())
      }
    }
    return features
  }

  def parseMeasurement(file : String) : scala.collection.mutable.Map[String, Double] = {
    var unaggregated : scala.collection.mutable.Map[String, scala.collection.mutable.Set[Double]] = scala.collection.mutable.Map()
    var aggregated : scala.collection.mutable.Map[String, Double] = scala.collection.mutable.Map()

    var lines = io.Source.fromFile(file).getLines.toList
    lines.foreach { x =>
      if (x.size > 0) {
        var splitted = x.split(";")
        var methods = splitted.size / 3
        for (a <- 0 to methods - 1) {
          var name = splitted(a * 3).trim()
          var time = splitted(a * 3 + 1).trim().toDouble
          if (unaggregated.contains(name))
            unaggregated(name).add(time)
          else {
            var times : scala.collection.mutable.Set[Double] = scala.collection.mutable.Set()
            times.add(time)
            unaggregated.put(name, times)
          }

        }
      }
    }
    unaggregated.foreach(f => {
      var name = f._1
      var sum = 0.0
      f._2.foreach { x => sum += x }
      sum = sum / f._2.size
      //      if (sum == 88552.61666666668)
      //        println("")
      aggregated.put(name, sum)
    })

    var globalTime : scala.collection.mutable.Map[String, Double] = scala.collection.mutable.Map()
    var sum = 0.0

    aggregated.foreach(x => {
      sum += x._2
    })

    sum = sum / aggregated.size
    globalTime.put("allTime", sum)

    return globalTime
  }

  def createConfiguration(features : scala.collection.mutable.Map[String, String], measuement : scala.collection.mutable.Map[String, Double]) : Configuration = {

    var config : Configuration = new Configuration()
    config.readSolutionUnseparated(features)
    config.setMeasurements(measuement)

    return config
  }

}