package exastencils.spl.test

import exastencils.spl._
import exastencils.spl.samplingStrategies._
import scala.util.Random
import exastencils.spl.samplingStrategies.doe.PlackettBurmanDesign
import exastencils.spl.samplingStrategies.heuristics.FWHeuristic
import exastencils.spl.samplingStrategies.heuristics.PWHeuristic
import java.io.FileWriter
import java.io._


object SICS2015 {

  var featuresToConsider : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()
  

  val max_num_blocks_per_dim : Int = 1024
  val max_num_frags_per_block_per_dim : Int = 64
  val max_fragment_length_per_dim : Int = 64
  
  
  import exastencils.core.Settings
  
  def main(args : Array[String]) : Unit = {
    
    samplingWithPartitionedFeatureSpace()
    //orgTest()
  }  
  
  def samplingWithPartitionedFeatureSpace(): Boolean = {
    
    featureToConsider2D()
    
    val file = Settings.basePathPrefix + "/Compiler/src/exastencils/knowledge/Knowledge.scala"
    VariabilityParser.parseKnowlegdeFile(file)
    
    print("for filter ")
    println(FeatureModel.allFeatures.size)
    FeatureModel.filter(featuresToConsider)
    
    print("after filter ")
    println(FeatureModel.allFeatures.size)
    
   //  splitting feature in binary and numeric (numeric features with less than 5 values are used in the binary samplings)  
    var featuresBinary : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    var featuresNumeric : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    FeatureModel.allFeatures.foreach(x => {
      if(x._2.isXorFeature || !x._2.isNumerical) 
        featuresBinary.add(x._2) 
      else featuresNumeric.add(x._2)});
   
    println("numeric:::: "+featuresNumeric.size)
    featuresNumeric.foreach(x => {println(x.identifier)})
    println("--------------------------------")
    
    println("binary:::: "+featuresBinary.size)
    featuresBinary.foreach(x => {println(x.identifier)})
    println("--------------------------------")
    
    // partition numeric options to domain_ALL,maxLevel and other features
    var numericOptionsFirstSampling : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    var numericOptionsSecondSampling : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    featuresNumeric.foreach(x => 
      {if(x.identifier.startsWith("domain") || x.identifier.equals("maxLevel"))
          numericOptionsFirstSampling.add(x)
        else
          numericOptionsSecondSampling.add(x)
      }
    )
    println("Numeric First  Set size "+numericOptionsFirstSampling.size)
    println("Numeric Second Set size "+numericOptionsSecondSampling.size)
    
    // end numeric option splitting
    var binaryConfigs : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature, String]] = scala.collection.mutable.Set()
    // binary Sampling
    var fwSampling = new FWHeuristic(featuresBinary)
    binaryConfigs ++= fwSampling.getPoints()
    println("binary FW "+binaryConfigs.size)    

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
    
    var validPartitionsWithKey : scala.collection.mutable.Map[String,scala.collection.mutable.Map[Feature, Double]] = scala.collection.mutable.Map()
    numericSamplings.foreach(f => {
      if(isValidConfigDomainPartition(f, problemDefinition)){
        var key = ""
        f.foreach(f => key+=(f._1.identifier + ":"+f._2 + " "))
        println()
        validPartitionsWithKey.put(key,f)
      }else{
        var x = getValidConfiguration(f, problemDefinition, validPartitionsWithKey.size)
        var key = ""
        x.foreach(f => key+=(f._1.identifier + ":"+f._2 + " "))
        println()
        if(!validPartitionsWithKey.contains(key))
          validPartitionsWithKey.put(key,x)
      }
    })
    
    println("binary "+binaryConfigs.size)
    println("numeric "+validPartitionsWithKey.size)
    
    var validPartitions : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature, Double]] = scala.collection.mutable.Set()
    validPartitionsWithKey.foreach(x => validPartitions.add(x._2))
    
 
    
    var configurationsPreFiltered : scala.collection.mutable.Set[Configuration] = (new Configuration).generateConfigurations(binaryConfigs, validPartitions)
    println("configsBeforeFiltering "+configurationsPreFiltered.size)
//       
    var configurationsFiltered : scala.collection.mutable.Set[Configuration] = scala.collection.mutable.Set()
    configurationsPreFiltered.foreach ( x =>  
      {
       this.problemDefinition2D_ConstCoeff(x.partialBaseConfig)
       this.derivedParameters(x)
        if(isValidConfigDomainPartition(x)) 
          configurationsFiltered.add(x) 
      })

    println("configsAfterFiltering  "+ configurationsFiltered.size)
    
    pbd = new PlackettBurmanDesign(numericOptionsSecondSampling)
    pbd.initSeeds()
    pbd.setSeed(7, 49)
    numericSamplings = pbd.getPoints()
    println("second sampling combinations "+numericSamplings.size)
    
    
    var configurationsWithSecondSampling : scala.collection.mutable.Set[Configuration] = scala.collection.mutable.Set()
    configurationsFiltered.foreach { x =>  
      {
        numericSamplings.foreach(y => {
          var configCopy = x.copy();
          configCopy.addNumericOptions(y)
          if(isValidConfigSecondStep(configCopy))
            configurationsWithSecondSampling.add(configCopy)
        
        })
        
      }
    }    
    println("configsCombined  "+ configurationsWithSecondSampling.size)

    
    ////////////////////////////////////// end feature wise sampling in combination with pdb
    
    var pwSampling = new PWHeuristic(featuresBinary)
    binaryConfigs = pwSampling.getPoints()
    println("binary PW "+binaryConfigs.size)

    
    // numeric Sampling for domain partition
    pbd = new PlackettBurmanDesign(numericOptionsFirstSampling)
    pbd.initSeeds()
    pbd.setSeed(5, 25)
    numericSamplings = pbd.getPoints()
    
    validPartitionsWithKey = scala.collection.mutable.Map()
    numericSamplings.foreach(f => {
      if(isValidConfigDomainPartition(f, problemDefinition)){
        var key = ""
        f.foreach(f => key+=(f._1.identifier + ":"+f._2 + " "))
        println()
        validPartitionsWithKey.put(key,f)
      }else{
        var x = getValidConfiguration(f, problemDefinition, validPartitionsWithKey.size)
        var key = ""
        x.foreach(f => key+=(f._1.identifier + ":"+f._2 + " "))
        println()
        if(!validPartitionsWithKey.contains(key))
          validPartitionsWithKey.put(key,x)
      }
    })
    
    println("binary "+binaryConfigs.size)
    println("numeric "+validPartitionsWithKey.size)
    
    validPartitions = scala.collection.mutable.Set()
    validPartitionsWithKey.foreach(x => validPartitions.add(x._2))
    
 
    
    configurationsPreFiltered = (new Configuration).generateConfigurations(binaryConfigs, validPartitions)
    println("configsBeforeFiltering "+configurationsPreFiltered.size)
//       
    configurationsFiltered = scala.collection.mutable.Set()
    configurationsPreFiltered.foreach ( x =>  
      {
       this.problemDefinition2D_ConstCoeff(x.partialBaseConfig)
       this.derivedParameters(x)
        if(isValidConfigDomainPartition(x)) 
          configurationsFiltered.add(x) 
      })

    println("configsAfterFiltering  "+ configurationsFiltered.size)
    
    pbd = new PlackettBurmanDesign(numericOptionsSecondSampling)
    pbd.initSeeds()
    pbd.setSeed(5, 25)
    numericSamplings = pbd.getPoints()
    println("second sampling combinations "+numericSamplings.size)
    
    
    var configurationsWithSecondSamplingPW : scala.collection.mutable.Set[Configuration]  = scala.collection.mutable.Set()
    configurationsFiltered.foreach { x =>  
      {
        numericSamplings.foreach(y => {
          var configCopy = x.copy();
          configCopy.addNumericOptions(y)
          if(isValidConfigSecondStep(configCopy))
            configurationsWithSecondSamplingPW.add(configCopy)
        
        })
        
      }
    }    
    println("configsCombined PW  "+ configurationsWithSecondSamplingPW.size)

    configurationsWithSecondSampling ++= configurationsWithSecondSamplingPW
    
    println("configsCombined FW & PW "+ configurationsWithSecondSampling.size)
    
    printConfigurations(configurationsWithSecondSampling)
    
    return true
  }  
  
  
  def printConfigurations(configurations : scala.collection.mutable.Set[Configuration]) = {
    
    configToKnowledgeFile(configurations.head,1)
    
  }
  
  
  def configToKnowledgeFile(config : Configuration, index : Integer) = {
      
      var newFilelocation = "E://ScalaExaStencil/configsSiSC/knowledgeFile_"+index+".knowledge";
      
      val writer = new PrintWriter(new File(newFilelocation))

      var configContent = config.getKnowledgeFileContent()
      
      configContent.foreach { x => writer.write(x) }
      var partialBase = partialBaseConfigInformation()
      writer.write(partialBase)

      writer.write("l3tmp_timerOuputFile = \"timings_"+index+".csv\"\n")
      
      
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
    featuresToConsider.add("omp_minWorkItemsPerThread")
    
    featuresToConsider.add("mpi_useCustomDatatypes")
    
    featuresToConsider.add("maxLevel")
    
  }
  
  
  def partialBaseConfigInformation() : String = {
    var sb : StringBuilder = new StringBuilder()
    
    sb ++= "targetCompiler = \"IBMBG\" \n"
    sb ++= "targetCompilerVersion = 12\n"
    sb ++= "targetCompilerVersionMinor = 1\n"

    sb ++= "useDblPrecision = true"
  
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

    sb ++= "l3tmp_cgs = CG\n"
    
    sb ++= "l3tmp_useConditionsForRBGS = true\n"
    sb ++= "l3tmp_useSlotVariables = true\n"
    sb ++= "l3tmp_genHDepStencils = true\n"

    sb ++= "l3tmp_genTimersPerFunction = true\n"
    sb ++= "l3tmp_genTimersPerLevel = false\n"
    sb ++= "l3tmp_genTimersForComm = false\n"
    sb ++= "l3tmp_genCommTimersPerLevel = false\n"

    sb ++= "l3tmp_printAllTimers = false\n"
    sb ++= "l3tmp_printTimersToFile = true\n"


    sb ++= "l3tmp_exactSolution = \"Kappa_VC\"\n"
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
    sb ++= "l3tmp_printError = true\n"
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
    config.put("num_points_per_dim", 32768) // 32*1024
  }
  
 
  def problemDefinition2D_VarCoeff(config : scala.collection.mutable.Map[String, Any]) = {
    config.put("dimensionality", 2)
    config.put("l3tmp_genStencilFields", true)
    config.put("num_points_per_dim", 32768) // 32*1024
  }

  def problemDefinition3D_ConstCoeff(config : scala.collection.mutable.Map[String, Any]) = {
    config.put("dimensionality", 3)
    config.put("l3tmp_genStencilFields", false)
    config.put("num_points_per_dim", 1024)
  }
    
  def problemDefinition3D_VarCoeff(config : scala.collection.mutable.Map[String, Any]) = {
    config.put("dimensionality", 3)
    config.put("l3tmp_genStencilFields", true)
    config.put("num_points_per_dim", 1024)
  }
  
  
  
  
  def isValidConfigDomainPartition(config : Configuration ) : Boolean = {
    var num_unit_frags_x : Long = 1
    var num_unit_frags_y : Long = 1
    var num_unit_frags_z : Long = 1
    
    var frag_volume = 1.0
    
    //config.partialBaseConfig.put("domain_rect_numBlocks_x", domain_rect_numBlocks_x)
    
    if(config.partialBaseConfig.apply("dimensionality").asInstanceOf[Int] == 3){
      num_unit_frags_x = config.partialBaseConfig.apply("domain_rect_numBlocks_x").asInstanceOf[Double].toLong * (config.getNumericFeatureValue("domain_rect_numFragsPerBlock_x") * config.getNumericFeatureValue("domain_fragmentLength_x")).toLong
      num_unit_frags_y = config.partialBaseConfig.apply("domain_rect_numBlocks_y").asInstanceOf[Double].toLong * (config.getNumericFeatureValue("domain_rect_numFragsPerBlock_y") * config.getNumericFeatureValue("domain_fragmentLength_y")).toLong
      num_unit_frags_z = config.partialBaseConfig.apply("domain_rect_numBlocks_z").asInstanceOf[Double].toLong * (config.getNumericFeatureValue("domain_rect_numFragsPerBlock_z") * config.getNumericFeatureValue("domain_fragmentLength_z")).toLong
      frag_volume = 
         config.getNumericFeatureValue("domain_fragmentLength_x") * config.getNumericFeatureValue("domain_fragmentLength_y") * config.getNumericFeatureValue("domain_fragmentLength_z")
    }
    else {
      num_unit_frags_x = config.partialBaseConfig.apply("domain_rect_numBlocks_x").asInstanceOf[Double].toLong * (config.getNumericFeatureValue("domain_rect_numFragsPerBlock_x") * config.getNumericFeatureValue("domain_fragmentLength_x")).toLong
      num_unit_frags_y = config.partialBaseConfig.apply("domain_rect_numBlocks_y").asInstanceOf[Double].toLong * (config.getNumericFeatureValue("domain_rect_numFragsPerBlock_y") * config.getNumericFeatureValue("domain_fragmentLength_y")).toLong
      frag_volume = 
         config.getNumericFeatureValue("domain_fragmentLength_x") * config.getNumericFeatureValue("domain_fragmentLength_y")
    }
    
    if(!(getNumNodes(config) > 8))
      return false
    
    var mem_per_node = (8.0 * 4.0 * 4.0 / 3.0 * Math.pow(config.partialBaseConfig.apply("num_points_per_dim").asInstanceOf[Int], config.partialBaseConfig.apply("dimensionality").asInstanceOf[Int])) / getNumNodes(config)
    var memory : Double = 12L * 1024L * 1024L * 1024L
    if(!(mem_per_node <= memory))
      return false
    
    if(config.partialBaseConfig("dimensionality").asInstanceOf[Int] == 3){
      if(!(num_unit_frags_x == num_unit_frags_y) || !(num_unit_frags_y == num_unit_frags_z))
        return false
      
    }else{
      if(!(num_unit_frags_x == num_unit_frags_y))
        return false
    }
    
    if(config.partialBaseConfig.apply("domain_numBlocks").asInstanceOf[Double] < 8.0)
      return false
    
    if(!(frag_volume <= 64.0 && config.partialBaseConfig.apply("domain_numFragmentsPerBlock").asInstanceOf[Double] <= 64.0))  
      return false
    
    if(!(frag_volume == 1.0 || config.partialBaseConfig.apply("domain_numFragmentsPerBlock").asInstanceOf[Double] == 1.0))  
      return false  
      
    return true;
  }
  
  def isValidConfigDomainPartition(config : scala.collection.mutable.Map[Feature, Double] , problemDefinition : scala.collection.mutable.Map[String, Any] ) : Boolean = {

    // calculation of dependent features 
    var domain_rect_numBlocks_x = ((problemDefinition("num_points_per_dim").asInstanceOf[Int] / (
        config(FeatureModel.get("domain_fragmentLength_x")) * Math.pow(2, config(FeatureModel.get("maxLevel")))))
        /  config(FeatureModel.get("domain_rect_numFragsPerBlock_x")))
    
    var domain_rect_numBlocks_y = ((problemDefinition("num_points_per_dim").asInstanceOf[Int] / (
        config(FeatureModel.get("domain_fragmentLength_y")) * Math.pow(2, config(FeatureModel.get("maxLevel")))))
        /  config(FeatureModel.get("domain_rect_numFragsPerBlock_y")))

    var domain_rect_numBlocks_z : Double = 1.0
    
    if(problemDefinition("dimensionality").asInstanceOf[Int] == 3){
      domain_rect_numBlocks_z = ((problemDefinition("num_points_per_dim").asInstanceOf[Int] / (
        config(FeatureModel.get("domain_fragmentLength_z")) * Math.pow(2, config(FeatureModel.get("maxLevel"))))) 
        /  config(FeatureModel.get("domain_rect_numFragsPerBlock_z")))
    }    
   
   var num_frags_per_block_total : Double = 1.0
   var frag_volume = 1.0
   var num_blocks_total = 1.0
   if(problemDefinition("dimensionality").asInstanceOf[Int] == 3){   
     num_frags_per_block_total =
         config(FeatureModel.get("domain_rect_numFragsPerBlock_x")) * config(FeatureModel.get("domain_rect_numFragsPerBlock_y")) * config(FeatureModel.get("domain_rect_numFragsPerBlock_z"))
     frag_volume = 
         config(FeatureModel.get("domain_fragmentLength_x")) * config(FeatureModel.get("domain_fragmentLength_y")) * config(FeatureModel.get("domain_fragmentLength_z"))
     num_blocks_total = 
         domain_rect_numBlocks_x * domain_rect_numBlocks_y * domain_rect_numBlocks_z
   }else{
     num_frags_per_block_total =
         config(FeatureModel.get("domain_rect_numFragsPerBlock_x")) * config(FeatureModel.get("domain_rect_numFragsPerBlock_y"))
     frag_volume = 
         config(FeatureModel.get("domain_fragmentLength_x")) * config(FeatureModel.get("domain_fragmentLength_y"))
     num_blocks_total = 
         domain_rect_numBlocks_x * domain_rect_numBlocks_y
   }
   
   var domain_numBlocks= num_blocks_total
   var domain_numFragmentsPerBlock = num_frags_per_block_total
   var mpi_numThreads = num_blocks_total
   
   var omp_enabled = false
   var omp_numThreads = 1.0
   var omp_parallelizeLoopOverFragments = false
   var omp_parallelizeLoopOverDimensions = false
   
   if(num_frags_per_block_total == 1.0 && frag_volume == 1.0){
     omp_enabled = false
     omp_numThreads =  1.0
     omp_parallelizeLoopOverFragments = false
     omp_parallelizeLoopOverDimensions = false
   }else if(num_frags_per_block_total > frag_volume){
     omp_enabled = true
     omp_numThreads =  num_frags_per_block_total
     omp_parallelizeLoopOverFragments = true
     omp_parallelizeLoopOverDimensions = false
   }else{
     omp_enabled = true
     omp_numThreads =  frag_volume
     omp_parallelizeLoopOverFragments = false
     omp_parallelizeLoopOverDimensions = true
   }
   
    var num_unit_frags_x : Long = 1
    var num_unit_frags_y : Long = 1
    var num_unit_frags_z : Long = 1
        
    if(problemDefinition("dimensionality").asInstanceOf[Int] == 3){
      num_unit_frags_x = domain_rect_numBlocks_x.toLong * (config(FeatureModel.get("domain_rect_numFragsPerBlock_x")) * config(FeatureModel.get("domain_fragmentLength_x"))).toLong
      num_unit_frags_y = domain_rect_numBlocks_y.toLong * (config(FeatureModel.get("domain_rect_numFragsPerBlock_y")) * config(FeatureModel.get("domain_fragmentLength_y"))).toLong
      num_unit_frags_z = domain_rect_numBlocks_z.toLong * (config(FeatureModel.get("domain_rect_numFragsPerBlock_z")) * config(FeatureModel.get("domain_fragmentLength_z"))).toLong
    }
    else {
      num_unit_frags_x = domain_rect_numBlocks_x.toLong * (config(FeatureModel.get("domain_rect_numFragsPerBlock_x")) * config(FeatureModel.get("domain_fragmentLength_x"))).toLong
      num_unit_frags_y = domain_rect_numBlocks_y.toLong * (config(FeatureModel.get("domain_rect_numFragsPerBlock_y")) * config(FeatureModel.get("domain_fragmentLength_y"))).toLong
    }
    
    var numNodes : Double = getNumNodes(mpi_numThreads.toInt, omp_numThreads.toInt)
    
    
    if(!(numNodes > 8.0))
      // println("Not enough nodes in use")
      return false
    
//    println(numNodes)  
      
    var mem_per_node = (8.0 * 4.0 * 4.0 / (3.0 * Math.pow(problemDefinition("num_points_per_dim").asInstanceOf[Int], problemDefinition("dimensionality").asInstanceOf[Int]))) / numNodes
    var memory : Double = 12L * 1024L * 1024L * 1024L
    if(!(mem_per_node <= memory))
    //println("Memory requirements for each node are too high")
      return false
    
    if(problemDefinition("dimensionality").asInstanceOf[Int] == 3){
      if(!(num_unit_frags_x == num_unit_frags_y) || !(num_unit_frags_y == num_unit_frags_z))
        return false
      
    }else{
      if(!(num_unit_frags_x == num_unit_frags_y))
        //println("Not square")
        return false
    }
    
    if(!(domain_numBlocks >= 8))
      //println("Not enough blocks to distribute :%s" % num_blocks_total)
      return false
    
    if(!(frag_volume <= 64.0 && domain_numFragmentsPerBlock <= 64))  
      //println("Too many omp threads :%s" % self.get_value("omp_numThreads", 1))
      return false
    
    if(!(frag_volume == 1.0 || domain_numFragmentsPerBlock == 1))  
      //print("Two different omp parallelization strategies chosen concurrently")
      return false  
      
    return true;
  }
   
  def isValidConfigSecondStep(config : Configuration ) : Boolean = {

    if(config.getNumericFeatureValue("l3tmp_numPre").toInt + config.getNumericFeatureValue("l3tmp_numPost").toInt < 1)
      return false  

    if(config.getNumericFeatureValue("l3tmp_numPre").toInt + config.getNumericFeatureValue("l3tmp_numPost").toInt > 12)
      return false    
          
    // select l3tmp_useSlotsForJac only if Jac is the Smoother
    if(config.boolFeatures(FeatureModel.allFeatures("l3tmp_useSlotsForJac")) && 
        config.xorFeatureValues(FeatureModel.allFeatures("l3tmp_smoother")).asInstanceOf[String].equals("Jac"))
      return false

    // un-splotted Jac
    if(config.xorFeatureValues(FeatureModel.allFeatures("l3tmp_smoother")).asInstanceOf[String].equals("Jac") &&    
      !config.boolFeatures(FeatureModel.allFeatures("l3tmp_useSlotsForJac"))){
      
      if(!(config.getNumericFeatureValue("l3tmp_numPre").toInt % 2 == 0) || !(config.getNumericFeatureValue("l3tmp_numPre").toInt % 2 == 0))
        return false
    }
    
    if(config.getNumericFeatureValue("minLevel").toInt >= config.getNumericFeatureValue("maxLevel").toInt)
      return false 
      
    return true;
  }
  
  def derivedParameters(config: Configuration) = {
    var domain_rect_numBlocks_x = (((config.partialBaseConfig("num_points_per_dim").asInstanceOf[Int] / (
        config.getNumericFeatureValue("domain_fragmentLength_x") * Math.pow(2, config.getNumericFeatureValue("maxLevel"))))) 
        /  config.getNumericFeatureValue("domain_rect_numFragsPerBlock_x"))

     config.partialBaseConfig.put("domain_rect_numBlocks_x", domain_rect_numBlocks_x)
        
     
    var domain_rect_numBlocks_y = (((config.partialBaseConfig("num_points_per_dim").asInstanceOf[Int] / (
        config.getNumericFeatureValue("domain_fragmentLength_y") * Math.pow(2, config.getNumericFeatureValue("maxLevel"))))) 
        /  config.getNumericFeatureValue("domain_rect_numFragsPerBlock_y"))

     config.partialBaseConfig.put("domain_rect_numBlocks_y", domain_rect_numBlocks_y)
     
     if(config.partialBaseConfig("dimensionality").asInstanceOf[Int] == 3){   

       var domain_rect_numBlocks_z = (((config.partialBaseConfig("num_points_per_dim").asInstanceOf[Int] / (
        config.getNumericFeatureValue("domain_fragmentLength_z") * Math.pow(2, config.getNumericFeatureValue("maxLevel")))))
        /  config.getNumericFeatureValue("domain_rect_numFragsPerBlock_z"))

     config.partialBaseConfig.put("domain_rect_numBlocks_z", domain_rect_numBlocks_z)
     }
   
   var num_frags_per_block_total = 1.0
   var frag_volume = 1.0
   var num_blocks_total = 1.0
   if(config.partialBaseConfig("dimensionality").asInstanceOf[Int] == 3){   
     num_frags_per_block_total =
         config.getNumericFeatureValue("domain_rect_numFragsPerBlock_x") * config.getNumericFeatureValue("domain_rect_numFragsPerBlock_y") * config.getNumericFeatureValue("domain_rect_numFragsPerBlock_z")
     frag_volume = 
         config.getNumericFeatureValue("domain_fragmentLength_x") * config.getNumericFeatureValue("domain_fragmentLength_y") * config.getNumericFeatureValue("domain_fragmentLength_z")
     num_blocks_total = 
         config.partialBaseConfig("domain_rect_numBlocks_x").asInstanceOf[Int] * config.partialBaseConfig("domain_rect_numBlocks_y").asInstanceOf[Int] * config.partialBaseConfig("domain_rect_numBlocks_z").asInstanceOf[Int]
   }else{
     num_frags_per_block_total =
         config.getNumericFeatureValue("domain_rect_numFragsPerBlock_x") * config.getNumericFeatureValue("domain_rect_numFragsPerBlock_y")
    frag_volume = 
         config.getNumericFeatureValue("domain_fragmentLength_x") * config.getNumericFeatureValue("domain_fragmentLength_y")
    num_blocks_total = 
         config.partialBaseConfig("domain_rect_numBlocks_x").asInstanceOf[Double] * config.partialBaseConfig("domain_rect_numBlocks_y").asInstanceOf[Double]
   }
   
   config.partialBaseConfig.put("domain_numBlocks", num_blocks_total)
   config.partialBaseConfig.put("domain_numFragmentsPerBlock", num_frags_per_block_total)
   config.partialBaseConfig.put("mpi_numThreads", num_blocks_total)
   
   if(num_frags_per_block_total == 1.0 && frag_volume == 1.0){
    config.partialBaseConfig.put("omp_enabled", false)
    config.partialBaseConfig.put("omp_numThreads", 1.0)
    config.partialBaseConfig.put("omp_parallelizeLoopOverFragments", false)
    config.partialBaseConfig.put("omp_parallelizeLoopOverDimensions", false)
   }else if(num_frags_per_block_total > frag_volume){
     config.partialBaseConfig.put("omp_enabled", true)
     config.partialBaseConfig.put("omp_numThreads", num_frags_per_block_total)
     config.partialBaseConfig.put("omp_parallelizeLoopOverFragments", true)
     config.partialBaseConfig.put("omp_parallelizeLoopOverDimensions", false) 
   }else{
     config.partialBaseConfig.put("omp_enabled", true)
     config.partialBaseConfig.put("omp_numThreads", frag_volume)
     config.partialBaseConfig.put("omp_parallelizeLoopOverFragments", false)
     config.partialBaseConfig.put("omp_parallelizeLoopOverDimensions", true)
   }
   

   if(config.partialBaseConfig("dimensionality").asInstanceOf[Int] == 2){
     if(config.xorFeatureValues.contains(FeatureModel.allFeatures("l3tmp_smoother"))){
       config.partialBaseConfig.put("l3tmp_omega ", 1.16)
     }else{
       config.partialBaseConfig.put("l3tmp_omega ", 0.79)
     }     
   }else{
     if(config.xorFeatureValues.contains(FeatureModel.allFeatures("l3tmp_smoother"))){
       config.partialBaseConfig.put("l3tmp_omega ", 1.19)
     }else{
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
    return (config.partialBaseConfig("mpi_numThreads").asInstanceOf[Double] * config.partialBaseConfig("omp_numThreads").asInstanceOf[Double]) / 64
  }
  
  def identifyValidConfigurations(nonValidConfigs : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature, Double]]) : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature, Double]] = {
    
    
    return null
  }
  
  def enumAllValidDomainPartitions3D( problemDefinition : scala.collection.mutable.Map[String, Any]) = {
    for(rectX <- 1 to 64){
       for(rectY <- 1 to 64){
         for(rectZ <- 1 to 64){
           for(fragX <- 1 to 64){
             for(fragY <- 1 to 64){
               for(fragZ <- 1 to 64){
                 for(maxL <- 4 to 12){
                    var config : scala.collection.mutable.Map[Feature, Double] = scala.collection.mutable.Map()
                    config.put(FeatureModel.get("domain_rect_numFragsPerBlock_x"), rectX) // 64
                    config.put(FeatureModel.get("domain_rect_numFragsPerBlock_y"), rectY) // 64
                    config.put(FeatureModel.get("domain_rect_numFragsPerBlock_z"), rectZ) // 64
                    
                    config.put(FeatureModel.get("domain_fragmentLength_x"), fragX) // 64
                    config.put(FeatureModel.get("domain_fragmentLength_y"), fragY) // 64
                    config.put(FeatureModel.get("domain_fragmentLength_z"), fragZ) // 64
                    
                    config.put(FeatureModel.get("maxLevel"), maxL) // 12
    
                    if(isValidConfigDomainPartition(config, problemDefinition)){
                      config.foreach(f => print(f._1.identifier + ": "+f._2 + "  "))
                      println()
                    }
                   
                 }
               }
             }
           }
         }
       }
    }
    print("finished")
    
  }
  
  
  def enumAllValidDomainPartitions( problemDefinition : scala.collection.mutable.Map[String, Any]) : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature, Double]] = {
    
    var validConfigs : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature, Double]] = scala.collection.mutable.Set()
    
    var fw : FileWriter = new FileWriter("E:\\LFA-Tool\\testPythonGenerator\\scala.txt")
    
    var pows : scala.collection.mutable.Set[Int] = scala.collection.mutable.Set()
    pows+=1
    pows+=2
    pows+=4
    pows+=8
    pows+=16
    pows+=32
    pows+=64
    
    for(rectX <- 1 to 64){
       for(rectY <- 1 to 64){
         for(fragX <- 1 to 64){
           for(fragY <- 1 to 64){
             for(maxL <- 4 to 12){
               
                if(pows.contains(rectX) && pows.contains(rectY) && pows.contains(fragX) && pows.contains(fragY) ){
                
                  var config : scala.collection.mutable.Map[Feature, Double] = scala.collection.mutable.Map()
                  config.put(FeatureModel.get("domain_rect_numFragsPerBlock_x"), rectX) // 64
                  config.put(FeatureModel.get("domain_rect_numFragsPerBlock_y"), rectY) // 64
                  
                  config.put(FeatureModel.get("domain_fragmentLength_x"), fragX) // 64
                  config.put(FeatureModel.get("domain_fragmentLength_y"), fragY) // 64
                  
                  config.put(FeatureModel.get("maxLevel"), maxL) // 12
  
                  if(isValidConfigDomainPartition(config, problemDefinition)){
                    config.foreach(f => fw.write(f._1.identifier + ": "+f._2 + "  "))
                    fw.write("\n")
                    validConfigs.add(config)
                  }
                }
             }
           }
         }
       }
    }
    fw.close()
    return validConfigs
  }
  
  
  def enumAllValidDomainPartitions_small( problemDefinition : scala.collection.mutable.Map[String, Any]) : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature, Double]] = {
    var validConfigs : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature, Double]] = scala.collection.mutable.Set()
    
    for(rectX <- 1 to 10){
       for(rectY <- 1 to 10){
         for(fragX <- 1 to 10){
           for(fragY <- 1 to 10){
             for(maxL <- 4 to 12){
                var config : scala.collection.mutable.Map[Feature, Double] = scala.collection.mutable.Map()
                config.put(FeatureModel.get("domain_rect_numFragsPerBlock_x"), rectX) // 64
                config.put(FeatureModel.get("domain_rect_numFragsPerBlock_y"), rectY) // 64
                
                config.put(FeatureModel.get("domain_fragmentLength_x"), fragX) // 64
                config.put(FeatureModel.get("domain_fragmentLength_y"), fragY) // 64
                
                config.put(FeatureModel.get("maxLevel"), maxL) // 12

                if(isValidConfigDomainPartition(config, problemDefinition)){
                  config.foreach(f => print(f._1.identifier + ": "+f._2 + "  "))
                  println()
                  validConfigs.add(config)
                }
               
             }
           }
         }
       }
    }
    return validConfigs
  }
  
  def getValidConfiguration(nonValidConfig : scala.collection.mutable.Map[Feature, Double], problemDefinition : scala.collection.mutable.Map[String, Any], seed : Int ) : scala.collection.mutable.Map[Feature, Double] = {
    // nearest neighbor search
    var config : scala.collection.mutable.Map[Feature, Double] = scala.collection.mutable.Map()
    var featureList : scala.collection.mutable.Map[Int, Feature] = scala.collection.mutable.Map()
    nonValidConfig.foreach(x => {
      config.put(x._1, x._2)
      featureList.put(featureList.size,x._1)
    })

    var r :Random = new Random(seed)
    
    do{
      // random step
      var idx = r.nextInt(config.size)

      
      var featureToChange : Feature = featureList(idx)
      
      var signum  = r.nextBoolean()
      if(signum){
       if(config(featureToChange) + featureToChange.stepsize <= featureToChange.maxValue)
         config.update(featureToChange, config(featureToChange) + featureToChange.stepsize)
      }else{
        if(config(featureToChange) - featureToChange.stepsize >= featureToChange.minValue)
          config.update(featureToChange, config(featureToChange) - featureToChange.stepsize)
      }
    }while(!isValidConfigDomainPartition(config, problemDefinition))
    
    return config
  }
  
}