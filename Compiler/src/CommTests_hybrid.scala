
import scala.collection.mutable.ListBuffer

import exastencils.constraints._
import exastencils.logger._

object CommTests_hybrid {
  // memory estimation:
  // CC 2D/3D
  // 2+2=4 fields
  // 8*4*2^(3*level) * 4/3 Bytes
  // 16384/64-50 = 200 MB avail
  // 3D: 128^3 grid points per dim/frag
  // => ~90MB pro rank
  // with fragLenTotal == 2 => ~180MB pro rank
  // 2D: 2048^2 grid points per dim/frag
  // => ~180MB pro rank
  //
  // 2^22 grid points per frag == ~16M points per core == ~256M points per node

  def initFixedSettingsParams = {
    var fixedParams = Map[String, Any]()

    fixedParams += "user" -> "\"Sebastian\""

    fixedParams += "basePathPrefix" -> "\"./\""
    fixedParams += "l4file" -> "\"Compiler\\dsl\\CommTests.exa4\""
    fixedParams += "htmlLogFile" -> "\"Compiler\\log\\CommTests.html\""

    fixedParams += "produceHtmlLog" -> false
    fixedParams += "timeStrategies" -> false
    fixedParams += "timeStratPercentThreshold" -> 5

    fixedParams
  }

  def initFixedKnowledgeParams = {
    var fixedParams = Map[String, Any]()

    fixedParams += "minLevel" -> 0

    fixedParams += "domain_onlyRectangular" -> true
    fixedParams += "domain_rect_generate" -> true

    fixedParams += "mpi_enabled" -> true

    fixedParams += "omp_parallelizeLoopOverFragments" -> false
    fixedParams += "omp_parallelizeLoopOverDimensions" -> true

    fixedParams += "l3tmp_generateL4" -> true
    fixedParams += "l3tmp_genForAutoTests" -> false

    fixedParams += "l3tmp_sisc" -> false
    fixedParams += "l3tmp_genStencilFields" -> false
    fixedParams += "l3tmp_genHDepStencils" -> false
    fixedParams += "l3tmp_genNonZeroRhs" -> false
    fixedParams += "l3tmp_initSolWithRand" -> false
    fixedParams += "l3tmp_targetResReduction" -> 1e-5

    fixedParams += "l3tmp_exactSolution" -> "\"Polynomial\"" //"\"Kappa\""

    fixedParams += "l3tmp_smoother" -> "\"Jac\""
    fixedParams += "l3tmp_omega" -> 0.8
    fixedParams += "l3tmp_numPre" -> 3
    fixedParams += "l3tmp_numPost" -> 3
    fixedParams += "l3tmp_useSlotsForJac" -> true
    fixedParams += "l3tmp_useSlotVariables" -> true

    fixedParams += "l3tmp_maxNumCGSSteps" -> 256

    fixedParams += "poly_optLevel_fine" -> 3
    fixedParams += "poly_numFinestLevels" -> 100
    fixedParams += "opt_useAddressPrecalc" -> true
    fixedParams += "opt_vectorize" -> true
    fixedParams += "data_alignFieldPointers" -> true
    fixedParams += "simd_avoidUnaligned" -> true
    fixedParams += "opt_unroll" -> 1
    fixedParams += "omp_minWorkItemsPerThread" -> 128

    fixedParams += "l3tmp_printError" -> false
    fixedParams += "l3tmp_useMaxNormForError" -> true

    fixedParams += "timer_type" -> "\"MPI_TIME\""

    fixedParams += "l3tmp_genTimersPerFunction" -> true
    fixedParams += "l3tmp_genTimersPerLevel" -> true

    fixedParams += "l3tmp_printTimersToFile" -> true
    fixedParams += "l3tmp_printTimersToFileForEachRank" -> false // not enough memory on JuQueen
    fixedParams += "l3tmp_printAllTimers" -> true

    fixedParams += "experimental_addPerformanceEstimate" -> true

    fixedParams
  }

  def writeToFile(fileName : String, text : String) = {
    val outFile = new java.io.FileWriter(fileName)
    outFile.write(text)
    outFile.close
  }

  def main(args : Array[String]) : Unit = {
    // for runtime measurement
    val start : Long = System.nanoTime()

    if (args.size != 4) Logger.error("Invalid number of args")
    if ("g" == args(0) || "generate" == args(0))
      generate(args.tail)
    else if ("c" == args(0) || "collect" == args(0))
      collect(args.tail)

    Logger.dbg("Runtime:\t" + math.round((System.nanoTime() - start) / 1e8) / 10.0 + " seconds")
  }

  def generate(args : Array[String]) = {
    val settingsFile = args(0)
    val knowledgeFile = args(1)
    val platformFile = args(2)

    val outConfigs = ListBuffer[(String, String, String)]()

    var generateScript = ""
    var compileScript = "#!/bin/bash\n\n"
    var submitScript = "#!/bin/bash\n\n"

    for (
      numDims <- List(2, 3);
      useMPITypes <- List(true, false);
      mergeComm <- List( /*true,*/ false);
      mergeCommThres <- (if (mergeComm) List(0, 4) else List(0));
      commStrategy <- List(6, 26);
      useOMP <- List(true, false)
    ) {
      val fixedSettingsParams = initFixedSettingsParams
      val fixedKnowledgeParams = initFixedKnowledgeParams

      val partitions = if (useOMP) {
        numDims match {
          // numBlocks, numFrags, fragLen
          case 2 => Map(
            1 -> ((4, 4, 1), (1, 1, 1), (1, 1, 1)),
            2 -> ((8, 4, 1), (1, 1, 1), (1, 1, 1)),
            4 -> ((8, 8, 1), (1, 1, 1), (1, 1, 1)),
            8 -> ((16, 8, 1), (1, 1, 1), (1, 1, 1)),
            16 -> ((16, 16, 1), (1, 1, 1), (1, 1, 1)),
            32 -> ((32, 16, 1), (1, 1, 1), (1, 1, 1)),
            64 -> ((32, 32, 1), (1, 1, 1), (1, 1, 1)),
            128 -> ((64, 32, 1), (1, 1, 1), (1, 1, 1)),
            256 -> ((64, 64, 1), (1, 1, 1), (1, 1, 1)),
            512 -> ((128, 64, 1), (1, 1, 1), (1, 1, 1)),
            1024 -> ((128, 128, 1), (1, 1, 1), (1, 1, 1)),
            2048 -> ((256, 128, 1), (1, 1, 1), (1, 1, 1)),
            4096 -> ((256, 256, 1), (1, 1, 1), (1, 1, 1)),
            8192 -> ((512, 256, 1), (1, 1, 1), (1, 1, 1)),
            16384 -> ((512, 512, 1), (1, 1, 1), (1, 1, 1)),
            28672 -> ((896, 512, 1), (1, 1, 1), (1, 1, 1)))
          case 3 => Map(
            1 -> ((4, 2, 2), (1, 1, 1), (1, 1, 1)),
            2 -> ((4, 4, 2), (1, 1, 1), (1, 1, 1)),
            4 -> ((4, 4, 4), (1, 1, 1), (1, 1, 1)),
            8 -> ((8, 4, 4), (1, 1, 1), (1, 1, 1)),
            16 -> ((8, 8, 4), (1, 1, 1), (1, 1, 1)),
            32 -> ((8, 8, 8), (1, 1, 1), (1, 1, 1)),
            64 -> ((16, 8, 8), (1, 1, 1), (1, 1, 1)),
            128 -> ((16, 16, 8), (1, 1, 1), (1, 1, 1)),
            256 -> ((16, 16, 16), (1, 1, 1), (1, 1, 1)),
            512 -> ((32, 16, 16), (1, 1, 1), (1, 1, 1)),
            1024 -> ((32, 32, 16), (1, 1, 1), (1, 1, 1)),
            2048 -> ((32, 32, 32), (1, 1, 1), (1, 1, 1)),
            4096 -> ((64, 32, 32), (1, 1, 1), (1, 1, 1)),
            8192 -> ((64, 64, 32), (1, 1, 1), (1, 1, 1)),
            16384 -> ((64, 64, 64), (1, 1, 1), (1, 1, 1)),
            28672 -> ((112, 64, 64), (1, 1, 1), (1, 1, 1)))
        }
      } else {
        numDims match {
          // numBlocks, numFrags, fragLen
          case 2 => Map(
            1 -> ((8, 8, 1), (1, 1, 1), (1, 1, 1)),
            2 -> ((16, 8, 1), (1, 1, 1), (1, 1, 1)),
            4 -> ((16, 16, 1), (1, 1, 1), (1, 1, 1)),
            8 -> ((32, 16, 1), (1, 1, 1), (1, 1, 1)),
            16 -> ((32, 32, 1), (1, 1, 1), (1, 1, 1)),
            32 -> ((64, 32, 1), (1, 1, 1), (1, 1, 1)),
            64 -> ((64, 64, 1), (1, 1, 1), (1, 1, 1)),
            128 -> ((128, 64, 1), (1, 1, 1), (1, 1, 1)),
            256 -> ((128, 128, 1), (1, 1, 1), (1, 1, 1)),
            512 -> ((256, 128, 1), (1, 1, 1), (1, 1, 1)),
            1024 -> ((256, 256, 1), (1, 1, 1), (1, 1, 1)),
            2048 -> ((512, 256, 1), (1, 1, 1), (1, 1, 1)),
            4096 -> ((512, 512, 1), (1, 1, 1), (1, 1, 1)),
            8192 -> ((1024, 512, 1), (1, 1, 1), (1, 1, 1)),
            16384 -> ((1024, 1024, 1), (1, 1, 1), (1, 1, 1)),
            28672 -> ((1792, 1024, 1), (1, 1, 1), (1, 1, 1)))
          case 3 => Map(
            1 -> ((4, 4, 4), (1, 1, 1), (2, 1, 1)),
            2 -> ((4, 8, 4), (1, 1, 1), (2, 1, 1)),
            4 -> ((4, 8, 8), (1, 1, 1), (2, 1, 1)),
            8 -> ((8, 8, 8), (1, 1, 1), (2, 1, 1)),
            16 -> ((8, 16, 8), (1, 1, 1), (2, 1, 1)),
            32 -> ((8, 16, 16), (1, 1, 1), (2, 1, 1)),
            64 -> ((16, 16, 16), (1, 1, 1), (2, 1, 1)),
            128 -> ((16, 32, 16), (1, 1, 1), (2, 1, 1)),
            256 -> ((16, 32, 32), (1, 1, 1), (2, 1, 1)),
            512 -> ((32, 32, 32), (1, 1, 1), (2, 1, 1)),
            1024 -> ((32, 64, 32), (1, 1, 1), (2, 1, 1)),
            2048 -> ((32, 64, 64), (1, 1, 1), (2, 1, 1)),
            4096 -> ((64, 64, 64), (1, 1, 1), (2, 1, 1)),
            8192 -> ((64, 128, 64), (1, 1, 1), (2, 1, 1)),
            16384 -> ((64, 128, 128), (1, 1, 1), (2, 1, 1)),
            28672 -> ((112, 128, 128), (1, 1, 1), (2, 1, 1)))
        }
      }

      for (n <- Stream.iterate(1)(_ * 2).takeWhile(_ <= 32 * 1024)) {
        val numNodes = if (n > 28672) 28672 else n // limit node number
        val configName = s"commTest_${numDims}_${numNodes}_${useOMP}_${if (useMPITypes) "t" else "f"}_${commStrategy}_${if (mergeComm) s"t" else "f"}${mergeCommThres}"
        //val configName = s"commTest_${numDims}_${numNodes}_${if (useMPITypes) "t" else "f"}${if (6 == commStrategy) "" else s"_$commStrategy"}"

        val outSettingsFile = { val tmp = settingsFile.split("\\."); tmp.dropRight(1).mkString(".") + "_" + configName + "." + tmp.last }
        val outKnowledgeFile = { val tmp = knowledgeFile.split("\\."); tmp.dropRight(1).mkString(".") + "_" + configName + "." + tmp.last }

        var setSettingsParams = Map[String, Any]()
        setSettingsParams += "outputPath" -> s"""\"generated/$configName/\""""
        setSettingsParams += "configName" -> s"""\"$configName\""""
        //setSettingsParams += "binary" -> s"""\"exastencils_$numNodes\""""

        var setKnowledgeParams = Map[String, Any]()

        setKnowledgeParams += "dimensionality" -> numDims
        setKnowledgeParams += "maxLevel" -> ((if (2 == numDims) 11 else 7) + (if (useOMP) 1 else 0)) // 2048^2 or 128^3 == 2^22 or 2^21 -> totalFragLength == 2 for 3D / adaptation for 4 OMP threads

        setKnowledgeParams += "comm_strategyFragment" -> commStrategy

        val numBlocksTotal = partitions(numNodes)._1._1 * partitions(numNodes)._1._2 * partitions(numNodes)._1._3
        Constraints.condWarn(numNodes * (if (useOMP) 16 else 64) != numBlocksTotal, s"${numNodes * (if (useOMP) 16 else 64)} != $numBlocksTotal")
        setKnowledgeParams += "domain_numBlocks" -> numBlocksTotal
        setKnowledgeParams += "domain_rect_numBlocks_x" -> partitions(numNodes)._1._1
        setKnowledgeParams += "domain_rect_numBlocks_y" -> partitions(numNodes)._1._2
        setKnowledgeParams += "domain_rect_numBlocks_z" -> partitions(numNodes)._1._3

        val numFragsPerBlockTotal = partitions(numNodes)._2._1 * partitions(numNodes)._2._2 * partitions(numNodes)._2._3
        setKnowledgeParams += "domain_numFragmentsPerBlock" -> numFragsPerBlockTotal
        setKnowledgeParams += "domain_rect_numFragsPerBlock_x" -> partitions(numNodes)._2._1
        setKnowledgeParams += "domain_rect_numFragsPerBlock_y" -> partitions(numNodes)._2._2
        setKnowledgeParams += "domain_rect_numFragsPerBlock_z" -> partitions(numNodes)._2._3

        setKnowledgeParams += "domain_fragmentLength_x" -> partitions(numNodes)._3._1
        setKnowledgeParams += "domain_fragmentLength_y" -> partitions(numNodes)._3._2
        setKnowledgeParams += "domain_fragmentLength_z" -> partitions(numNodes)._3._3

        setKnowledgeParams += "mpi_numThreads" -> numBlocksTotal

        setKnowledgeParams += "omp_enabled" -> useOMP
        setKnowledgeParams += "omp_numThreads" -> (if (useOMP) 4 else 1)

        // 65536 dbl's per thread l2 cache
        // ~10k points per layer

        numDims match {
          case 2 => { // already l2 blocked -> 2k/4k per line
            setKnowledgeParams += "poly_tileSize_x" -> 0
            setKnowledgeParams += "poly_tileSize_y" -> 0
            setKnowledgeParams += "poly_tileSize_z" -> 0
          }
          case 3 => { // 72 * 128 w/o OMP || 32 * 256 w/ OMP
            setKnowledgeParams += "poly_tileSize_x" -> 0
            if (useOMP)
              setKnowledgeParams += "poly_tileSize_y" -> 32
            else
              setKnowledgeParams += "poly_tileSize_y" -> 72
            setKnowledgeParams += "poly_tileSize_z" -> 0
          }
        }
        //setKnowledgeParams += "l3tmp_timerOuputFile" -> ("\"$WORK/ExaTemp/timings_" + configName + ".csv\"")
        setKnowledgeParams += "l3tmp_timerOuputFile" -> ("\"timings_" + configName + ".csv\"")

        setKnowledgeParams += "mpi_useCustomDatatypes" -> useMPITypes

        setKnowledgeParams += "experimental_mergeCommIntoLoops" -> mergeComm
        setKnowledgeParams += "experimental_splitLoopsForAsyncComm" -> mergeComm
        setKnowledgeParams += "experimental_splitLoops_minInnerWidth" -> mergeCommThres

        // experimental_mergeCommIntoLoops is prevented by l3tmp_genTimersForComm
        setKnowledgeParams += "l3tmp_genTimersForComm" -> !mergeComm
        setKnowledgeParams += "l3tmp_genCommTimersPerField" -> !mergeComm
        setKnowledgeParams += "l3tmp_genCommTimersPerLevel" -> !mergeComm

        val settingsText = (fixedSettingsParams ++ setSettingsParams).map(p => p._1 + " = " + p._2).toList.sorted.mkString("\n")
        val knowledgeText = (fixedKnowledgeParams ++ setKnowledgeParams).map(p => p._1 + " = " + p._2).toList.sorted.mkString("\n")

        Logger.debug("Printing settings to " + outSettingsFile)
        writeToFile(outSettingsFile, settingsText)
        Logger.debug("Printing knowledge to " + outKnowledgeFile)
        writeToFile(outKnowledgeFile, knowledgeText)

        generateScript += (
          "java.exe -Xmx2G -Xms2G -cp \"C:\\Eclipse\\plugins\\*;.\\Compiler\\bin;.\\CompilerMacros\\CompilerMacros\\bin;.\\Compiler\\lib\\*\" Main "
          + outSettingsFile + " "
          + outKnowledgeFile + " "
          + platformFile + "\n")

        compileScript += s"cd $configName\n"
        compileScript += s"make clean && time make -j\n"
        //compileScript += s"mv exastencils ../exastencils_$numNodes\n"
        compileScript += s"cd ..\n"

        submitScript += s"llsubmit $configName/runJuQueen\n"
      }
    }

    submitScript += "watch llq -u her182\n"

    writeToFile(".\\Configs\\Sebastian\\generate_CommTests.bat", generateScript)
    writeToFile(".\\generated\\compile_CommTests", compileScript)
    writeToFile(".\\generated\\submit_CommTests", submitScript)
  }

  def collect(args : Array[String]) = {
    val settingsFile = args(0)
    val knowledgeFile = args(1)
    val platformFile = args(2)

    val onlyOneFile = true
    var outputTimes = ""

    if (onlyOneFile)
      outputTimes = "numDims;useOMP;useMPITypes;commStrategy;mergeComm;mergeCommThres;configName;" +
        "numThreads;timeToSolve;total_cycle_time;mean_cycle_time;coarse_grid_solve;total_computation_time;total_communication_time;\n"

    for (
      numDims <- List(2, 3);
      useMPITypes <- List(true, false);
      mergeComm <- List( /*true, */ false);
      mergeCommThres <- (if (mergeComm) List(0, 4) else List(0));
      commStrategy <- List(6, 26);
      useOMP <- List(true, false)
    ) {
      if (!onlyOneFile)
        outputTimes = "numThreads;timeToSolve;total_cycle_time;mean_cycle_time;coarse_grid_solve;total_computation_time;total_communication_time;\n"

      for (n <- Stream.iterate(1)(_ * 2).takeWhile(_ <= 32 * 1024)) {
        val numNodes = if (n > 28672) 28672 else n // limit node number
        val configName = s"commTest_${numDims}_${numNodes}_${useOMP}_${if (useMPITypes) "t" else "f"}_${commStrategy}_${if (mergeComm) s"t" else "f"}${mergeCommThres}"
        //val configName = s"commTest_${numDims}_${numNodes}_${if (useMPITypes) "t" else "f"}${if (6 == commStrategy) "" else s"_$commStrategy"}"

        val path = ".\\comm_paper\\"
        val timingsFile = path + "timings_" + configName + ".csv"

        if (!(new java.io.File(timingsFile)).exists)
          Logger.warn("Invalid file " + timingsFile)
        else {
          val timingsRaw = scala.io.Source.fromFile(timingsFile).mkString
          val timings = timingsRaw.trim.stripSuffix(" ;").split(" ; ").sliding(3, 3).collect({ case Array(n, tTotal, t) => (n, (t.toDouble, tTotal.toDouble)) }).toMap

          if (onlyOneFile)
            outputTimes += s"$numDims;$useOMP;$useMPITypes;$commStrategy;$mergeComm;$mergeCommThres" +
              s"commTest_${numDims}_${if (useMPITypes) "t" else "f"}_${commStrategy}_${if (mergeComm) s"t" else "f"}${mergeCommThres};"

          outputTimes += (numNodes * 64) + ";"
          outputTimes += timings.get("timeToSolve").get._1 + ";"
          outputTimes += timings.get("cycle").get._1 + ";" + timings.get("cycle").get._2 + ";"
          outputTimes += timings.get("cgs").get._1 + ";"
          val totalCommTime = timings.filter(_._1.startsWith("communication_")).map(_._2._1).sum
          outputTimes += (timings.get("timeToSolve").get._1 - totalCommTime) + ";"
          outputTimes += totalCommTime + ";"
          outputTimes += "\n"
        }
      }

      if (!onlyOneFile) {
        val filename = s"collectedTimes_${numDims}_${useOMP}_${if (useMPITypes) "t" else "f"}_${commStrategy}_${if (mergeComm) s"t" else "f"}${mergeCommThres}.csv"
        Logger.debug(s"Writing to file $filename")
        writeToFile(filename, outputTimes)
      }
    }
    if (onlyOneFile) {
      val filename = "collectedTimes.csv"
      Logger.debug(s"Writing to file $filename")
      writeToFile(filename, outputTimes)
    }
  }
}
