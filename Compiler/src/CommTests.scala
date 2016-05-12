
import exastencils.logger._
import scala.collection.mutable.ListBuffer
import exastencils.constraints.Constraints

object CommTests {
  var numDims = 2

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

    fixedParams += "dimensionality" -> numDims

    fixedParams += "minLevel" -> 0
    fixedParams += "maxLevel" -> (if (2 == numDims) 11 else 7) // 2048^2 or 128^3 == 2^22 or 2^21 -> totalFragLength == 2 for 3D
    fixedParams += "comm_strategyFragment" -> 6

    fixedParams += "domain_onlyRectangular" -> true
    fixedParams += "domain_rect_generate" -> true

    fixedParams += "omp_enabled" -> false
    fixedParams += "omp_numThreads" -> 1

    fixedParams += "mpi_enabled" -> true

    fixedParams += "omp_parallelizeLoopOverFragments" -> true
    fixedParams += "omp_parallelizeLoopOverDimensions" -> false

    fixedParams += "l3tmp_generateL4" -> true
    fixedParams += "l3tmp_genForAutoTests" -> false

    fixedParams += "l3tmp_sisc" -> false
    fixedParams += "l3tmp_genStencilFields" -> false
    fixedParams += "l3tmp_genHDepStencils" -> false
    fixedParams += "l3tmp_genNonZeroRhs" -> false

    fixedParams += "l3tmp_exactSolution" -> "\"Polynomial\"" //"\"Kappa\""

    fixedParams += "l3tmp_smoother" -> "\"Jac\""
    fixedParams += "l3tmp_numPre" -> 3
    fixedParams += "l3tmp_numPost" -> 3
    fixedParams += "l3tmp_useSlotsForJac" -> true
    fixedParams += "l3tmp_useSlotVariables" -> true

    fixedParams += "poly_optLevel_fine" -> 3
    fixedParams += "opt_useAddressPrecalc" -> true
    fixedParams += "opt_vectorize" -> true
    fixedParams += "opt_unroll" -> 1

    fixedParams += "l3tmp_printError" -> false
    fixedParams += "l3tmp_useMaxNormForError" -> true

    fixedParams += "timer_type" -> "\"MPI_TIME\""

    fixedParams += "l3tmp_genTimersPerFunction" -> true
    fixedParams += "l3tmp_genTimersPerLevel" -> true
    fixedParams += "l3tmp_genTimersForComm" -> true
    fixedParams += "l3tmp_genCommTimersPerField" -> true
    fixedParams += "l3tmp_genCommTimersPerLevel" -> true

    fixedParams += "l3tmp_printTimersToFile" -> true
    fixedParams += "l3tmp_printAllTimers" -> true

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

    val settingsFile = args(0)
    val knowledgeFile = args(1)
    val platformFile = args(2)

    val outConfigs = ListBuffer[(String, String, String)]()

    var generateScript = ""
    var compileScript = "#!/bin/bash\n\n"
    var submitScript = "#!/bin/bash\n\n"

    for (d <- List(2, 3); useMPITypes <- List(true, false)) {
      numDims = d

      val fixedSettingsParams = initFixedSettingsParams
      val fixedKnowledgeParams = initFixedKnowledgeParams

      val partitions = numDims match {
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
          2 -> ((8, 4, 4), (1, 1, 1), (1, 2, 1)),
          4 -> ((8, 8, 4), (1, 1, 1), (1, 1, 2)),
          8 -> ((8, 8, 8), (1, 1, 1), (2, 1, 1)),
          16 -> ((16, 8, 8), (1, 1, 1), (1, 2, 1)),
          32 -> ((16, 16, 8), (1, 1, 1), (1, 1, 2)),
          64 -> ((16, 16, 16), (1, 1, 1), (2, 1, 1)),
          128 -> ((32, 16, 16), (1, 1, 1), (1, 2, 1)),
          256 -> ((32, 32, 16), (1, 1, 1), (1, 1, 2)),
          512 -> ((32, 32, 32), (1, 1, 1), (2, 1, 1)),
          1024 -> ((64, 32, 32), (1, 1, 1), (1, 2, 1)),
          2048 -> ((64, 64, 32), (1, 1, 1), (1, 1, 2)),
          4096 -> ((64, 64, 64), (1, 1, 1), (2, 1, 1)),
          8192 -> ((128, 64, 64), (1, 1, 1), (1, 2, 1)),
          16384 -> ((128, 128, 64), (1, 1, 1), (1, 1, 2)),
          28672 -> ((112, 128, 128), (1, 1, 1), (2, 1, 1)))
      }

      for (numNodes <- Stream.iterate(16)(_ * 2).takeWhile(_ <= 256)) {
        val configName = s"commTest_${numDims}_${numNodes}_${if (useMPITypes) "t" else "f"}"

        val outSettingsFile = { val tmp = settingsFile.split("\\."); tmp.dropRight(1).mkString(".") + "_" + configName + "." + tmp.last }
        val outKnowledgeFile = { val tmp = knowledgeFile.split("\\."); tmp.dropRight(1).mkString(".") + "_" + configName + "." + tmp.last }

        var setSettingsParams = Map[String, Any]()
        setSettingsParams += "outputPath" -> s"""\"generated/$configName/\""""
        setSettingsParams += "configName" -> s"""\"$configName\""""
        //setSettingsParams += "binary" -> s"""\"exastencils_$numNodes\""""

        var setKnowledgeParams = Map[String, Any]()

        val numBlocksTotal = partitions(numNodes)._1._1 * partitions(numNodes)._1._2 * partitions(numNodes)._1._3
        Constraints.condWarn(numNodes * 64 != numBlocksTotal, s"${numNodes * 64} != $numBlocksTotal")
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

        //setKnowledgeParams += "l3tmp_timerOuputFile" -> ("\"$WORK/ExaTemp/timings_" + configName + ".csv\"")
        setKnowledgeParams += "l3tmp_timerOuputFile" -> ("\"timings_" + configName + ".csv\"")

        setKnowledgeParams += "mpi_useCustomDatatypes" -> useMPITypes

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

    Logger.dbg("Runtime:\t" + math.round((System.nanoTime() - start) / 1e8) / 10.0 + " seconds")
  }
}
