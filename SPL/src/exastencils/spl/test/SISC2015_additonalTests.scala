package exastencils.spl.test

import exastencils.spl._
import exastencils.spl.util._
import java.io._

object SISC2015_additonalTests {

  def nonRunningTestExtraction(featuresToConsider : Array[String]) = {
    var fileNonTerminatingConfigs = "E:\\JobsForJuQueen\\SISC_Paper\\2dCC\\nonTerminatingIn30Min.txt"
    var dirKnowledgeFiles : File = new File("E:\\JobsForJuQueen\\SISC_Paper\\2dCC\\KnowledgeFilesAndVariants_afterSlurm\\knowledgeFiles")

    var lines = io.Source.fromFile(fileNonTerminatingConfigs).getLines.toList

    var nonWorkingConfigs : scala.collection.mutable.Set[Configuration] = scala.collection.mutable.Set()
    var nonWorkingConfigs2 : scala.collection.mutable.Set[Configuration] = scala.collection.mutable.Set()
    var nfps : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()

    lines.foreach { x =>
      {
        if (x.startsWith("time runjob")) {
          var configIndex = x.substring(x.indexOf("Exa/config") + "Exa/config".length(), x.indexOf("/exastencils.exe"))
          dirKnowledgeFiles.listFiles().foreach { kFile =>
            {
              if (kFile.getName.contains(configIndex + ".")) {

                var featureValues = IO.parseKnowledgeFile(kFile.getAbsolutePath)
                var measurement : scala.collection.mutable.Map[String, Double] = scala.collection.mutable.Map()
                val config = Util.createConfiguration(featureValues, measurement)
                config.measurementName = kFile.getName
                nonWorkingConfigs.add(config)

                IO.copyF(kFile, "E:/JobsForJuQueen/SISC_Paper/2dCC/knowFilesNonWorking/" + kFile.getName())
              }
            }
          }

        }
      }
    }

    IO.printAllResultsToOneFile(nonWorkingConfigs.toArray, "E:\\JobsForJuQueen\\SISC_Paper\\2dCC\\moreThan30Min.csv", featuresToConsider)

  }

  def lfaTests(baseCaseStudyDir : String, dimToConsider : Int, ccVSvc : String, featuresToConsider : scala.collection.mutable.Set[String]) = {

    var configs = Util.createConfigsBasedOnMeasurements(baseCaseStudyDir + "2dCC_NoRandom", featuresToConsider).toArray

    var lfaConfigs = IO.JSONParsing(baseCaseStudyDir + "LFA_version1\\", dimToConsider)
    var lfaConfigsCaseStudy : scala.collection.mutable.Set[LFAConfig] = scala.collection.mutable.Set()

    if (dimToConsider == 2 && ccVSvc.equals("cc"))
      lfaConfigs.foreach { x => if (x.dimensionality == 2 && x.constCoeff && x.block_size == 4 && x.name.contains("16384")) lfaConfigsCaseStudy.add(x) }

    if (dimToConsider == 2 && ccVSvc.equals("vc"))
      lfaConfigs.foreach { x => if (x.dimensionality == 2 && !x.constCoeff && x.block_size == 4 && x.name.contains("16384")) lfaConfigsCaseStudy.add(x) }

    if (dimToConsider == 3 && ccVSvc.equals("cc"))
      lfaConfigs.foreach { x => if (x.dimensionality == 3 && x.constCoeff && x.block_size == 4 && x.name.contains("16384")) lfaConfigsCaseStudy.add(x) }

    if (dimToConsider == 3 && ccVSvc.equals("vc"))
      lfaConfigs.foreach { x => if (x.dimensionality == 3 && !x.constCoeff && x.block_size == 4 && x.name.contains("16384")) lfaConfigsCaseStudy.add(x) }

    var lfaConfigs_2 = IO.JSONParsing(baseCaseStudyDir + "LFA_version2\\", dimToConsider)
    var lfaConfigsCaseStudy_2 : scala.collection.mutable.Set[LFAConfig] = scala.collection.mutable.Set()

    if (dimToConsider == 2 && ccVSvc.equals("cc"))
      lfaConfigs_2.foreach { x => if (x.dimensionality == 2 && x.constCoeff && x.block_size == 4 && x.name.contains("16384")) lfaConfigsCaseStudy_2.add(x) }

    if (dimToConsider == 2 && ccVSvc.equals("vc"))
      lfaConfigs_2.foreach { x => if (x.dimensionality == 2 && !x.constCoeff && x.block_size == 4 && x.name.contains("16384")) lfaConfigsCaseStudy_2.add(x) }

    if (dimToConsider == 3 && ccVSvc.equals("cc"))
      lfaConfigs_2.foreach { x => if (x.dimensionality == 3 && x.constCoeff && x.block_size == 4 && x.name.contains("16384")) lfaConfigsCaseStudy_2.add(x) }

    if (dimToConsider == 3 && ccVSvc.equals("vc"))
      lfaConfigs_2.foreach { x => if (x.dimensionality == 3 && !x.constCoeff && x.block_size == 4 && x.name.contains("16384")) lfaConfigsCaseStudy_2.add(x) }

    var sbVer1 : StringBuilder = new StringBuilder()
    var sum1 : Int = 0
    var sbVer2 : StringBuilder = new StringBuilder()
    var sum2 : Int = 0

    sbVer1.append("pre;post;smoother;numNodes;predicted;measured\n")
    sbVer2.append("pre;post;smoother;numNodes;predicted;measured\n")

    configs.foreach { x =>
      {
        var lfaConfig = x.getLFAConfig(lfaConfigsCaseStudy)
        var iterations : Int = x.getLFAConfig(lfaConfigsCaseStudy).iterationsNeeded
        var iterations_2 : Int = x.getLFAConfig(lfaConfigsCaseStudy_2).iterationsNeeded
        var itInConfig : Int = x.nfpValues("NumIterations").toInt

        sbVer1.append(lfaConfig.numPre + ";" + lfaConfig.numPost + ";" + lfaConfig.Smoother + ";" + x.numNode + ";" + iterations + ";" + itInConfig + "\n")
        sum1 += Math.abs(iterations - itInConfig)
        sbVer2.append(lfaConfig.numPre + ";" + lfaConfig.numPost + ";" + lfaConfig.Smoother + ";" + x.numNode + ";" + iterations_2 + ";" + itInConfig + "\n")
        sum2 += Math.abs(iterations_2 - itInConfig)
      }
    }

    var lfaFile1 = baseCaseStudyDir + "lfa1.csv"
    var writer1 = new PrintWriter(new File(lfaFile1))
    writer1.write(sbVer1.toString())
    writer1.flush()
    writer1.close()

    println("sum error LFA1 : " + sum1)

    var lfaFile2 = baseCaseStudyDir + "lfa2.csv"
    var writer2 = new PrintWriter(new File(lfaFile2))
    writer2.write(sbVer2.toString())
    writer2.flush()
    writer2.close()

    println("sum error LFA2 : " + sum2)

  }

}