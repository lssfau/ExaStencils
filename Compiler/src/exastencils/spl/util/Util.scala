package exastencils.spl.util

import exastencils.spl._
import java.io._

object Util {

  def createConfiguration(features : scala.collection.mutable.Map[String, String], measurement : scala.collection.mutable.Map[String, Double]) : Configuration = {

    var config : Configuration = new Configuration()
    config.readSolutionUnseparated(features)
    config.setMeasurements(measurement)

    return config
  }

  def createConfigsBasedOnMeasurements(basicDir : String, featuresConsidered : scala.collection.mutable.Set[String]) : scala.collection.mutable.Set[Configuration] = {

    var configurations : scala.collection.mutable.Set[Configuration] = scala.collection.mutable.Set()

    var basicPath : File = new File(basicDir)

    var knowledgeFilesPath : File = new File(basicDir + File.separator + "knowledgeFiles")
    var resultFilePath : File = new File(basicDir + File.separator + "measurements")

    //    var featuresConsidered : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()
    //    FeatureModel.allFeatures.foreach(x => featuresConsidered.add(x._1))

    var measurements : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()
    resultFilePath.listFiles().foreach(x => measurements.add(x.getName))

    knowledgeFilesPath.listFiles().foreach { x =>
      //      var featureValues = IO.parseKnowledgeFile(x.getAbsolutePath)

      var featureValuesConsidered : scala.collection.mutable.Map[String, String] = null;
      if (featuresConsidered != null)
        featureValuesConsidered = IO.parseKnowledgeFile(x.getAbsolutePath, featuresConsidered)
      else
        featureValuesConsidered = IO.parseKnowledgeFile(x.getAbsolutePath)

      if (measurements.contains(featureValuesConsidered("l3tmp_timerOuputFile").replaceAll("\"", ""))) {
        var numIterationsMeasured = numberOfIterations(basicDir + File.separator + "measurements" + File.separator + featureValuesConsidered("l3tmp_timerOuputFile").replaceAll("\"", ""))
        var measurement : scala.collection.mutable.Map[String, Double] = IO.parseMeasurement(basicDir + File.separator + "measurements" + File.separator + featureValuesConsidered("l3tmp_timerOuputFile").replaceAll("\"", ""), numIterationsMeasured)

        var timeCGS = coarseGridSolverTime(basicDir + File.separator + "measurements" + File.separator + featureValuesConsidered("l3tmp_timerOuputFile").replaceAll("\"", ""), numIterationsMeasured)
        measurement.put("NumIterations", numIterationsMeasured)
        measurement.put("cgsTime", timeCGS)
        val conf = Util.createConfiguration(featureValuesConsidered, measurement)
        conf.numNode = getNumNodesFromKnowledgeFile(x.getAbsolutePath, featuresConsidered)
        conf.measurementName = featureValuesConsidered("l3tmp_timerOuputFile")
        configurations.add(conf)

      }

    }

    return configurations
  }

  def getNumNodesFromKnowledgeFile(file : String, featureNames : scala.collection.mutable.Set[String]) : Int = {

    var lines = io.Source.fromFile(file).getLines.toList
    var omp_numThreads : Int = 0
    var mpi_numThreads : Int = 0

    lines.foreach { x =>
      if (x.size > 0) {
        var splitted = x.split("=")
        if (splitted(0).trim().equals("omp_numThreads"))
          omp_numThreads = splitted(1).trim().toInt
        if (splitted(0).trim().equals("mpi_numThreads"))
          mpi_numThreads = splitted(1).trim().toInt

      }
    }
    return getNumNodes(mpi_numThreads, omp_numThreads).toInt
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

  def numberOfIterations(file : String) : Int = {
    var sum = 0.0
    var number = 0.0
    var overtime = 0.0

    var lines = io.Source.fromFile(file).getLines.toList
    lines.foreach { x =>
      if (x.size > 0) {
        var splitted = x.split(";")
        var methods = splitted.size / 3
        for (a <- 0 to methods - 1) {
          var name = splitted(a * 3).trim()
          if (name.trim().equals("cycle")) {
            var time = splitted(a * 3 + 2).trim().toDouble
            overtime = splitted(a * 3 + 1).trim().toDouble
            sum = time
            number += 1
          }
        }
      }
    }

    var it = (overtime / sum)

    return Math.round(it.toFloat)
  }

  def coarseGridSolverTime(file : String, numIterations : Int) : Double = {

    var neededParts : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()
    neededParts.add("postSmoothing")
    neededParts.add("correction")
    neededParts.add("settingSolution")
    neededParts.add("restriction")
    neededParts.add("preSmoothing")
    neededParts.add("residualUpdate")

    var lines = io.Source.fromFile(file).getLines.toList
    var splitted = lines(0).split(";")
    var methods = splitted.size / 3

    var timeOtherParts = 0.0
    var timeCycle = 0.0
    for (a <- 0 to methods - 1) {
      var name = splitted(a * 3).trim()
      if (name.trim().equals("cycle")) {
        timeCycle = splitted(a * 3 + 1).trim().toDouble
      }
      if (neededParts.contains(name)) {
        timeOtherParts += splitted(a * 3 + 1).trim().toDouble
      }
    }

    return (timeCycle - timeOtherParts) / numIterations
  }

  def createConfigsBasedOnMeasurementOneFile(file : String, featuresToConsider : scala.collection.mutable.Set[String]) : scala.collection.mutable.Set[Configuration] = {

    var configurations : scala.collection.mutable.Set[Configuration] = scala.collection.mutable.Set()
    var lines = io.Source.fromFile(file).getLines.toList

    val headLine = lines(0).split(";")

    for (a <- 1 to lines.size - 1) {
      val parts = lines(a).split(";")
      var features : scala.collection.mutable.Map[String, String] = scala.collection.mutable.Map()
      var nfps : scala.collection.mutable.Map[String, Double] = scala.collection.mutable.Map()
      var name = parts(0)
      // "configName" is in position 0
      for (b <- 1 to parts.length - 1) {
        val headPart = headLine(b)
        if (featuresToConsider.contains(headPart)) {
          // feature
          features.put(headPart, parts(b))
        } else {
          // nfps
          nfps.put(headPart, parts(b).toDouble)
        }
      }
      val conf = Util.createConfiguration(features, nfps)
      conf.measurementName = name
      configurations.add(conf)

    }

    return configurations
  }

}