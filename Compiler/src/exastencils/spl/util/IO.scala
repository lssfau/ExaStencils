package exastencils.spl.util

import exastencils.spl._
import java.io._

object IO {

  def parseMeasurement(file : String, numIterationsMeasured : Integer) : scala.collection.mutable.Map[String, Double] = {
    var maxValues : scala.collection.mutable.Map[String, Double] = scala.collection.mutable.Map()

    var toConsider : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()
    toConsider.add("postSmoothing")
    toConsider.add("correction")
    toConsider.add("settingSolution")
    toConsider.add("restriction")
    toConsider.add("preSmoothing")
    toConsider.add("residualUpdate")

    var lines = io.Source.fromFile(file).getLines.toList
    lines.foreach { x =>
      if (x.size > 0) {
        var splitted = x.split(";")
        var methods = splitted.size / 3
        for (a <- 0 to methods - 1) {
          var name = splitted(a * 3).trim()
          if (toConsider.contains(name)) {
            val value = splitted(a * 3 + 1).trim().toDouble
            if (maxValues.contains(name)) {
              if (maxValues(name) < value)
                maxValues.update(name, value)
            } else {
              maxValues.put(name, value)
            }

          }
        }
      }
    }

    var iterationTimes : scala.collection.mutable.Map[String, Double] = scala.collection.mutable.Map()

    maxValues.foreach(x => {
      iterationTimes.put(x._1, x._2 / numIterationsMeasured)
    })

    return iterationTimes
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

  def copyF(from : java.io.File, to : String) {
    val out = new java.io.BufferedWriter(new java.io.FileWriter(to));
    io.Source.fromFile(from).getLines.foreach(s => out.write(s + "\n"));
    out.close()
  }

  def printAllResultsToOneFile(configs : Array[Configuration], resultFile : String, featuresToConsider : Array[String]) = {
    var featureArray = featuresToConsider.toArray

    var nfpArray : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()

    var writer = new PrintWriter(new File(resultFile))

    var stringBuild : StringBuilder = new StringBuilder()

    stringBuild.append("configName" + ";")
    featureArray.foreach { x => stringBuild.append(x + ";") }
    nfpArray.foreach { x => stringBuild.append(x + ";") }
    writer.write(stringBuild.toString() + "\n")

    configs.foreach { conf =>
      {
        writer.write(printConfigToOneresultFile(conf, featureArray, nfpArray.toArray).toString().replace(".", ",") + "\n")
      }
    }

    writer.flush()
    writer.close()

  }

  def printConfigToOneresultFile(config : Configuration, featureArray : Array[String], nfpArray : Array[String]) : StringBuilder = {
    var stringBuilder : StringBuilder = new StringBuilder()

    stringBuilder.append(config.measurementName + ";")
    featureArray.foreach { feat =>
      {
        if (config.partialBaseConfig.contains(feat)) {
          stringBuilder.append(config.partialBaseConfig(feat) + ";")
        } else {
          var feature = FeatureModel.get(feat)
          if (feature.isNumerical && !feature.isXorFeature)
            stringBuilder.append(config.numericalFeatureValues(feature) + ";")
          else if (feature.isXorFeature)
            stringBuilder.append(config.xorFeatureValues(feature) + ";")
          else
            stringBuilder.append(config.boolFeatures(feature) + ";")
        }
      }
    }
    nfpArray.foreach { nfp =>
      {
        stringBuilder.append(config.nfpValues(nfp) + ";")
      }
    }
    return stringBuilder
  }

  def printAllResultsToOneFile(configs : Array[Configuration], configsValidation : Array[Configuration], nfps : scala.collection.mutable.Set[String], featuresToConsider : Array[String], fileName : String) = {

    nfps.add("NumIterations")
    var nfpArray = nfps.toArray

    // "E:/SISC_" + dimToConsider + "_" + ccVSvc +
    var resultFile = fileName + ".csv"
    var writer = new PrintWriter(new File(resultFile))

    var resultFileValidation = fileName + "_validation.csv"
    var writerValidation = new PrintWriter(new File(resultFileValidation))

    var stringBuild : StringBuilder = new StringBuilder()

    stringBuild.append("configName" + ";")
    featuresToConsider.foreach { x => stringBuild.append(x + ";") }
    nfpArray.foreach { x => stringBuild.append(x + ";") }
    writer.write(stringBuild.toString() + "\n")
    writerValidation.write(stringBuild.toString() + "\n")

    configs.foreach { conf =>
      {
        writer.write(printConfigToOneresultFile(conf, featuresToConsider, nfpArray) + "\n")
      }
    }

    if (configsValidation != null) {
      configsValidation.foreach { conf =>
        {
          writerValidation.write(printConfigToOneresultFile(conf, featuresToConsider, nfpArray) + "\n")
        }
      }
    }

    writer.flush()
    writer.close()

    writerValidation.flush()
    writerValidation.close()

  }

  def printConfigurationsOneFile(configs : Array[Configuration], lfaConfigs : scala.collection.mutable.Set[LFAConfig], nfps : scala.collection.mutable.Set[String], feturesToConsider : Array[String], fileName : String) = {

    nfps.add("NumIterations")
    var nfpArray = nfps.toArray

    // "E:/SISC_" + dimToConsider + "_" + ccVSvc
    var resultFile = fileName + "_modified_withLFA.csv"
    var writer = new PrintWriter(new File(resultFile))

    var stringBuild : StringBuilder = new StringBuilder()

    stringBuild.append("configName" + ";")
    feturesToConsider.foreach { x => stringBuild.append(x + ";") }
    nfpArray.foreach { x => stringBuild.append(x + ";") }
    stringBuild.append("NumItLFA;")
    writer.write(stringBuild.toString() + "\n")

    configs.foreach { conf =>
      {
        writer.write(IO.printConfigToOneresultFile(conf, feturesToConsider, nfpArray) + "")
        writer.write(conf.getLFAConfig(lfaConfigs).iterationsNeeded + "\n")
      }
    }

    writer.flush()
    writer.close()

  }

  val JASON_Max2d = "16384"
  val JASON_Max3d = "512"

  val epsilon = 1E-05

  def JSONParsing(lfaPath : String, dimToConsider : Int) : scala.collection.mutable.Set[LFAConfig] = {
    var lfaDir = new File(lfaPath)
    var lfaConfigs : scala.collection.mutable.Set[LFAConfig] = scala.collection.mutable.Set()
    lfaDir.listFiles().foreach { x =>
      {
        var cont : Map[String, Any] = scala.util.parsing.json.JSON.parseFull(scala.io.Source.fromFile(x).mkString).get.asInstanceOf[Map[String, Any]]

        var lfaConfig : LFAConfig = new LFAConfig()
        var config = cont.get("configuration").get.asInstanceOf[Map[String, Any]]

        lfaConfig.dimensionality = config("dimension").toString().toDouble.toInt
        lfaConfig.numPre = config("no_pre_smoothing").toString().toDouble.toInt
        lfaConfig.numPost = config("no_post_smoothing").toString().toDouble.toInt
        if (lfaConfig.numPre == lfaConfig.numPost && lfaConfig.numPre == 0)
          println("")
        lfaConfig.Smoother = config("smoother").toString()
        lfaConfig.numCGC = config("no_cgc").toString().toDouble.toInt
        lfaConfig.block_size = config("block_size").toString().toDouble.toInt
        lfaConfig.name = config("name").toString()
        if (config("name").toString().contains("true"))
          lfaConfig.constCoeff = true
        else
          lfaConfig.constCoeff = false

        if (lfaConfig.name.contains(JASON_Max2d) && dimToConsider == 2 || lfaConfig.name.contains(JASON_Max3d) && dimToConsider == 3) {

          var convergenceRate = cont.get("convergence_rates").get.asInstanceOf[List[Any]]

          var factor = 1.0
          var numIt = 0
          var break = false
          convergenceRate.foreach { x =>
            {
              if (!break) {
                factor = factor * x.toString().toDouble
                numIt += 1
                if (factor < epsilon)
                  break = true
              }
            }
          }
          var spectral_radius = cont.get("spectral_radius").get.toString().toDouble
          while (factor > epsilon) {
            factor = factor * spectral_radius
            numIt += 1
          }

          lfaConfig.iterationsNeeded = numIt
          lfaConfigs.add(lfaConfig)
        }
      }
    }
    return lfaConfigs
  }

}