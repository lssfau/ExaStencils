package exastencils.spl

import scala.io.Source
import exastencils.core.Settings
import exastencils.knowledge.Knowledge

object VariabilityParser {
  def main(args: Array[String]): Unit = {

    val file = Settings.basePathPrefix + "/Compiler/src/exastencils/knowledge/Knowledge.scala"
    readFeatures(file)

    generateVariant()

  }

  /**
   * Split the given file and add all features (defined by lines with an // [...] to the model
   *
   */
  def readFeatures(fileName: String) = {
    var knowledgeFile = Source.fromFile(fileName).mkString.split("\n")

    for (i <- 0 until knowledgeFile.length - 1) {
      var currStatement = knowledgeFile(i)
      if (currStatement.contains("//") && currStatement.contains("[")) {
        FeatureModel.addFeature(currStatement)
      }
    }
    FeatureModel.createFeatureDependenciesByFeatureNames()
  }

  def generateVariant() = {
    //    FeatureModel.printAllConfigurations()

    var config = FeatureModel.getMinimalConfig()

    defineVariant(config)

  }

  def defineVariant(configuration: Configuration) = {

    // TODO
//    Knowledge.getClass().getDeclaredFields().foreach(f =>
//      if (FeatureModel.allFeatures.contains(f.getName())) {
//        f.setAccessible(true)
//        var feature = FeatureModel.allFeatures(f.getName())
//        if (feature.isNumerical)
//          f.setInt(null, configuration.getValueOfNumericalFeature(feature.identifier))
//        else if (configuration.selectedBoolFeatures.contains(feature)) {
//          println(f.getName())
//          f.setBoolean(null, true)
//        }
//      })

  }

}

