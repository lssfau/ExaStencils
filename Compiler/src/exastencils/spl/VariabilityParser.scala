package exastencils.spl

import scala.io.Source
import exastencils.core.Settings

object VariabilityParser {
  def main(args: Array[String]): Unit = {

    val file = Settings.basePathPrefix +"/Compiler/src/exastencils/knowledge/Knowledge.scala"
    readFeatures(file)

  }
  
  /**
   * Split the given file and add all features (defined by lines with an // [...] to the model
   * 
   */
  def readFeatures(fileName: String) = {
   var knowledgeFile = Source.fromFile(fileName).mkString.split("\n")
   
   for (i <- 0 until knowledgeFile.length - 1) {
     var currStatement = knowledgeFile(i)
     if(currStatement.contains("//") && currStatement.contains("[") ){
       FeatureModel.addFeature(currStatement)
     }
   }
   
   print("fertig")
   
   FeatureModel.allFeatures.foreach(f => print(f._2.toString()))
   
  }
  
}

