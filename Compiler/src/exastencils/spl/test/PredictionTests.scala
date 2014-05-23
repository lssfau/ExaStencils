package exastencils.spl.test

import scala.io.Source
import exastencils.spl.FeatureModel
import exastencils.spl.Configuration
import exastencils.spl.Feature
import jp.kobe_u.copris.Var
import jp.kobe_u.copris.Constraint
import jp.kobe_u.copris.Add
import jp.kobe_u.copris.Term
import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter

class PredictionTests {

  var allConfigs : scala.collection.mutable.Set[Configuration] = scala.collection.mutable.Set()

  
  
  
  def readFile(fileName : String) = {
    val fileLines = Source.fromFile(fileName).getLines.toList

    var lineNr = 0
    while (lineNr < fileLines.size) {
      var lineContent = fileLines(lineNr)
      var elements = lineContent.split(";")

      var booleanFeatures   : scala.collection.mutable.Set[Feature]     = scala.collection.mutable.Set()
      var numericalFeatures : scala.collection.mutable.Map[Feature,Int] = scala.collection.mutable.Map()
      
      
      for (d <- 0 until elements.size-1) {
        if(FeatureModel.allFeatures.contains(elements(d)))
        	booleanFeatures.add(FeatureModel.allFeatures(elements(d)))
        else{ // numerical feature
           // TODO consider variable length of number suffix 
        	var numFeature = FeatureModel.allFeatures(elements(d).substring(0, elements(d).length()-1))
            var value = augmentString(elements(d).substring(elements(d).length()-1,elements(d).length())).toInt
            numericalFeatures.put(numFeature, value)
        }
      }
      var nfpValue = elements(9).toDouble

      var config = new Configuration
      config.readSolution(booleanFeatures)
      config.nfpValues .times  = nfpValue
      allConfigs.add(config)
      lineNr = lineNr + 1
    }
  }

//  def predictPerformanceFW(fwConfigs : scala.collection.mutable.Set[Configuration]) = {
//
//    var array = fwConfigs.toArray
//
//    var numberOfFWConfigs = 0
//
//    FeatureModel.copris.init
//
//    println("start with add fwConfigSolutions")
//    FeatureModel.allFeatures.filter(!_._2.isNumerical).foreach(x => FeatureModel.copris.int(Var(x._1), 0, 4500))
//
//    while (numberOfFWConfigs < fwConfigs.size) {
//      var currFWConfig = array(numberOfFWConfigs)
//      var selectedFeatures = currFWConfig.selectedBoolFeaturesAsArray
//
//      var nfpValue = allConfigs.filter(_.numberOfDifferentBooleanFeatures(currFWConfig) == 0).head._2
//
//      var constraint : Add = generateFeatureSelection(selectedFeatures)
//
//      FeatureModel.copris.add(constraint === nfpValue.toInt)
//      numberOfFWConfigs = numberOfFWConfigs + 1
//    }
//
//    if (FeatureModel.copris.find) {
//      println(FeatureModel.copris.solution)
//      interpretSolutionFW(FeatureModel.copris.solution)
//    }
//
//    predictConfigsAndMeasureDifference
//
//  }
  
  
//  def predictPerformanceFWUncertainty(fwConfigs : scala.collection.mutable.Set[Configuration]) = {
//
//    var array = fwConfigs.toArray
//
//    var numberOfFWConfigs = 0
//
//    FeatureModel.copris.init
//
//    println("start with add fwConfigSolutions")
//    FeatureModel.allFeatures.filter(!_._2.isNumerical).foreach(x => FeatureModel.copris.int(Var(x._1), 0, 400))
//
//    while (numberOfFWConfigs < fwConfigs.size) {
//      var currFWConfig = array(numberOfFWConfigs)
//      var selectedFeatures = currFWConfig.selectedBoolFeaturesAsArray
//
//      println(currFWConfig)
//      
//      var nfpValue = allConfigs.filter(_._1.numberOfDifferentBooleanFeatures(currFWConfig) == 0).head._2
//
//      var constraint : Add = generateFeatureSelection(selectedFeatures)
//
//      FeatureModel.copris.add(constraint === nfpValue.toInt)
//      numberOfFWConfigs = numberOfFWConfigs + 1
//    }
//
//    var solutionNr = 0
//    println("Start solving")
//    FeatureModel.copris.find
//   
//    var solutions = FeatureModel.copris.solutions
//    //println(solutions.size)
//    
//    while(solutions.hasNext){
//      var solution = solutions.next
//      println(solution)
//      interpretSolutionFW(solution)
//      predictConfigsAndMeasureDifference(solutionNr)
//      solutionNr+=1
//    }
//  }
  
  
  
  def addToFile(fileName: String, content : String) = {
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file,true))
        bw.append(content)
        bw.close()
  }
  
 
  
  def generateFeatureSelection(features :  Array[exastencils.spl.Feature]) : Add = {
    var sol = new Add(Var(features(0).identifier), Var(features(1).identifier))
    for(i <- 2 until features.length) yield {
      sol = new Add(sol, Var(features(i).identifier));
    }
    return sol
  }
  

//  def predictConfigsAndMeasureDifference() = {
//
//    var allPercentDifference = 0.0
//
//    allConfigs.foreach(currConfig => allPercentDifference += percentDifferencePredictMeasured(currConfig._1, currConfig._2))
//
//    println(allPercentDifference / allConfigs.size)
//
//  }

//  def predictConfigsAndMeasureDifference(variantNr : Int) = {
//
//    addToFile("./FeatureValues"+variantNr+".txt", getFeatureValues)
//    
//    var allPercentDifference = 0.0
//
//    allConfigs.foreach(currConfig => allPercentDifference += percentDifferencePredictMeasured(currConfig._1, currConfig._2, variantNr))
//
//    
//    addToFile("./AllVariants.csv", variantNr+";"+(allPercentDifference / allConfigs.size)+"\n")
//    println(allPercentDifference / allConfigs.size)
//    
//    
//
//  }

   def percentDifferencePredictMeasured(config : Configuration, measured : Double, variantNr : Int) : Double = {

    var predicted = predictNFPoneConfig(config)

    addToFile("./VariantPerdictions_"+variantNr+".csv", config + ";" + measured + ";" + predicted+ "\n")
    var difference = measured - predicted

    
    
    return difference.abs / measured
  }
  
  
  
  def percentDifferencePredictMeasured(config : Configuration, measured : Double) : Double = {

    var predicted = predictNFPoneConfig(config)

    println(config + " -> " + measured + " to:  " + predicted)
    var difference = measured - predicted

    return difference.abs / measured
  }

  def predictNFPoneConfig(config : Configuration) : Double = {
    var sumValue : Double = 0
    var feature = config.selectedBoolFeatures.iterator
    while (feature.hasNext) {
      var currFeat = feature.next
      sumValue += FeatureModel.allFeatures(currFeat.identifier).nfpValue
    }
    return sumValue
  }

  def interpretSolutionFW(solution : jp.kobe_u.copris.Solution) = {
    solution.intValues.foreach(a => FeatureModel.allFeatures(a._1.toString).nfpValue = a._2)
    //FeatureModel.allFeatures.foreach(f => println(f._1 + "  -> " + f._2.nfpValue))
  }
  
  def getFeatureValues(): String = {
    var sb = new scala.collection.mutable.StringBuilder()
    FeatureModel.allFeatures.foreach(f => sb ++=  (f._1 + "  -> " + f._2.nfpValue)+"\n" )
    return sb.toString
  }

}