package exastencils.spl.learning

import Jama.Matrix
import exastencils.spl.Configuration
import exastencils.spl.Feature
import com.sun.xml.internal.ws.message.RelatesToHeader
import exastencils.spl.FeatureModel

class ForwardFeatureSelection(featuresOfInterest : scala.collection.mutable.Set[Feature], numInterations : Int, measurements : Array[Configuration], nfpOfInterest : String) {

  // feature as the key and a tuple consisting of the textual representation of the feature and a global index as values
  var featureIdMap : scala.collection.mutable.Map[Feature, Tuple2[String, Int]] = scala.collection.mutable.Map()

  var featuresOfDomain : scala.collection.mutable.Map[String, Feature] = scala.collection.mutable.Map()

  var nameToFeatureAndID : scala.collection.mutable.Map[String, Tuple2[Feature, Int]] = scala.collection.mutable.Map()

  var iterationDelta = 0.0001
  val iterationDeltaRelative = 0.0001
  val iterationDeltaAbs = 0.1

  var withOffset = false
  var configurationOffset : scala.collection.mutable.Map[Configuration, Double] = scala.collection.mutable.Map()

  var Y2 : Jama.Matrix = null

  var validationConfigurations : Array[Configuration] = null

  def setValidationSet(configurations : Array[Configuration]) = {
    validationConfigurations = configurations
  }

  var useRealtive = false;

  /**
    * This method prepares all internal properties to start the forward feature selection algorithm.
    *
    */
  def apply(relative : Boolean) = {
    useRealtive = relative
    if (useRealtive)
      iterationDelta = iterationDeltaRelative
    else
      iterationDelta = iterationDeltaAbs
    featuresOfInterest.foreach(x => {
      if (x.isXorFeature && !x.isNumerical) {
        x.values.foreach { y =>
          {
            var idx = nameToFeatureAndID.size
            featureIdMap.put(x, new Tuple2(y, idx))
            featuresOfDomain.put(y, x)
            nameToFeatureAndID.put(y, new Tuple2(x, idx))
          }
        }
      } else {
        var idx = nameToFeatureAndID.size
        featureIdMap.put(x, new Tuple2(x.identifier, idx))
        featuresOfDomain.put(x.identifier, x)
        nameToFeatureAndID.put(x.identifier, new Tuple2(x, idx))
      }
    })

  }

  def init() = {
    Y2 = new Matrix(1, measurements.size)

    var pos = 0
    for (measurement <- measurements) {
      Y2.set(0, pos, measurement.nfpValues(nfpOfInterest))
      //      println(measurement.nfpValues(nfpOfInterest))
      pos += 1
    }
    Y2 = Y2.transpose()
  }

  /**
    * This method prepares all internal properties to start the forward feature selection algorithm. With the configurationOffset parameter, not the whole nfp value of the configurations are learned,
    * but only the difference between the whole value and the configurationOffset value. As a consequence, we perform a delta model learning approach here.
    *
    * @param configOffset the offset for the configurations
    *
    */
  def applyWithOffset(configOffset : scala.collection.mutable.Map[Configuration, Double]) = {
    withOffset = true;
    configurationOffset = configOffset
    featuresOfInterest.foreach(x => {
      if (x.isXorFeature && !x.isNumerical) {
        x.values.foreach { y =>
          {
            var idx = nameToFeatureAndID.size
            featureIdMap.put(x, new Tuple2(y, idx))
            featuresOfDomain.put(y, x)
            nameToFeatureAndID.put(y, new Tuple2(x, idx))
          }
        }
      } else {
        var idx = nameToFeatureAndID.size
        featureIdMap.put(x, new Tuple2(x.identifier, idx))
        featuresOfDomain.put(x.identifier, x)
        nameToFeatureAndID.put(x.identifier, new Tuple2(x, idx))
      }
    })

    Y2 = new Matrix(1, measurements.size)

    var pos = 0
    for (measurement <- measurements) {
      Y2.set(0, pos, measurement.nfpValues.get(nfpOfInterest).get - configurationOffset(measurement))
      pos += 1
    }
    Y2 = Y2.transpose()
    //perform()

  }

  var solutionSet : scala.collection.mutable.Set[FFS_Expression] = scala.collection.mutable.Set()

  /**
    *
    * This method starts the feature selection algorithm.
    *
    */
  def perform() = {
    init()
    // features and feature-interactions with the greatest impact

    var currIteration = 0

    var performIteration = true
    var minOverallError = Double.MaxValue

    solutionSet.add(new FFS_Expression(featuresOfDomain, "1"))

    while (performIteration) {
      var startTIME = System.currentTimeMillis()
      var minErrorOfThisIteration = Double.MaxValue
      var bestSolutionInIteration : scala.collection.mutable.Set[FFS_Expression] = null

      for (i <- nameToFeatureAndID) {

        // compute combinations
        var candidate = new FFS_Expression(featuresOfDomain, i._1)
        var combinationsForTesting = computeAllCombinations(solutionSet, candidate, measurements)
        //        println(combinationsForTesting)

        //        var candidateLog = new FFS_Expression(featuresOfDomain, "[ " + i._1 + "]")
        //        combinationsForTesting ++= (computeAllCombinations(solutionSet, candidateLog, measurements))

        for (j <- combinationsForTesting) {

          var combinationAsArray = j.toArray[FFS_Expression]

          // computeError for Combination
          var currError = computeErrorForCombination(combinationAsArray, measurements)
          //          var currError = computePercErrorForCombination(combinationAsArray, measurements)

          // if error < best error until now => store combination in solutionSet
          if (currError < minErrorOfThisIteration) {
            minErrorOfThisIteration = currError
            bestSolutionInIteration = j
          }
        }
      }

      // perform until no more iterations are beneficial 
      if ((iterationDelta > Math.abs(minErrorOfThisIteration - minOverallError)) || currIteration > numInterations || minErrorOfThisIteration > minOverallError) {
        performIteration = false
      }

      // store best combination 
      if (minErrorOfThisIteration < minOverallError) {
        minOverallError = minErrorOfThisIteration
        solutionSet = bestSolutionInIteration
      }

      println("Iteration " + currIteration + " finished with an abs error of: " + minOverallError + "  needing: " + (System.currentTimeMillis() - startTIME))
      var mod = this.getModelWithConstants(solutionSet.toArray[FFS_Expression])
      println(this.printModelWithConstants(mod._1, mod._2))
      currIteration += 1

    }

  }

  def perform(featureList : scala.collection.mutable.Set[String]) = {
    init()
    // features and feature-interactions with the greatest impact

    var currIteration = 0

    var performIteration = true
    var minOverallError = Double.MaxValue

    featureList.foreach { x => solutionSet.add(new FFS_Expression(featuresOfDomain, x)) }

    while (performIteration) {
      var minErrorOfThisIteration = Double.MaxValue
      var bestSolutionInIteration : scala.collection.mutable.Set[FFS_Expression] = null

      for (i <- nameToFeatureAndID) {

        // compute combinations
        var candidate = new FFS_Expression(featuresOfDomain, i._1)
        var combinationsForTesting = computeAllCombinations(solutionSet, candidate, measurements)

        //        var candidateLog = new FFS_Expression(featuresOfDomain, "[ " + i._1 + "]")
        //        combinationsForTesting ++= (computeAllCombinations(solutionSet, candidateLog, measurements))

        for (j <- combinationsForTesting) {

          var combinationAsArray = j.toArray[FFS_Expression]

          // computeError for Combination
          //          var currError = computeErrorForCombination(combinationAsArray, measurements)

          var currError = computePercErrorForCombination(combinationAsArray, measurements)

          // if error < best error until now => store combination in solutionSet
          if (currError < minErrorOfThisIteration) {
            minErrorOfThisIteration = currError
            bestSolutionInIteration = j
          }
        }
      }

      // perform until no more iterations are beneficial 
      if ((iterationDelta > Math.abs(minErrorOfThisIteration - minOverallError)) || currIteration > numInterations) {
        performIteration = false
      }

      // store best combination 
      if (minErrorOfThisIteration < minOverallError) {
        minOverallError = minErrorOfThisIteration
        solutionSet = bestSolutionInIteration
      } else {
        performIteration = false
      }

      println("Iteration " + currIteration + " finished with an abs error of: " + minOverallError)
      currIteration += 1

    }

  }

  def computeError(featureSetI : Array[FFS_Expression]) : Double = {
    return computePercErrorForCombination(featureSetI, measurements)
  }

  /**
    * Internal method computing the influence of the FFS_Expressions on the set of configurations used.
    *
    * @param featureSetI the array of FFS_expressions
    *
    */
  def getMatrixFromFeatureSet(featureSetI : Array[FFS_Expression]) : Jama.Matrix = {

    var result : Matrix = new Matrix(featureSetI.size, measurements.size)
    var m = 0

    for (i <- measurements) {
      var jCount = 0
      for (j <- featureSetI) {
        if (j.wellFormedExpression.contains("l3tmp_numPre"))
          print("")
        if (allFeaturesInConfig(i, j)) {
          //          var x = getValueForExpression(i, j)
          result.set(jCount, m, getValueForExpression(i, j))
          //          println(result.get(jCount, m))
        }
        jCount += 1
      }
      m += 1
    }
    return result
  }

  def computeError(constants : Jama.Matrix, combination : Array[FFS_Expression], configurations : Array[Configuration]) : Double = {
    var sumError = 0.0

    for (config <- configurations) {
      var predict = predictConfig(constants, combination, config)
      var realValue = config.nfpValues.get(nfpOfInterest).get
      sumError += Math.abs(predict - realValue)
    }
    return (sumError / configurations.size)
  }

  def computePercError(constants : Jama.Matrix, combination : Array[FFS_Expression], configurations : Array[Configuration]) : Double = {
    var sumError = 0.0

    for (config <- configurations) {
      var predict = predictConfig(constants, combination, config)
      var realValue : Double = -1
      if (withOffset) {
        realValue = configurationOffset(config)
      } else {
        realValue = config.nfpValues.get(nfpOfInterest).get
      }
      var curr = (Math.abs(realValue - predict) / realValue) * 100

      sumError += curr
    }

    return (sumError / configurations.size)
  }

  def predictConfig(constants : Jama.Matrix, combination : Array[FFS_Expression], config : Configuration) : Double = {
    var prediction : Double = 0.0

    var i = 0
    for (combi <- combination) {
      if (allFeaturesInConfig(config, combi)) {
        var newPart = getValueForExpression(config, combi) * constants.get(i, 0)

        prediction += newPart

      }
      i += 1
    }
    if (Math.abs(prediction) > 100000)
      print("")
    return prediction
  }

  def getAvgPredictionConfig(constants : Jama.Matrix, combination : Array[FFS_Expression], configs : scala.collection.mutable.Set[Configuration]) : Double = {
    var prediction : Double = 0.0

    configs.foreach { x =>
      {
        prediction += predictConfig(constants, combination, x)
      }
    }

    return prediction / configs.size
  }

  var expressionConfigValueCache : scala.collection.mutable.Map[String, scala.collection.mutable.Map[Configuration, Double]] = scala.collection.mutable.Map()

  def getValueForExpression(config : Configuration, list : FFS_Expression) : Double = {
    val expString = list.wellFormedExpression
    if (!expressionConfigValueCache.contains(expString)) {
      expressionConfigValueCache.put(expString, scala.collection.mutable.Map())
    }
    var configsWithCalculatedValues = expressionConfigValueCache(expString)
    if (configsWithCalculatedValues.contains(config)) {
      return configsWithCalculatedValues(config)
    }
    val value = list.evaluationOfRPN(config)
    configsWithCalculatedValues.put(config, value)
    return value
  }

  def allFeaturesInConfig(config : Configuration, combi : FFS_Expression) : Boolean = {
    var featuresFound = 0
    if (combi.numberOfParticipatingFeatures == 0) {
      return true
    }

    combi.participatingXorFeatures.foreach { x => featuresFound += 1 }

    combi.participatingBoolFeatures.foreach(x =>
      if (config.boolFeatures.contains(x) == true) featuresFound += 1)
    combi.participatingNumFeatures.foreach(x => if (config.numericalFeatureValues.contains(x)) featuresFound += 1)

    if (featuresFound < combi.numberOfParticipatingFeatures)
      return false

    return true
  }

  def getModelWithConstants(combination : Array[FFS_Expression]) : Tuple2[Jama.Matrix, Array[FFS_Expression]] = {

    var org : Matrix = getMatrixFromFeatureSet(combination)
    var temp = (org.transpose())
    var const : Matrix = null

    // FIXME in some cases the pinverse can not be computed 
    try {
      temp = temp.inverse()
      const = temp.times(Y2)

    } catch {
      case t : Throwable => const = org.times(Y2)
    }

    return Tuple2[Jama.Matrix, Array[FFS_Expression]](const, combination)
  }

  def computeErrorForCombination(combination : Array[FFS_Expression], configs : Array[Configuration]) : Double = {

    var org : Matrix = getMatrixFromFeatureSet(combination)
    var temp = (org.transpose())
    var const : Matrix = null

    // FIXME in some cases the pinverse can not be computed 
    try {
      temp = temp.inverse()
      const = temp.times(Y2)

    } catch {
      case t : Throwable => const = org.times(Y2)
    }

    if (validationConfigurations != null)
      return computeError(const, combination, validationConfigurations)
    else
      return computeError(const, combination, configs)

  }

  def computePercErrorForCombination(combination : Array[FFS_Expression], configs : Array[Configuration]) : Double = {

    var org : Matrix = getMatrixFromFeatureSet(combination)
    var temp = (org.transpose())
    var const : Matrix = null

    // FIXME in some cases the pinverse can not be computed 
    try {
      temp = temp.inverse()
      const = temp.times(Y2)

    } catch {
      case t : Throwable => const = org.times(Y2)
    }

    if (validationConfigurations != null)
      return computePercError(const, combination, validationConfigurations)
    else
      return computePercError(const, combination, configs)

  }

  def computeAllCombinations(combinationSet : scala.collection.mutable.Set[FFS_Expression], feature : FFS_Expression, configurations : Array[Configuration]) : scala.collection.mutable.Set[scala.collection.mutable.Set[FFS_Expression]] = {

    var combinations : scala.collection.mutable.Set[scala.collection.mutable.Set[FFS_Expression]] =
      scala.collection.mutable.Set()

    var independentConsidertAlready = false
    combinationSet.foreach(x => if (x.isTheSame(feature)) independentConsidertAlready = true)

    if (!independentConsidertAlready) {
      var independent : scala.collection.mutable.Set[FFS_Expression] = scala.collection.mutable.Set()
      combinationSet.foreach(x => independent.add(x))
      independent.add(feature)
      combinations.add(independent)
    }

    for (oldFeature <- combinationSet) {
      var newCombination : scala.collection.mutable.Set[FFS_Expression] = scala.collection.mutable.Set()

      // interactions with 1 make no sense
      if (!oldFeature.toString().toString().trim().equals("1")) {
        var newInteraction : FFS_Expression = new FFS_Expression(featuresOfDomain, oldFeature.toString() + " * " + feature.toString());
        var alreadyConsidered = false
        combinationSet.foreach(x => {
          if (x.isTheSame(newInteraction)) alreadyConsidered = true
        })
        newCombination.add(newInteraction)

        if (!alreadyConsidered) {
          for (otherFeature <- combinationSet) {
            if (!otherFeature.equals(oldFeature)) {
              newCombination.add(otherFeature)
            }
          }
          combinations.add(newCombination)

        }

      }
    }

    //    for (i <- combinationSet) {
    //      var combination : scala.collection.mutable.Set[FFS_Expression] = scala.collection.mutable.Set()
    //
    //      if (!i.toString().trim().equals("1")) {
    //        var subSet : FFS_Expression = new FFS_Expression(featuresOfDomain, i.toString() + " * " + feature.toString());
    //
    //        if (atLeastOneConfigurationHasCombination(configurations, subSet))
    //          combination.add(subSet)
    //
    //        for (k <- combinationSet) {
    //          if (!setContaintsExpression(combination, k))
    //            combination.add(k)
    //        }
    //        if (combination.size > 0)
    //
    //          combinations.add(combination)
    //      }
    //    }
    //
    //    var singleFeature : FFS_Expression = new FFS_Expression(featuresOfDomain, feature.toString())
    //    //    if(atLeastOneConfigurationHasCombination(configurations, singleFeature))
    //
    //    if (combinations.size == 1) {
    //      var firstConfig : scala.collection.mutable.Set[FFS_Expression] = scala.collection.mutable.Set()
    //      combinationSet.foreach(x => firstConfig.add(x))
    //
    //      //    if(singleFeature.size > 0 ){
    //      firstConfig.add(singleFeature)
    //      combinations.add(firstConfig)
    //    }
    //    }
    return combinations
  }

  def setContaintsExpression(combinationSet : scala.collection.mutable.Set[FFS_Expression], newExp : FFS_Expression) : Boolean = {
    combinationSet.foreach { x => if (x.equalExpression(newExp)) return true }
    return false;
  }

  def atLeastOneConfigurationHasCombination(configurations : Array[Configuration], combination : FFS_Expression) : Boolean = {

    for (config <- configurations) {
      if (allFeaturesInConfig(config, combination))
        return true

    }

    return false
  }

  /**
    * This methods returns a string consisting of the given set of FFS_Expressions and the coefficients of the expressions.
    *
    * @param constants: the coefficients of the expressions. This matrix is a 1xN Matrix for N expressions.
    * @param
    *
    */
  //  def printModelWithConstants(constants : Jama.Matrix, components : Array[FFS_Expression], decimalPlaces : Int) : String = {
  //    var result : String = ""
  //    for (i <- 0 to components.size - 1) {
  //      result += Precision.round(constants.get(i, 0), decimalPlaces) + " * " + components(i).toString() + " + "
  //    }
  //    result = result.substring(0, result.size - 3)
  //    return result
  //  }

  def printModelWithConstants(constants : Jama.Matrix, components : Array[FFS_Expression]) : String = {
    var result : String = ""
    for (i <- 0 to components.size - 1) {
      result += constants.get(i, 0) + " * " + components(i).toString() + " + "
    }
    result = result.substring(0, result.size - 3)
    return result
  }

  def modelToOSiLSyntax(constants : Jama.Matrix, components : Array[FFS_Expression]) : String = {
    var strBuild : StringBuilder = new StringBuilder()

    return modelToOSiLSyntax(constants, components, 0, strBuild)
  }

  def modelToOSiLSyntax(constants : Jama.Matrix, components : Array[FFS_Expression], index : Int, strBuild : StringBuilder) : String = {

    if (index < components.size - 1) {
      strBuild.append("<plus>\n")
      // call for first part 
      strBuild.append("<times>\n")
      strBuild.append("<number type=\"real\" value=\"" + constants.get(index, 0) + "\"/>\n")

      strBuild.append(components(index).toOSiL_syntax(nameToFeatureAndID, strBuild))
      strBuild.append("</times>\n")

      modelToOSiLSyntax(constants, components, index + 1, strBuild)
      strBuild.append("</plus>\n")
    } else {
      strBuild.append("<times>\n")
      strBuild.append("<number type=\"real\" value=\"" + constants.get(index, 0) + "\"/>\n")

      strBuild.append(components(index).toOSiL_syntax(nameToFeatureAndID, strBuild))
      strBuild.append("</times>\n")
    }

    return strBuild.toString();
  }

}