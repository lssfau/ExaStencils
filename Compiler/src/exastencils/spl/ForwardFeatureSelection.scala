package exastencils.spl

import Jama.Matrix

class ForwardFeatureSelection(featuresOfInterest : scala.collection.mutable.Set[Feature], numInterations : Int, measurements : Array[Configuration], nfpOfInterest : String) {

  // feature as the key and a tuple consisting of the textual representation of the feature and a global index as values
  var featureIdMap : scala.collection.mutable.Map[Feature, Tuple2[String, Int]] = scala.collection.mutable.Map()

  var featuresOfDomain : scala.collection.mutable.Map[String, Feature] = scala.collection.mutable.Map()

  var nameToFeatureAndID : scala.collection.mutable.Map[String, Tuple2[Feature, Int]] = scala.collection.mutable.Map()

  var interationDelta = 0.001

  var Y2 : Jama.Matrix = null

  /**
    * This method prepares all internal properties to start the forward feature selection algorithm.
    *
    */
  def apply() = {
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
      Y2.set(0, pos, measurement.nfpValues(nfpOfInterest))
      //      println(measurement.nfpValues(nfpOfInterest))
      pos += 1
    }
    Y2 = Y2.transpose()
    perform()

  }

  /**
    * This method prepares all internal properties to start the forward feature selection algorithm. With the configurationOffset parameter, not the whole nfp value of the configurations are learned,
    * but only the difference between the whole value and the configurationOffset value. As a consequence, we perform a delta model learning approach here.
    *
    * @param configurationOffset the offset for the configurations
    *
    */
  def applyWithOffset(configurationOffset : scala.collection.mutable.Map[Configuration, Double]) = {
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
    perform()

  }

  var solutionSet : scala.collection.mutable.Set[FFS_Expression] = scala.collection.mutable.Set()

  /**
    *
    * This method starts the feature selection algorithm.
    *
    */
  def perform() = {
    // features and feature-interactions with the greatest impact

    var currIteration = 0

    var performIteration = true
    var minOverallError = Double.MaxValue

    solutionSet.add(new FFS_Expression(featuresOfDomain, "1"))

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
      if ((interationDelta > Math.abs(minErrorOfThisIteration - minOverallError)) || currIteration > numInterations) {
        performIteration = false
      }

      // store best combination 
      if (minErrorOfThisIteration < minOverallError) {
        minOverallError = minErrorOfThisIteration
        solutionSet = bestSolutionInIteration
      }

      println("Iteration " + currIteration + " finished with an abs error of: " + minOverallError)
      currIteration += 1

    }

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
        if (allFeaturesInConfig(i, j)) {
          var x = getValueForExpression(i, j)
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
      var realValue = config.nfpValues.get(nfpOfInterest).get
      sumError += Math.abs(predict - realValue) / realValue
    }

    return (sumError / configurations.size)
  }

  def predictConfig(constants : Jama.Matrix, combination : Array[FFS_Expression], config : Configuration) : Double = {
    var prediction : Double = 0.0

    var i = 0
    for (combi <- combination) {
      if (allFeaturesInConfig(config, combi)) {
        prediction += getValueForExpression(config, combi) * constants.get(i, 0)

      }
      i += 1
    }
    return prediction
  }

  def getValueForExpression(config : Configuration, list : FFS_Expression) : Double = {
    return list.evaluationOfRPN(config)
  }

  def allFeaturesInConfig(config : Configuration, combi : FFS_Expression) : Boolean = {
    var featuresFound = 0
    if (combi.numberOfParticipatingFeatures == 0) {
      return true
    }

    combi.participatingXorFeatures.foreach { x => featuresFound += 1 }

    combi.participatingBoolFeatures.foreach(x =>
      if (config.boolFeatures(x) == true) featuresFound += 1)
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

    return computePercError(const, combination, configs)

  }

  def computeAllCombinations(combinationSet : scala.collection.mutable.Set[FFS_Expression], feature : FFS_Expression, configurations : Array[Configuration]) : scala.collection.mutable.Set[scala.collection.mutable.Set[FFS_Expression]] = {

    var combinations : scala.collection.mutable.Set[scala.collection.mutable.Set[FFS_Expression]] =
      scala.collection.mutable.Set()

    for (i <- combinationSet) {
      var combination : scala.collection.mutable.Set[FFS_Expression] = scala.collection.mutable.Set()

      var subSet : FFS_Expression = new FFS_Expression(featuresOfDomain, i.toString() + " * " + feature.toString());

      if (atLeastOneConfigurationHasCombination(configurations, subSet))
        combination.add(subSet)

      for (k <- combinationSet) {
        combination.add(k)
      }
      if (combination.size > 0)
        combinations.add(combination)

    }

    var singleFeature : FFS_Expression = new FFS_Expression(featuresOfDomain, feature.toString())
    //    if(atLeastOneConfigurationHasCombination(configurations, singleFeature))

    var firstConfig : scala.collection.mutable.Set[FFS_Expression] = scala.collection.mutable.Set()
    combinationSet.foreach(x => firstConfig.add(x))
    //    if(singleFeature.size > 0 ){
    firstConfig.add(singleFeature)
    combinations.add(firstConfig)
    //    }
    return combinations
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
  def printModelWithConstants(constants : Jama.Matrix, components : Array[FFS_Expression]) : String = {
    var result : String = ""
    for (i <- 0 to components.size - 1) {
      result += constants.get(i, 0) + " * " + components(i).toString() + " + "
    }
    result = result.substring(0, result.size - 3)
    return result
  }

}