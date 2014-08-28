package exastencils.spl

import Jama.Matrix

//TODO Only work for boolean features 
//TODO Implement function learning and consider numerical features

class ForwardFeatureSelection(numInterations : Int , measurements: Array[Configuration]) {

  var featureIdMap: scala.collection.mutable.Map[Feature, Int] = scala.collection.mutable.Map()

  var interationDelta = 0.001
 
  var Y2: Jama.Matrix = null

  def apply() = {
    var notConsider: scala.collection.mutable.Set[String] = scala.collection.mutable.Set();
    // TODO remove dirty hack --> do not consider features that are mandatory in all configurations 
    notConsider.add("Prototype")
    notConsider.add("post")
    notConsider.add("pre")
    notConsider.add("l3tmp_cgs")
    notConsider.add("l3tmp_smoother")

    // create projection of feature to id for later identification of feature with greatest impact
    FeatureModel.allFeatures.filter(x => !notConsider.contains(x._2.identifier)).foreach(x => featureIdMap.put(x._2, featureIdMap.size))

    Y2 = new Matrix(1, measurements.size)

    var pos = 0
    for (measurement <- measurements) {
      Y2.set(0, pos, measurement.nfpValues.times)
      pos += 1
    }
    Y2 = Y2.transpose()

    perform()

  }

  var solutionSet: scala.collection.mutable.Set[scala.collection.mutable.Set[Feature]] = scala.collection.mutable.Set()

  def perform() = {
    // features and feature-interactions with the greatest impact

    var currIteration = 0

    var performIteration = true
    var minOverallError = Double.MaxValue

    while (performIteration) {

      var minErrorOfThisIteration = Double.MaxValue
      var bestSolutionInIteration: scala.collection.mutable.Set[scala.collection.mutable.Set[Feature]] = null

      for (i <- featureIdMap) {
        // compute combinations
        var combinationsForTesting = computeAllCombinations(solutionSet, i._1, measurements)

        for (j <- combinationsForTesting) {

          var combinationAsArray = j.toArray[scala.collection.mutable.Set[Feature]]

          // computeError for Combination
          var currError = computeErrorForCombination(combinationAsArray, measurements)

          // if error < best error until now => store combination in solutionSet
          if (currError < minErrorOfThisIteration) {
            minErrorOfThisIteration = currError
            bestSolutionInIteration = j
          }
        }
      }
      // perform until no more iterations are beneficial 
      if ((interationDelta > Math.abs(minErrorOfThisIteration - minOverallError)) || currIteration > numInterations)
        performIteration = false

      // store best combination 
      if (minErrorOfThisIteration < minOverallError) {
        minOverallError = minErrorOfThisIteration
        solutionSet = bestSolutionInIteration
      }
      println("Iteration " + currIteration + " finished with an error off  " + minOverallError)
      currIteration += 1

    }

  }

  def getMatrixFromFeatureSet(featureSetI: Array[scala.collection.mutable.Set[Feature]]): Jama.Matrix = {

    var result: Matrix = new Matrix(featureSetI.size, measurements.size)
    var m = 0

    for (i <- measurements) {
      var jCount = 0
      for (j <- featureSetI) {
        if (allFeaturesInConfig(i, j)) {
          result.set(jCount, m, getValueForFeature(i, j))
        }
        jCount += 1
      }
      m += 1
    }
    return result
  }

  def computeError(constants: Jama.Matrix, combination: Array[scala.collection.mutable.Set[exastencils.spl.Feature]], configurations: Array[Configuration]): Double = {
    var sumError = 0.0

    for (config <- configurations) {
      var predict = predictValue(constants, combination, config)
      var realValue = config.nfpValues.times
      sumError += Math.abs(predict - realValue)
    }

    return (sumError / configurations.size)
  }

  def predictValue(constants: Jama.Matrix, combination: Array[scala.collection.mutable.Set[exastencils.spl.Feature]], config: Configuration): Double = {
    var prediction: Double = 0.0

    var i = 0
    for (combi <- combination) {
      if (allFeaturesInConfig(config, combi)) {
        prediction += getValueForFeature(config, combi) * constants.get(i, 0)

      }
      i += 1
    }
    return prediction
  }

  def getValueForFeature(config: Configuration, list: scala.collection.mutable.Set[Feature]): Double = {

    var value = 1.0

    for (feature <- list) {
      if (feature.isNumerical)
        value * config.numericalFeatureValues(feature)
      else if (!config.selectedBoolFeatures.contains(feature))
        return 0.0
    }
    return 1.0
  }

  def allFeaturesInConfig(config: Configuration, combi: scala.collection.mutable.Set[Feature]): Boolean = {
    var featuresFound = 0
    if (combi.size == 0)
      throw new Exception()

    combi.filter(config.selectedBoolFeatures.contains(_)).foreach(x => featuresFound += 1)
    combi.filter(config.numericalFeatureValues.keySet.contains(_)).foreach(x => featuresFound += 1)

    if (featuresFound < combi.size)
      return false

    return true
  }

  def computeErrorForCombination(combination: Array[scala.collection.mutable.Set[Feature]], configs: Array[Configuration]): Double = {

    var org: Matrix = getMatrixFromFeatureSet(combination)
    var temp = (org.transpose())
    var const: Matrix = null

    // FIXME in some cases the pinverse can not be computed 
    try {
      temp = temp.inverse()
      const = temp.times(Y2)

    } catch {
      case t : Throwable => const = org.times(Y2)
    }

    // TODO store const variable in global Field 

    return computeError(const, combination, configs)

  }

  def computeAllCombinations(combinationSet: scala.collection.mutable.Set[scala.collection.mutable.Set[Feature]], feature: Feature, configurations: Array[Configuration]): scala.collection.mutable.Set[scala.collection.mutable.Set[scala.collection.mutable.Set[Feature]]] = {

    var combinations: scala.collection.mutable.Set[scala.collection.mutable.Set[scala.collection.mutable.Set[Feature]]] =
      scala.collection.mutable.Set()

    for (i <- combinationSet) {
      var combination: scala.collection.mutable.Set[scala.collection.mutable.Set[Feature]] = scala.collection.mutable.Set()

      var subSet: scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()

      i.foreach(x => subSet.add(x))
      subSet.add(feature)
      if (atLeastOneConfigurationHasCombination(configurations, subSet))
        combination.add(subSet)

      for (k <- combinationSet) {
        combination.add(k)
      }
      if (combination.size > 0)
        combinations.add(combination)

    }

    var singleFeature: scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    //    if(atLeastOneConfigurationHasCombination(configurations, singleFeature))
    singleFeature.add(feature)

    var firstConfig: scala.collection.mutable.Set[scala.collection.mutable.Set[Feature]] = scala.collection.mutable.Set()
    combinationSet.foreach(x => firstConfig.add(x))
    //    if(singleFeature.size > 0 ){
    firstConfig.add(singleFeature)
    combinations.add(firstConfig)
    //    }
    return combinations
  }

  def atLeastOneConfigurationHasCombination(configurations: Array[Configuration], combination: scala.collection.mutable.Set[Feature]): Boolean = {

    for (config <- configurations) {
      if (allFeaturesInConfig(config, combination))
        return true

    }

    return false
  }

}