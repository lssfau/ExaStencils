package exastencils.spl.samplingStrategies.heuristics

import exastencils.spl._

class FWHeuristic(binaryFeaturs : scala.collection.mutable.Set[Feature]) {

  // binary features being considered by the sampling
  var binaryFeaturesToConsider : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()

  // the minimal configuration
  var minimalConfig : scala.collection.mutable.Map[Feature, Any] = scala.collection.mutable.Map()

  // remove numeric features from the set to consider
  binaryFeaturs.foreach { x => binaryFeaturesToConsider.add(x) }

  def getPoints() : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature, String]] = {
    var featureWiseConfigurations : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature, String]] = scala.collection.mutable.Set()

    var basicValues : scala.collection.mutable.Map[Feature, String] = scala.collection.mutable.Map()

    binaryFeaturesToConsider.foreach { x => basicValues.put(x, x.defaultValue) }

    featureWiseConfigurations.add(basicValues)

    binaryFeaturesToConsider.foreach { x =>
      if (x.isXorFeature) {
        x.values.foreach {
          y =>
            if (x.dataType.equals("Int")) {
              var config : scala.collection.mutable.Map[Feature, String] = scala.collection.mutable.Map()
              var default = x.defaultValue.toInt.toString()
              if (!default.equals(y.toDouble.toInt.toString())) {
                config.put(x, y.toDouble.toInt.toString())
                addDefaultValuesOfOther(config, basicValues)
                featureWiseConfigurations.add(config)
              }
            } else if (!x.defaultValue.equals(y)) {
              var config : scala.collection.mutable.Map[Feature, String] = scala.collection.mutable.Map()
              config.put(x, y)
              addDefaultValuesOfOther(config, basicValues)
              featureWiseConfigurations.add(config)
            }
        }
      } else {
        var config : scala.collection.mutable.Map[Feature, String] = scala.collection.mutable.Map()
        if (x.defaultValue == "false") {
          config.put(x, "true")
        } else {
          config.put(x, "false")
        }
        if (x.identifier.equals("l3tmp_useSlotsForJac"))
          println()
        addDefaultValuesOfOther(config, basicValues)
        featureWiseConfigurations.add(config)
      }
    }
    return featureWiseConfigurations
  }

  def addDefaultValuesOfOther(config : scala.collection.mutable.Map[Feature, String], defaultValues : scala.collection.mutable.Map[Feature, String]) = {
    defaultValues.foreach(x =>
      if (!config.contains(x._1)) {
        config.put(x._1, x._2)
      })
      //print()
      None
  }

}