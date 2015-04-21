package exastencils.spl.samplingStrategies.doe

import exastencils.spl.Feature
import scala.util.Random

class RandomDesign(numericFeaturs : scala.collection.mutable.Set[Feature]) {

  var seed : Int = 0
  var numberOfPoints : Int = 20

  def getPoints() : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature, Double]] = {
    var resultPoints : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature, Double]] = scala.collection.mutable.Set()
    var rand : Random = new Random(seed)

    for (i <- 0 to numberOfPoints) {
      var onePoint : scala.collection.mutable.Map[Feature, Double] = scala.collection.mutable.Map()
      numericFeaturs.foreach(x => onePoint.put(x, x.getRandomValue(rand.nextInt())))
      resultPoints.add(onePoint)
    }
    return resultPoints;
  }

}