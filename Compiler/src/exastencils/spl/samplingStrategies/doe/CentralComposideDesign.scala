package exastencils.spl.samplingStrategies.doe

import exastencils.spl.Feature

/**
  * This class represents the central composite design. It is a experimental design used to sample numeric configurations options in order to
  * learn a surface model describing the influence of the options on an dependent variable such as performance.
  *
  * @param The set of numeric configuration options considered in the sampling.
  *
  */
class CentralCompositeDesign(numericFeaturs : scala.collection.mutable.Set[Feature]) {

  def getTwoLevelFactorial(numericFeaturs : scala.collection.mutable.Set[Feature]) : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature, Double]] = {
    var points : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature, Double]] = scala.collection.mutable.Set()

    // prepare array 
    var twoLevelFac = Array.ofDim[Integer](Math.pow(2, numericFeaturs.size).toInt, numericFeaturs.size)
    var signum = 1
    var counter = 0
    for (x <- 0 to twoLevelFac(0).length - 1) {
      for (y <- 0 to twoLevelFac.length - 1) {
        counter += 1

        twoLevelFac(y)(x) = -1 * signum
        if (counter % Math.pow(2, x).toInt == 0)
          signum *= -1
      }
      counter = 0
      signum = 1
    }

    // generate configs based on the values of the array
    for (i <- 0 to twoLevelFac.length - 1) {
      var currPoint : scala.collection.mutable.Map[Feature, Double] = scala.collection.mutable.Map()
      var j = 0
      numericFeaturs.foreach(x => {
        if (twoLevelFac(i)(j) == -1)
          currPoint.put(x, x.getMinValue)
        else
          currPoint.put(x, x.getMaxValue)
        j += 1
      })
      points.add(currPoint)
    }

    return points
  }

  def getPoints() : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature, Double]] = {

    var resultPoints : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature, Double]] = scala.collection.mutable.Set()
    println("in CCD ")
    numericFeaturs.foreach { x => println(x.identifier) }
    println("-------")

    // center point -----------------------------------------------------------    1
    var centerPoint : scala.collection.mutable.Map[Feature, Double] = scala.collection.mutable.Map()
    numericFeaturs.foreach(x => centerPoint.put(x, x.getCenterValue()))
    resultPoints.add(centerPoint)

    //    // axial points -----------------------------------------------------------    2*k
    numericFeaturs.foreach(x => {
      var maxPoint : scala.collection.mutable.Map[Feature, Double] = scala.collection.mutable.Map()
      var minPoint : scala.collection.mutable.Map[Feature, Double] = scala.collection.mutable.Map()
      maxPoint.put(x, x.getMaxValue())
      minPoint.put(x, x.getMinValue())

      numericFeaturs.foreach(y => {
        if (!y.equals(x)) {
          minPoint.put(y, y.getCenterValue)
          maxPoint.put(y, y.getCenterValue)
        }
      })
      resultPoints.add(minPoint)
      resultPoints.add(maxPoint)
    })

    // cube points  -----------------------------------------------------------    2^k
    var fullFactorialPoints : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature, Double]] = getTwoLevelFactorial(numericFeaturs)
    var rootN = Math.sqrt(numericFeaturs.size);
    var lowerCubeValue : scala.collection.mutable.Map[Feature, Double] = scala.collection.mutable.Map()
    var upperCubeValue : scala.collection.mutable.Map[Feature, Double] = scala.collection.mutable.Map()

    numericFeaturs.foreach(x => {
      var b_i = x.getCenterValue - x.getMinValue
      var a_i = b_i / rootN

      var lower = x.getCenterValue - a_i
      var upper = x.getCenterValue + a_i

      lowerCubeValue.put(x, x.nearestAllowedValue(lower))
      upperCubeValue.put(x, x.nearestAllowedValue(upper))
    })
    // create cube points
    var newPoints = Math.pow(2, numericFeaturs.size).toInt - 1
    for (i <- 0 to newPoints) {
      var cubePoint : scala.collection.mutable.Map[Feature, Double] = scala.collection.mutable.Map()

      var it = (i.toBinaryString)
      while (it.length() < numericFeaturs.size) {
        it = "0" + it;
      }
      var pos = 0

      numericFeaturs.foreach(y => {
        if (it(pos).equals('0')) {
          cubePoint.put(y, lowerCubeValue(y))
        } else {
          cubePoint.put(y, upperCubeValue(y))
        }
        pos += 1
      })

      resultPoints.add(cubePoint)

    }

    return resultPoints;
  }

}