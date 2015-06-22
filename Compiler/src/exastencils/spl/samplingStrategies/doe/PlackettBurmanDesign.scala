package exastencils.spl.samplingStrategies.doe

import exastencils.spl.Feature

class PlackettBurmanDesign(numericFeaturs : scala.collection.mutable.Set[Feature]) {

  object Seed extends Enumeration {
    type Seed = Value
    val Seed9_3, Seed27_3, Seed81_3, Seed25_5, Seed125_5, Seed49_7 = Value
  }

  initSeeds()

  import Seed._

  var currSeed : Seed = Seed.Seed9_3

  var seeds : scala.collection.mutable.Map[Seed, Array[Integer]] = scala.collection.mutable.Map()

  def initSeeds() {
    this.seeds = scala.collection.mutable.Map()
    this.seeds.put(Seed9_3, Array(0, 1, 2, 2, 0, 2, 1, 1))
    this.seeds.put(Seed27_3, Array(0, 0, 1, 0, 1, 2, 1, 1, 2, 0, 1, 1, 1, 0, 0, 2, 0, 2, 1, 2, 2, 1, 0, 2, 2, 2))
    this.seeds.put(Seed81_3, Array(0, 1, 1, 1, 1, 2, 0, 1, 2, 1, 1, 2, 1, 2, 0, 2, 0, 2, 2, 1, 1, 0, 2, 0, 1, 1, 0, 0, 1, 2, 2, 2, 0, 2, 1, 0, 0, 2, 0, 0, 0, 2, 2, 2, 2, 1, 0, 2, 1, 2, 2, 1, 2, 1, 0, 1, 0, 1, 1, 2, 2, 0, 1, 0, 2, 2, 0, 0, 2, 1, 1, 1, 0, 1, 2, 0, 0, 1, 0, 0))
    this.seeds.put(Seed25_5, Array(0, 4, 1, 1, 2, 1, 0, 3, 2, 2, 4, 2, 0, 1, 4, 4, 3, 4, 0, 2, 3, 3, 1, 3))
    this.seeds.put(Seed125_5, Array(0, 2, 2, 2, 1, 0, 4, 1, 1, 4, 1, 3, 1, 3, 4, 1, 2, 0, 2, 1, 1, 0, 2, 4, 4, 3, 1, 4, 0, 2, 0, 0, 4, 4, 4, 2, 0, 3, 2, 2, 3, 2, 1, 2, 1, 3, 2, 4, 0, 4, 2, 2, 0, 4, 3, 3, 1, 2, 3, 0, 4, 0, 0, 3, 3, 3, 4, 0, 1, 4, 4, 1, 4, 2, 4, 2, 1, 4, 3, 0, 3, 4, 4, 0, 3, 1, 1, 2, 4, 1, 0, 3, 0, 0, 1, 1, 1, 3, 0, 2, 3, 3, 2, 3, 4, 3, 4, 2, 3, 1, 0, 1, 3, 3, 0, 1, 2, 2, 4, 3, 2, 0, 1, 0))
    this.seeds.put(Seed49_7, Array(0, 1, 2, 6, 2, 2, 1, 6, 0, 5, 3, 2, 3, 3, 5, 2, 0, 4, 1, 3, 1, 1, 4, 3, 0, 6, 5, 1, 5, 5, 6, 1, 0, 2, 4, 5, 4, 4, 2, 5, 0, 3, 6, 4, 6, 6, 3, 4))
  }

  def getPoints() : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature, Double]] = {
    println(currSeed)
    var points : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature, Double]] = scala.collection.mutable.Set()

    var valuesOfFeatures : scala.collection.mutable.Map[Feature, Array[Double]] = scala.collection.mutable.Map()
    var levelPerFeature = getLevelOfCurrSeed();

    numericFeaturs.foreach { x => valuesOfFeatures.put(x, x.getSampledValue(levelPerFeature)) }

    // combine all values
    var matrix = this.computeDesignMatrix()
    for (i <- 0 to matrix.length - 1) {
      var point : scala.collection.mutable.Map[Feature, Double] = scala.collection.mutable.Map()
      var j = 0
      numericFeaturs.foreach { x => { point.put(x, valuesOfFeatures(x)(matrix(i)(j))); j += 1 } }
      points.add(point)

    }
    return points
  }

  def computeDesignMatrix() : Array[Array[Int]] = {
    var numberOfFeatures = numericFeaturs.size

    var seed = this.seeds(currSeed)

    var matrix : Array[Array[Int]] = Array.ofDim[Int](seed.length + 1, numberOfFeatures)
    var offset = 0

    for (i <- 0 to matrix(0).length - 1) {
      for (j <- 0 to matrix.length - 1) {
        if (j == matrix.length - 1) {
          matrix(j)(i) = 0
        } else {
          matrix(j)(i) = seed((j + offset) % seed.length)
        }
      }
      offset += 1
    }
    return matrix;
  }

  def setSeed(level : Integer, measurements : Integer) {
    initSeeds()
    if (measurements == 9 && level == 3)
      this.currSeed = Seed9_3
    if (measurements == 27 && level == 3)
      this.currSeed = Seed27_3
    if (measurements == 81 && level == 3)
      this.currSeed = Seed81_3
    if (measurements == 25 && level == 5)
      this.currSeed = Seed25_5
    if (measurements == 125 && level == 5)
      this.currSeed = Seed125_5
    if (measurements == 49 && level == 7)
      this.currSeed = Seed49_7
  }

  def getLevelOfCurrSeed() : Integer = {
    var num = currSeed match {
      case Seed9_3   => 3
      case Seed27_3  => 3
      case Seed81_3  => 3
      case Seed25_5  => 5
      case Seed125_5 => 5
      case Seed49_7  => 7
    }
    return num;
  }

}