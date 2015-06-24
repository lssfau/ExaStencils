package exastencils.spl

import scala.collection.Set
import scala.util.Sorting
import scala.util.Random

class Feature(name : String) {
  var identifier = name
  var isOptional = true
  var isNumerical = false
  var isChild = false

  //  var isSelectedInAllConfigs = false

  // TODO for 1 == 1.0 casting in Sampling
  var dataType : String = ""

  var isXorFeature = false

  var hasValuesRange = false

  var minValue : Double = 0.0
  var maxValue : Double = 0.0
  var defaultValue : String = null
  var stepsize : Double = 1.0

  var values : Array[String] = Array()

  var nfpValue = 0.0

  override def toString() : String = {

    var sb = new scala.collection.mutable.StringBuilder()

    sb ++= identifier + ""

    //    sb ++= "Feature: " + identifier  +" "
    //    sb ++= "defaultValue: " + defaultValue + " "
    //    sb ++= "  isOptional: " + isOptional + " "
    //    //    + selected in all configs "+ isSelectedInAllConfigs + "\n"
    //    sb ++= " isXorFeature: " + isXorFeature
    //    if (FeatureModel.parentChildRelationships.contains(this)) {
    //      sb ++= " Childs: "
    //      FeatureModel.parentChildRelationships(this).map(sb ++= " " + _.identifier)
    //      sb ++= " "
    //    }
    //    sb ++= " values:"
    //    this.values .foreach(x => sb ++= x+" ")
    //    sb ++= " isNumerical " + isNumerical + " "
    //    if (isNumerical)
    //      sb ++= " minValue= " + minValue + " maxValue= " + maxValue + " stepsize= " + stepsize 

    return sb.toString()

  }

  def updateNumericalValues(content : String) = {
    var cont = content
    this.isNumerical = true
    cont = content.replaceAll("[\\}\\{]", "")
    if (cont.containsSlice("|")) {
      values = cont.split("\\|")
    } else {
      val contArr = cont.split(",")
      this.minValue = augmentString(contArr(0).trim()).toInt
      this.maxValue = augmentString(contArr(1).trim()).toInt
      // FIXME this.stepsize = augmentString(contArr(2).trim()).toInt
      this.defaultValue = augmentString(contArr(3).trim())
    }
  }

  def valuesAsIntSet() : Set[Int] = {
    var newSet : Set[Int] = Set()
    for (i <- this.values) {
      newSet += augmentString(i).toInt
    }
    return newSet
  }

  override def equals(other : Any) = other match {
    case that : Feature => this.identifier == that.identifier
    case _              => false
  }

  /**
    *
    * The method returns the median value of the values of the numerical feature
    *
    * @return the median value
    *
    */
  def getCenterValue() : Double = {
    if (values.size > 0) {
      var valuesAsDouble = new Array[Double](values.size)
      var i = 0
      values.foreach { x => valuesAsDouble(i) = x.toDouble; i += 1 }
      Sorting.quickSort(valuesAsDouble.asInstanceOf[Array[Double]])
      return valuesAsDouble((valuesAsDouble.length / 2).asInstanceOf[Int]).asInstanceOf[Double]
    }

    var numberOfValues = maxValue - minValue / stepsize
    return minValue + stepsize * ((numberOfValues / 2).toDouble)
  }

  def getMinValue() : Double = {
    if (values.size > 0) {
      var valuesAsDouble = new Array[Double](values.size)
      var i = 0
      values.foreach { x => valuesAsDouble(i) = x.toDouble; i += 1 }
      Sorting.quickSort(valuesAsDouble.asInstanceOf[Array[Double]])
      return valuesAsDouble(0).asInstanceOf[Double]
    }
    return minValue
  }

  def getMaxValue() : Double = {
    if (values.size > 0) {
      var valuesAsDouble = new Array[Double](values.size)
      var i = 0
      values.foreach { x => valuesAsDouble(i) = x.toDouble; i += 1 }
      Sorting.quickSort(valuesAsDouble.asInstanceOf[Array[Double]])
      return valuesAsDouble(valuesAsDouble.length - 1).asInstanceOf[Double]
    }
    return maxValue
  }

  def nearestAllowedValue(value : Double) : Double = {
    Sorting.quickSort(values.map(_.toDouble))
    var lowerValue = 0.0
    var upperValue = 0.0

    var curr = this.minValue
    if (values.size > 0) {
      var i = 1
      while (value < values(i).toDouble) {
        lowerValue = values(i).toDouble
        i += 1
      }
      upperValue = values(i).toDouble

    } else {
      while (value > curr) {
        lowerValue = curr
        curr += stepsize
      }
      upperValue = curr
    }
    if (math.abs(lowerValue - value) < math.abs(upperValue - value))
      return lowerValue
    else
      return upperValue
  }

  def applicableInCCI() : Boolean = {

    if (this.identifier.equals("l3tmp_omega"))
      println(this.identifier)

    if (!this.isNumerical)
      return false

    if (!this.has5Values)
      return false

    if (!this.hasValuesRange)
      return false

    return true
  }

  def has5Values() : Boolean = {
    if (minValue + 4 * stepsize <= maxValue) {
      return true
    }
    return false
  }

  def getSampledValue(distinctValues : Int) : Array[Double] = {

    if (this.name.startsWith("domain_")) {
      return getPowOf2Values(distinctValues)
    }

    if (this.name.equals("sisc2015_ranksPerNode")) {
      return getPowOf2Values(distinctValues)
    }

    if (this.name.equals("sisc2015_numNodes")) {
      return getPowOf2Values(distinctValues)
    }

    if (this.name.startsWith("aro_")) {
      return getPowOf2Values(distinctValues)
    }

    if (this.name.startsWith("sisc2015_numOMP_")) {
      return getPowOf2Values(distinctValues)
    }

    if (this.name.startsWith("poly_tileSize_"))
      return getPowOf2ValuesPolly(distinctValues)

    return getEqualDistributedValues(distinctValues)

  }

  def getRandomValue(seed : Int) : Double = {
    if (this.name.startsWith("domain_")) {
      return getRandomPowOf2Value(seed)
    }

    if (this.name.equals("sisc2015_ranksPerNode")) {
      return getRandomPowOf2Value(seed)
    }

    if (this.name.equals("numNodes")) {
      return getRandomPowOf2Value(seed)
    }

    if (this.name.startsWith("aro_")) {
      return getRandomPowOf2Value(seed)
    }

    if (this.name.startsWith("sisc2015_numOMP_")) {
      return getRandomPowOf2Value(seed)
    }

    if (this.name.startsWith("poly_tileSize_"))
      return getRandomPowOf2ValuePolly(seed)

    return getRandomEqualDistributedValue(seed)
  }

  def getRandomPowOf2Value(seed : Int) : Double = {
    var rand : Random = new Random(seed)
    var differentValues : Int = 7

    var valuesLocal : scala.collection.mutable.Set[Double] = scala.collection.mutable.Set()

    valuesLocal.add(minValue)
    valuesLocal.add(maxValue)

    var curr = minValue
    for (a <- 1 to differentValues - 1) {
      var curr = (Math.pow(2, (a - 1)))
      if (curr <= maxValue) {
        valuesLocal.add(curr)
      }
    }

    var localArray = valuesLocal.toArray[Double]
    return localArray(rand.nextInt(localArray.size))
  }

  def getRandomPowOf2ValuePolly(seed : Int) : Double = {
    var rand : Random = new Random(seed)
    var differentValues : Int = 7
    var valuesLocal : Array[Double] = Array.ofDim[Double](7)

    valuesLocal(0) = minValue

    var curr = minValue
    for (a <- 1 to differentValues - 1) {
      curr = curr + (Math.pow(2, (a + 3)))
      if (curr <= maxValue) {
        valuesLocal(a) = curr
      }
    }
    return valuesLocal(rand.nextInt(valuesLocal.size))
  }

  def getRandomEqualDistributedValue(seed : Int) : Double = {
    var rand : Random = new Random(seed)
    var differentValues : Int = (((maxValue - minValue) / stepsize) + 1).toInt // we have to influce 
    return rand.nextInt(differentValues).toDouble
  }

  def getEqualDistributedValues(distinctValues : Int) : Array[Double] = {
    var valuesLocal : Array[Double] = Array.ofDim[Double](distinctValues)

    valuesLocal(0) = (minValue)
    valuesLocal(distinctValues - 1) = (maxValue)

    // -2 because we already considered the min and maximum value, +1 we start with 0
    var stepsBetweenValues = numberOfValues / (distinctValues - 2 + 1)

    for (a <- 1 to distinctValues - 2) {
      valuesLocal(a) = (minValue + (stepsize * stepsBetweenValues * a))
    }
    return valuesLocal;
  }

  // TODO   
  def getPowOf2Values(distinctValues : Int) : Array[Double] = {
    //    println(distinctValues)
    var valuesLocal : scala.collection.mutable.Set[Double] = scala.collection.mutable.Set()

    valuesLocal.add(minValue)
    valuesLocal.add(maxValue)
    val defToDouble = augmentString(defaultValue).toDouble

    if (defToDouble != minValue && defToDouble != maxValue)
      valuesLocal.add(defToDouble)

    var curr = minValue
    var break = false
    while (!break) {
      curr = curr * 2
      if (curr < maxValue) {
        valuesLocal.add(curr)

      }
      if (curr > maxValue || valuesLocal.size == distinctValues) {
        {
          break = true
        }

      }
    }
    //    while (valuesLocal.size < distinctValues) {
    //      var r = new scala.util.Random(valuesLocal.size)
    //      curr = r.nextInt(maxValue.toInt).toDouble
    //      while (valuesLocal.contains(curr) || curr < minValue) {
    //        curr = r.nextInt(maxValue.toInt).toDouble
    //      }
    //      valuesLocal.add(curr)
    //    }
    return valuesLocal.toArray[Double];
  }

  // TODO 
  def getPowOf2ValuesPolly(distinctValues : Int) : Array[Double] = {
    var valuesLocal : scala.collection.mutable.Set[Double] = scala.collection.mutable.Set()

    valuesLocal.add(minValue)

    var curr = minValue
    for (a <- 1 to distinctValues - 1) {
      curr = curr + (Math.pow(2, (a + 3)))
      if (curr <= maxValue) {
        valuesLocal.add(curr)
      }
    }

    return valuesLocal.toArray[Double];
  }

  def numberOfValues() : Int = {
    return ((maxValue - minValue) / stepsize).toInt
  }

  override def hashCode() : Int = this.identifier.hashCode();

}