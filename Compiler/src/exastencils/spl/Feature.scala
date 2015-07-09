package exastencils.spl

import scala.collection.Set
import scala.util.Sorting
import scala.util.Random
import exastencils.spl.learning.FFS_Expression

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

  var valueCalculation : String = "n +1"

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

    var allValues = getAllValues().toArray

    return allValues(allValues.size / 2)
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
      var values : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()
      var domain : scala.collection.mutable.Map[String, exastencils.spl.Feature] = scala.collection.mutable.Map()
      domain.put(this.name, this)
      var exp : FFS_Expression = new FFS_Expression(domain, valueCalculation)
      while (value > curr) {
        lowerValue = curr
        var map : scala.collection.mutable.Map[Feature, Double] = scala.collection.mutable.Map()
        map.put(this, curr)
        curr = exp.evaluationOfRPN(map)
      }
      upperValue = curr
    }
    if (math.abs(lowerValue - value) < math.abs(upperValue - value))
      return lowerValue
    else
      return upperValue
  }

  def applicableInCCI() : Boolean = {

    if (!this.isNumerical)
      return false

    if (!this.hasNValues(5))
      return false

    if (!this.hasValuesRange)
      return false

    return true
  }

  def hasNValues(n : Int) : Boolean = {
    var domain : scala.collection.mutable.Map[String, exastencils.spl.Feature] = scala.collection.mutable.Map()
    domain.put(this.name, this)
    var exp : FFS_Expression = new FFS_Expression(domain, valueCalculation)
    var validValues = 1
    var curr = minValue

    while (curr <= maxValue && validValues < n) {
      var map : scala.collection.mutable.Map[Feature, Double] = scala.collection.mutable.Map()
      map.put(this, curr)
      curr = exp.evaluationOfRPN(map)
      if (curr <= maxValue)
        validValues += 1
    }
    if (validValues < n)
      return false

    return true
  }

  def getAllValuesAsString() : scala.collection.mutable.Set[String] = {
    var values : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()
    var domain : scala.collection.mutable.Map[String, exastencils.spl.Feature] = scala.collection.mutable.Map()
    domain.put(this.name, this)
    var exp : FFS_Expression = new FFS_Expression(domain, valueCalculation)
    var curr = minValue

    while (curr <= maxValue) {
      var map : scala.collection.mutable.Map[Feature, Double] = scala.collection.mutable.Map()
      map.put(this, curr)
      values.add(curr + "")
      curr = exp.evaluationOfRPN(map)

    }

    return values
  }

  def getAllValues() : scala.collection.mutable.Set[Double] = {
    var values : scala.collection.mutable.SortedSet[Double] = scala.collection.mutable.SortedSet()
    var domain : scala.collection.mutable.Map[String, exastencils.spl.Feature] = scala.collection.mutable.Map()
    domain.put(this.name, this)
    var exp : FFS_Expression = new FFS_Expression(domain, valueCalculation)
    var curr = minValue

    while (curr <= maxValue) {
      var map : scala.collection.mutable.Map[Feature, Double] = scala.collection.mutable.Map()
      map.put(this, curr)
      values.add(curr)
      curr = exp.evaluationOfRPN(map)

    }

    return values
  }

  def getSampledValue(distinctValues : Int) : Array[Double] = {

    if (this.name.startsWith("poly_tileSize_"))
      return getPowOf2ValuesPolly(distinctValues)

    return getValues(distinctValues)

  }

  def getRandomValue(seed : Int) : Double = {

    if (this.name.equals("opt_unroll"))
      print("")

    if (this.name.startsWith("domain_")) {
      return getRandomPowOf2Value(seed)
    }

    if (this.name.equals("sisc2015_ranksPerNode")) {
      return getRandomPowOf2Value(seed)
    }

    if (this.name.equals("sisc2015_numNodes")) {
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
    var differentValues : Int = 2

    var valuesLocal : scala.collection.mutable.Set[Double] = scala.collection.mutable.Set()

    var curr = minValue
    while (curr <= maxValue) {
      valuesLocal.add(curr)
      curr = curr * 2;
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
    var allValues = getAllValues().toArray
    return allValues(rand.nextInt(allValues.length))
  }

  def getEqualDistributedValues(distinctValues : Int) : Array[Double] = {
    var valuesLocal : Array[Double] = Array.ofDim[Double](distinctValues)

    var allValues = getAllValues().toArray

    valuesLocal(0) = allValues(0)
    valuesLocal(distinctValues - 1) = allValues(allValues.length - 1)

    // -2 because we already considered the min and maximum value, +1 we start with 0
    var stepsBetweenValues = numberOfValues / (distinctValues - 2 + 1)

    for (a <- 1 to distinctValues - 2) {
      valuesLocal(a) = allValues(stepsBetweenValues * a)
    }
    return valuesLocal;
  }

  def getValues(distinctValues : Int) : Array[Double] = {
    var valuesLocal : Array[Double] = Array.ofDim[Double](distinctValues)

    var allValues = getAllValues().toArray

    valuesLocal(0) = allValues(0)
    valuesLocal(distinctValues - 1) = allValues(allValues.length - 1)

    // -2 because we already considered the min and maximum value, +1 we start with 0
    var stepsBetweenValues = numberOfValues / (distinctValues - 2 + 1)

    for (a <- 1 to distinctValues - 2) {
      valuesLocal(a) = allValues(stepsBetweenValues * a)
    }
    return valuesLocal;
  }

  // TODO   
  def getPowOf2Values(distinctValues : Int) : Array[Double] = {
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
    return this.getAllValuesAsString().size
  }

  override def hashCode() : Int = this.identifier.hashCode();

}