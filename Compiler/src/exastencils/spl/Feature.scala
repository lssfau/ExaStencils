package exastencils.spl

import scala.collection.Set

class Feature(name : String) {
  var identifier = name
  var isOptional = true
  var isNumerical = false
  var isChild = false

//  var isSelectedInAllConfigs = false
  
  var isParentOfXor = false
  
  var hasValuesRange = false

  var minValue = 0.0
  var maxValue = 0.0
  var defaultValue : Any = null
  var stepsize = 0.0
  
  var values : Array[String] = Array()

  var nfpValue = 0.0

  override def toString() : String = {

    var sb = new scala.collection.mutable.StringBuilder()

    sb ++= "Feature: " + identifier + "  " //+ super.toString   
    sb ++= " isOptional " + isOptional + "\n" 
//    + selected in all configs "+ isSelectedInAllConfigs + "\n"
    sb ++= " isParentOfXor " + isParentOfXor
    if (FeatureModel.parentChildRelationships.contains(this)) {
      sb ++= " Childs: "
      FeatureModel.parentChildRelationships(this).map(sb ++= " " + _.identifier)
      sb ++= "\n"
    }
    sb ++= " isNumerical " + isNumerical + "\n"
    if (isNumerical)
      sb ++= " minValue " + minValue + " maxValue " + maxValue + " stepsize " + stepsize + " defaultValue " + defaultValue + "\n"
      
    return sb.toString();

  }

  def updateNumericalValues(content : String) = {
    var cont = content
    this.isNumerical = true
    cont = content.replaceAll("[\\}\\{]", "")
    val contArr = cont.split(",")
    this.minValue = augmentString(contArr(0).trim()).toInt
    this.maxValue = augmentString(contArr(1).trim()).toInt
    // FIXME this.stepsize = augmentString(contArr(2).trim()).toInt
    this.defaultValue = augmentString(contArr(3).trim())
  }

  def valuesAsIntSet() : Set[Int] = {
    var newSet : Set[Int] = Set()
    for(i <- this.values ){
      newSet+=augmentString(i).toInt      
    }
    return newSet
  }
  
  
  override def equals(other : Any) = other match {
    case that : Feature => this.identifier == that.identifier
    case _              => false
  }

  override def hashCode() : Int = this.identifier.hashCode();

}