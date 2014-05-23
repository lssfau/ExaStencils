package exastencils.spl

class Configuration() {

  var selectedBoolFeatures: scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
  var numericalFeatureValues: scala.collection.mutable.Map[Feature, Int] = scala.collection.mutable.Map()

  // TODO correct NFP implementation
  var nfpValues : NonFunctionalProperties = new NonFunctionalProperties
  
  def selectedBoolFeaturesAsArray(): Array[exastencils.spl.Feature] = {
    return selectedBoolFeatures.toArray
  }

  def readSolution(solution: jp.kobe_u.copris.Solution) = {
    //    solution.boolValues.filter(_._2).map(a => selectedBoolFeatures.add(FeatureModel.allFeatures(a._1.name)))
    solution.intValues.map(a =>
      if (!(a._1.name.equals("no") || a._1.name.equals("on") || a._1.name.equals("off")))
        if (FeatureModel.allFeatures(a._1.name).isNumerical)
        numericalFeatureValues.put(FeatureModel.allFeatures(a._1.name), a._2.toInt)
      else if (a._2.toInt != 0)
        selectedBoolFeatures.add(FeatureModel.allFeatures(a._1.name)))

  }

  def readSolution(boolFeatures: scala.collection.mutable.Set[Feature]) = {
    boolFeatures.foreach(a => selectedBoolFeatures.add(a))
  }

  def readSolution(boolFeatures: scala.collection.mutable.Set[Feature], numericalFeatures: scala.collection.mutable.Map[Feature, Int]) = {
    boolFeatures.foreach(a => selectedBoolFeatures.add(a))
    numericalFeatures.foreach(a => numericalFeatureValues.put(FeatureModel.allFeatures(a._1.identifier), a._2.toInt))
  }

  def isBoolFeatureIsSelected(featureName: String): Boolean = {
    return !selectedBoolFeatures.filter(_.identifier == featureName).isEmpty
  }

  def isNumericalFeatureSelected(name: String): Boolean = {
    return !numericalFeatureValues.filter(_._1.identifier == name).isEmpty
  }

  /**
   * This method returns the list of selected sub-features of the given feature (featureName).
   *
   * The returned set may be empty if the feature is not selected, if the feature has no sub-feature ,or if non of the sub-features is selected.
   *
   */
  def getSelectedSubFeatureNames(featureName: String): scala.collection.mutable.Set[String] = {
    var subFeatureNames: scala.collection.mutable.Set[String] = scala.collection.mutable.Set()
    // test whether the desired feature is selected 
    selectedBoolFeatures.filter(_.identifier == featureName)
      // identify childs of the parent feature
      .map(a => FeatureModel.parentChildRelationships
        .filter(_._1.identifier == a.identifier)
        // go through childs
        .map(parent => parent._2
          // add selected child to return list
          .map(a => if (this.isBoolFeatureIsSelected(a.identifier)) subFeatureNames.add(a.identifier))))

    return subFeatureNames
  }

  /**
   * This method returns the selected sub-features of the given feature (featureName). This method should be used for parent features of Xor groups, because only one sub-feature can be selected at a time.
   *
   * The returned set may be empty if the feature is not selected, or if the feature has no sub-feature and is thus not a parent of a Xor group.
   *
   */
  def getFirstSelectedSubFeatureName(featureName: String): String = {
    return getSelectedSubFeatureNames(featureName).head
  }

  /**
   * This method returns the value of desired numerical feature.
   *
   */
  def getValueOfNumericalFeature(featureName: String): Int = {
    return numericalFeatureValues(FeatureModel.allFeatures(featureName))
  }

  override def hashCode(): Int = this.toString.hashCode;

  override def equals(that: Any) = {
    that match {
      case that: Configuration => (if (numberOfDifferentBooleanFeatures(that) == 0) true else false)
      case _ => false
    }
  }

  override def toString(): String = {
    var sb = new scala.collection.mutable.StringBuilder()

    selectedBoolFeatures.map(a => sb ++= a.identifier + " ")
    sb ++= " | "
    numericalFeatureValues.map(a => sb ++= a._1.identifier + ": " + a._2.toInt + " ")

    return sb.toString

  }

  def toPath(): String = {
    var sb = new scala.collection.mutable.StringBuilder()

    selectedBoolFeatures.map(a => sb ++= a.identifier + "")
    sb ++= "__"
    numericalFeatureValues.map(a => sb ++= a._1.identifier + "_" + a._2.toInt + "_")
    return sb.toString
  }

  /**
   * Returns the number of boolean features that are only selected in one of the configurations.
   *
   */
  def numberOfDifferentBooleanFeatures(otherConfiguration: Configuration): Int = {
    return (this.selectedBoolFeatures &~ otherConfiguration.selectedBoolFeatures).size + (otherConfiguration.selectedBoolFeatures &~ this.selectedBoolFeatures).size
  }
  
  
  

}