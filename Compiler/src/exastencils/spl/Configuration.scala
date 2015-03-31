package exastencils.spl

import sun.reflect.generics.reflectiveObjects.NotImplementedException

class Configuration() {

  var boolFeatures : scala.collection.mutable.Map[Feature,Boolean] = scala.collection.mutable.Map()
  var numericalFeatureValues : scala.collection.mutable.Map[Feature, Double] = scala.collection.mutable.Map()
  var xorFeatureValues : scala.collection.mutable.Map[Feature, String] = scala.collection.mutable.Map()
  
  var additionalKnowledgeFileInformation : String = "\n"
  var partialBaseConfig : scala.collection.mutable.Map[String,Any] = scala.collection.mutable.Map()
  
  var nfpValues : scala.collection.mutable.Map[NonFunctionalProperties.Value, Double] = scala.collection.mutable.Map()
  
  var identifier = ""
  
  def selectedBoolFeaturesAsArray(): Array[Tuple2[exastencils.spl.Feature,Boolean]] = {
    var bool : Array[Tuple2[exastencils.spl.Feature,Boolean]] = new Array[Tuple2[exastencils.spl.Feature,Boolean]](boolFeatures.size)
    var index = 0
    boolFeatures.foreach(f => {
      bool(index) = new Tuple2(f._1,f._2)
    })
    return bool
  }

  def readSolution(solution: jp.kobe_u.copris.Solution) = {
    //    solution.boolValues.filter(_._2).map(a => selectedBoolFeatures.add(FeatureModel.allFeatures(a._1.name)))
    solution.intValues.map(a =>
      if (!(a._1.name.equals("no") || a._1.name.equals("on") || a._1.name.equals("off")))
        if (FeatureModel.allFeatures(a._1.name).isNumerical)
        numericalFeatureValues.put(FeatureModel.allFeatures(a._1.name), a._2.toInt)
      else if (a._2.toInt != 0)
        boolFeatures.put(FeatureModel.allFeatures(a._1.name),true)) // TODO consider false as default value
    solution.boolValues.map{a => 
       if(a._2 == true){
         if(a._1.name .contains("__")){ // part of an XOR group (virtual feature)
           var parts = a._1.name.split("__")
           
           xorFeatureValues.put(FeatureModel.allFeatures (parts(0)), parts(1))
         }else{
           boolFeatures .put(FeatureModel.allFeatures (a._1.name),true) // TODO consider false as default value
         }
       }
      
    }    
    
    identifier = this.toString()
  }

  def readSolution(boolSampling: scala.collection.mutable.Map[Feature,String], numericSampling: scala. collection.mutable.Map[Feature, Double]) = {
    numericalFeatureValues = numericSampling
    boolSampling.foreach(x => if(x._1.isXorFeature) {
        this.xorFeatureValues.put(x._1, x._2)
      }else{
        if(x._2.equals("true"))
          this.boolFeatures.put(x._1,true)
        else
          this.boolFeatures.put(x._1,false)
      })
     identifier = this.toString()
  }
  
  // TODO consider false as value
  def readSolution(bool: scala.collection.mutable.Set[Feature]) = {
    bool.foreach(a => boolFeatures.put(a,true))
    identifier = this.toString()
  }

  // TODO condier false as value
  def readSolution(bool: scala.collection.mutable.Set[Feature], numericalFeatures: scala.collection.mutable.Map[Feature, Double], xorFeatures: scala.collection.mutable.Map[Feature, String]) = {
    bool.foreach(a => boolFeatures.put(a,true))
    numericalFeatures.foreach(a => numericalFeatureValues.put(FeatureModel.allFeatures(a._1.identifier), a._2.toDouble))
    xorFeatures.foreach(a => xorFeatureValues.put(FeatureModel.allFeatures(a._1.identifier), a._2))
    identifier = this.toString()
  }

  def readSolution(numericalFeatures: scala.collection.mutable.Map[Feature, Double]) = {
    numericalFeatures.foreach(a => numericalFeatureValues.put(FeatureModel.allFeatures(a._1.identifier), a._2.toDouble))
    identifier = this.toString()
  }
  
  def isBoolFeatureIsSelected(featureName: String): Boolean = {
    return !boolFeatures.filter(_._1.identifier == featureName).isEmpty
  }

  def isNumericalFeatureSelected(name: String): Boolean = {
    return !numericalFeatureValues.filter(_._1.identifier == name).isEmpty
  }

  def getNumericFeatureValue(name: String) : Double = {
    return numericalFeatureValues.filter(_._1.identifier == name).head._2
  }
  
  def containtsFeature(name: String) : Boolean = {
    return numericalFeatureValues.contains(FeatureModel.getSpecificFeatureDkPaperTest(name))
    
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
    boolFeatures.filter(f => f._1.identifier == featureName)
      // identify childs of the parent feature
      .map(a => FeatureModel.parentChildRelationships
        .filter(_._1.identifier == a._1.identifier)
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
  def getValueOfNumericalFeature(featureName: String): Double = {
    return numericalFeatureValues(FeatureModel.allFeatures(featureName))
  }

  override def hashCode(): Int = identifier.hashCode;

  // TODO 
  override def equals(that: Any) = {
//    that match {
//      case that: Configuration => (if (numberOfDifferentBooleanFeatures(that) == 0 && !hasDifferentNumbericalFeatures(that)) true else false)
//      case _ => false
//    }
    false
  }

  override def toString(): String = {
    var sb = new scala.collection.mutable.StringBuilder()
    sb ++= "BinaryFeatures: "
    boolFeatures.map(a => sb ++= a._1.identifier +":"+a._2+", ")
    sb ++= " \nNumericFeatures: "
    numericalFeatureValues.map(a => sb ++= a._1.identifier + ": " + a._2.toInt + ", ")
    sb ++= " \nXOR-Features: "
    xorFeatureValues.map(a => sb ++= a._1.identifier + ": " + a._2 + ", ")
    sb ++= " \nPartialBase: "
    partialBaseConfig.foreach(x => sb++= x._1 + " : "+x._2+ ", ")
    
    return sb.toString

  }

  def toPath(): String = {
    var sb = new scala.collection.mutable.StringBuilder()

    boolFeatures.map(a => sb ++= a._1.identifier + ""+a._2+"")
    sb ++= "__"
    numericalFeatureValues.map(a => sb ++= a._1.identifier + "_" + a._2.toInt + "_")
    return sb.toString
  }

  /**
   * Returns the number of boolean features that are only selected in one of the configurations.
   *
   */
  def numberOfDifferentBooleanFeatures(otherConfiguration: Configuration): Int = {
    return throw new NotImplementedException()//(this.boolFeatures &~ otherConfiguration.boolFeatures).size + (otherConfiguration.boolFeatures &~ this.boolFeatures).size
  }
  
   def hasDifferentNumbericalFeatures(otherConfiguration: Configuration): Boolean = {
     var different = false
     
     this.numericalFeatureValues.foreach(x => {
       if(!otherConfiguration.numericalFeatureValues .contains(x._1))
         different = true
       if(!otherConfiguration.numericalFeatureValues(x._1).equals(x._2))
         different = true
      })
     
      otherConfiguration.numericalFeatureValues.foreach(x => {
       if(!this.numericalFeatureValues .contains(x._1))
         different = true
       if(!this.numericalFeatureValues(x._1).equals(x._2))
         different = true
      })
     
     return different;
  }
  
  def CSV_String() : String = {
    var sb : StringBuilder = new StringBuilder()
    
    this.boolFeatures .foreach(x => sb.append(x._1.identifier +x._2+""+";"))
    this.numericalFeatureValues .foreach(x => sb.append(x._1.identifier+":"+x._2+";" ))
    return sb.toString;
  }
  
  
  def addNumericOptions(additionalNumericOptions : scala. collection.mutable.Map[Feature, Double]){
    
    additionalNumericOptions.foreach(x => {
      if(this.numericalFeatureValues.contains(x._1))
        println("Feature already selected in this configuration")
      else
        this.numericalFeatureValues.put(x._1, x._2)  
    })
    identifier = this.toString()
  }
  
  def generateConfigurations(binarySampling: scala.collection.mutable.Set[scala.collection.mutable.Map[Feature,String]], 
      numericSampling: scala.collection.mutable.Set[scala. collection.mutable.Map[Feature, Double]]) : scala.collection.mutable.Set[Configuration] = {
    
    var configs : scala.collection.mutable.Set[Configuration] = scala.collection.mutable.Set()
     
    binarySampling.foreach(x => {
      numericSampling.foreach(y => {
        var conf = new Configuration()
        conf.readSolution(x, y)
        configs.add(conf)
      })
    })
    return configs
  }
  
  def copy() : Configuration = {
    var newConfig = new Configuration()
    boolFeatures.foreach( {x => newConfig.boolFeatures.put(x._1,x._2)})
    numericalFeatureValues.foreach(x => newConfig.numericalFeatureValues.put(x._1, x._2))
    xorFeatureValues.foreach(x => newConfig.xorFeatureValues.put(x._1, x._2))
    newConfig.additionalKnowledgeFileInformation = additionalKnowledgeFileInformation // todo validate whether this works correctly
    partialBaseConfig.foreach(x => newConfig.partialBaseConfig.put(x._1, x._2))
    
    return newConfig
  }

  def getKnowledgeFileContent() : scala.collection.mutable.Set[String] = {
    var content : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()
    boolFeatures.foreach ( x => {
      if(x._2)
        content.add(x._1 + "= true\n")
      else
        content.add(x._1 + "= false\n")
    })
    numericalFeatureValues.foreach(x => {
      if(x._1.dataType.equals("Int")){
        content.add(x._1 + "= "+x._2.toInt+"\n")
      }else{
        content.add(x._1 + "= "+x._2+"\n")
      }
    })
    content.add(additionalKnowledgeFileInformation)    
    partialBaseConfig.foreach(x => {
      content.add(x._1+"= "+x._2+"\n")
    })
    xorFeatureValues.foreach(x => {
      content.add(x._1+"= "+x._2+"\n")
    })
    return content
  }
  
}