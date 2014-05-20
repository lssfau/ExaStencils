package exastencils.spl
import scala.io.Source
import jp.kobe_u.copris._
import jp.kobe_u.copris.dsl._
import java.io.FileNotFoundException
import java.io.IOException
import scala.Array.canBuildFrom
import jp.kobe_u.copris.sugar.Sat4j
import java.io.File
import jp.kobe_u.copris.Term

object FeatureModel {

  val parentChildRelationships: scala.collection.mutable.Map[Feature, scala.collection.mutable.Set[Feature]] = scala.collection.mutable.Map()
  val allFeatures: scala.collection.mutable.Map[String, Feature] = scala.collection.mutable.Map()

  val featureModelConstraints: scala.collection.mutable.Set[ModelConstraint] = scala.collection.mutable.Set()

  var rootFeature: Feature = null
  val copris = new sugar.Sugar(Sat4j)

  var defaultConfig: Configuration = null

  private[this] var solutionOfDefaultConfig: jp.kobe_u.copris.Solution = null

  private[this] var allSolutions: scala.collection.mutable.Set[jp.kobe_u.copris.Solution] = scala.collection.mutable.Set()

  private[this] var additionalFeatureConstraints: scala.collection.mutable.Map[Feature, Constraint] = scala.collection.mutable.Map()

  private[this] var solutionSpaceNoMoreUpToDate = false

  /**
   *
   * The method returns one minimal configuration of the feature model. The minimality is defined by the number of
   * selected features and the number of numerical features not having their default value.
   *
   * As a consequence, we identify a configuration with the minimal number of selected boolean features and all numerical
   * features having a default value.
   *
   */
  def getMinimalConfig(): Configuration = {

    asPropositionalFormula()

    if (copris.find) {

      var solutions = copris.solutions

      var currSolution = solutions.next
      var numberOfSelectedBooleFeaturesAndNumericalFeatureNotAtDefaulfValue =
        countSelectedBooleanFeatures(currSolution) + countNumericalFeaturesNotAtDefaultValue(currSolution)

      defaultConfig = new Configuration()
      defaultConfig.readSolution(currSolution)
      solutionOfDefaultConfig = currSolution

      allSolutions.add(currSolution)
      while (solutions.hasNext) {
        currSolution = solutions.next
        allSolutions.add(currSolution)
        var numberOfSelectedBooleFeaturesAndNumericalFeatureNotAtDefaulfValueCurr =
          countSelectedBooleanFeatures(currSolution) + countNumericalFeaturesNotAtDefaultValue(currSolution)

        if (numberOfSelectedBooleFeaturesAndNumericalFeatureNotAtDefaulfValue
          > numberOfSelectedBooleFeaturesAndNumericalFeatureNotAtDefaulfValueCurr) {

          defaultConfig = new Configuration()
          defaultConfig.readSolution(currSolution)
          solutionOfDefaultConfig = currSolution

          numberOfSelectedBooleFeaturesAndNumericalFeatureNotAtDefaulfValue =
            numberOfSelectedBooleFeaturesAndNumericalFeatureNotAtDefaulfValueCurr
        }
      }
    }

    return defaultConfig
  }

  /**
   *
   * This method return a valid configuration with the smallest possible set of selected features to the default configuration under the condition that the desired feature
   * is selected.
   *
   */
  def getConfigForBoolFeature(feature: Feature): Configuration = {

    asPropositionalFormula()
    copris.add(Imp(TRUE, Bool(feature.identifier)))
    copris.find

    var oneMinimalConfiguration: Configuration = null
    var differentSelectedBooleFeaturesToDefaultConfig: Integer = Integer.MAX_VALUE

    var currSolution = copris.solution
    oneMinimalConfiguration = new Configuration()
    oneMinimalConfiguration.readSolution(currSolution)

    differentSelectedBooleFeaturesToDefaultConfig = defaultConfig.numberOfDifferentBooleanFeatures(oneMinimalConfiguration)

    while (copris.findNext) {
      var config = new Configuration()
      config.readSolution(copris.solution)
      var differentSelectedBooleFeaturesToDefaultConfig2 = defaultConfig.numberOfDifferentBooleanFeatures(config)

      if (differentSelectedBooleFeaturesToDefaultConfig > differentSelectedBooleFeaturesToDefaultConfig2) {
        oneMinimalConfiguration = config;
        differentSelectedBooleFeaturesToDefaultConfig = differentSelectedBooleFeaturesToDefaultConfig2
      }
    }
    return oneMinimalConfiguration
  }

  def printAllConfigurations() = {

    asPropositionalFormula()
    copris.find

    var i = 0

    var solutions = copris.solutions
    while (solutions.hasNext) {
      var solution = solutions.next
      i = i + 1
      var config = new Configuration()
      config.readSolution(solution)
      println(i + " " + config.toString)
    }
  }

  def countSelectedBooleanFeatures(solution: jp.kobe_u.copris.Solution): Integer = {
    return solution.boolValues.filter(_._2).size
  }

  def countNumericalFeaturesNotAtDefaultValue(solution: jp.kobe_u.copris.Solution): Integer = {
    return solution.intValues.filter(y => FeatureModel.allFeatures(y._1.toString).defaultValue != y._2.toInt).size
  }

  def test_generateFW_Variants() = {
    for (a <- allFeatures) {
      copris.init // = new sugar.Sugar()
      asPropositionalFormula()
      if (!a._2.isNumerical)
        //copris.add(Imp(allF(a).defaultValue, Bool(allF(a).identifier)))         
        copris.add(Imp(TRUE, Bool(a._1)))

      if (copris.find) {
        println("Solution for " + a._1 + "  " + copris.solution)

      } else {
        println("no solution with " + a._1)
      }

    }
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) = {
    if (!f.exists())
      f.createNewFile();
    val p = new java.io.PrintWriter(f)

    try { op(p) } finally { p.close() }
  }

  //  def readFeatureModel(file : String) : Unit = {
  //
  //    try {
  //
  //      var featureModelTextRepresentation = Source.fromFile(file).mkString.split("% constraints %")
  //
  //      interpretFeatureModel(featureModelTextRepresentation(0)) // feature model section
  //
  //      if (featureModelTextRepresentation.length > 1) // file has a constraint section
  //        featureModelTextRepresentation(1).split(";").map(f => interpretCrossTreeConstraint(f));
  //
  //      updatePropertiesOfFeatures(rootFeature);
  //
  //    } catch {
  //      case ex : FileNotFoundException => println("Couldn't find feature model.")
  //      case ex : IOException           => println("IOException trying to read the model")
  //    }
  //
  //  }

  //  def updatePropertiesOfFeatures(feature : Feature) : Unit = {
  //    if (!feature.isOptional) {
  //      feature.isSelectedInAllConfigs = true
  //
  //      if (parentChildRelationships.contains(feature))
  //        parentChildRelationships(feature).foreach(f => updatePropertiesOfFeatures(f))
  //    }
  //  }

  //  def interpretFeatureModel(tree : String) = {
  //    var statements = tree.split(";")
  //    for (i <- 0 until statements.length - 1) {
  //      var currStatement = statements(i)
  //      currStatement = currStatement.replaceAll("%[a-zA-Z0-9\\s()]+%", "") // remove comments
  //
  //      var featureName = currStatement.split(":")(0).trim()
  //      var specialization = currStatement.split(":")(1).trim()
  //
  //      var feat : Feature = allFeatures.get(featureName).getOrElse(new Feature(featureName))
  //      allFeatures(featureName) = feat
  //
  //      // consider specialization of current feature
  //      if (specialization.contains("{")) { // numerical feature
  //        // numerical features are no parent features
  //        feat.updateNumericalValues(specialization)
  //
  //      } else {
  //        var subfeatureNames = new Array[String](0)
  //        if (specialization.contains("|")) {
  //
  //          feat.isParentOfXor = true
  //
  //          subfeatureNames = specialization.split("\\|")
  //        } else {
  //          subfeatureNames = specialization.split(" ")
  //        }
  //        var subFeatures : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
  //        for (j <- 0 until subfeatureNames.length) {
  //          var currSubFeatureName = subfeatureNames(j).trim()
  //          if (currSubFeatureName.length() > 0) {
  //
  //            var isOptional = currSubFeatureName.startsWith("[")
  //            if (isOptional)
  //              currSubFeatureName = currSubFeatureName.substring(1, currSubFeatureName.length() - 1)
  //
  //            var currSubFeature : Feature = allFeatures.get(currSubFeatureName).getOrElse(new Feature(currSubFeatureName))
  //            allFeatures(currSubFeatureName) = currSubFeature
  //            currSubFeature.isOptional = isOptional
  //
  //            subFeatures.add(currSubFeature)
  //            currSubFeature.isChild = true
  //          }
  //        }
  //        parentChildRelationships.put(feat, subFeatures);
  //      }
  //
  //    }
  //    // the feature that does not appear within one right hand side is the root
  //    FeatureModel.rootFeature = allFeatures.find(x => (!x._2.isNumerical) && (!x._2.isChild)).get._2
  //
  //  }

  /**
   * Add one feature to the feature model. (no connections between features are created) => all features are
   * optional features with the "root feature" as parent feature
   *
   * Possible inputs:
   * var enum : Type = Value1 // [Value1|Value2|Value3]
   * var range : Int = 3 // [0-6|1]
   * var choose : Int = 6 // [6|26]
   * var bool : Boolean = true // [true|false]
   *
   */

  def addFeature(content: String): Unit = {
    var parts = content.split("//")
    if (parts.length > 2) {
      println("WARN:  Feature not added " + content)
      return
    }

    var name = parts(0).split(":")(0).trim().split(" ")(1)
    var default = parts(0).split("=")(1).trim()

    var values = parts(1).substring(parts(1).indexOf("[") + 1, parts(1).indexOf(']'))

    var feat: Feature = allFeatures.get(name).getOrElse(new Feature(name))
    allFeatures(name) = feat

    // boolean values
    if (default.equals("true") || default.equals("false")) {
      feat.defaultValue = augmentString(default).toBoolean
      return
    } else {
      feat.isNumerical = true
      feat.defaultValue = default
    }
    if (values.contains("-")) {
      // numerical values that are not enumerated
      feat.hasValuesRange = true

      feat.minValue = (augmentString(values.split("[|-]")(0)).toDouble-1)
      
      
      if (!values.split("[|-]")(1).equals("inf"))
        feat.maxValue = augmentString(values.split("[|-]")(1)).toDouble
      else{
//    FIXME    feat.maxValue = Double.MaxValue
        feat.maxValue = 3.0
      }
        
      if (values.contains("|"))
        feat.stepsize = augmentString(values.split("[|-]")(2)).toDouble

    } else {
      feat.values = values.split('|')
    }
  }

  def interpretCrossTreeConstraint(crossTreeConst: String): Unit = {
    var constraint = crossTreeConst.replaceAll("%[a-zA-Z0-9\\s()]+%", "")

    if (constraint.trim().length() == 0)
      return

    if (constraintHasOnlyBooleanFeatures(constraint))
      featureModelConstraints.add(new BooleanConstraint(constraint))
    else
      featureModelConstraints.add(new NumericalConstraint(constraint))

  }

  var noValue  : jp.kobe_u.copris.Term = Num(-1)
  var onValue  : jp.kobe_u.copris.Term = Num(1)
  var offValue : jp.kobe_u.copris.Term = Num(0)

  def asPropositionalFormula() = {

    
    //init of copris
    copris.init

    // FIXME:: Dirty hack because of problems in defining constraints between numerical and bool features
    // for example numerical feature x can only be selected if bool feature b is selected
//    onValue  = FeatureModel.copris.int(Var("on"),1,1)
//    offValue = FeatureModel.copris.int(Var("off"),0,0)
    
    // add all features to the copris namespace
    allFeatures.foreach(a =>
      if (a._2.isNumerical) 
        if(a._2.values .length > 0)
          FeatureModel.copris.int(Var(a._1), a._2.valuesAsIntSet())
//          print("!")
        else
          FeatureModel.copris.int(Var(a._1), a._2.minValue.toInt, a._2.maxValue.toInt)
      //else FeatureModel.copris.bool(Bool(a._1)))
      else 
        FeatureModel.copris.int(Var(a._1),0,1)
    )
    
    // add semantic of optional and mandatory parent sub-feature relationships
    parentChildRelationships
      .foreach(a =>
        if (a._1.isParentOfXor)
//          addXorRelationship(copris, a._1, a._2)
          println("TODO")
        else  
          addRelationship(copris, a._1, a._2)
//           println("TODO")
       )
        
//    // add cross tree constaints
//    featureModelConstraints
//      .foreach(const => copris.add(const.asPropositionalFormular))

    // root feature is selected
    //copris.add(Imp(TRUE, Bool(rootFeature.identifier)))
    
    
  }

  /**
   *
   *  f1,..,fn alternative sub-features of f ->  (f1 v ... v fn <=> f) ^ \bigvee_{i<j} \lnot(fi ^ fj)
   */
  def addXorRelationship(copris: jp.kobe_u.copris.sugar.Sugar, f: Feature, subFeatures: scala.collection.mutable.Set[Feature]) = {

    // (f1 v ... v fn)  -> x
    def subFeatureAnd(subFeat: scala.collection.mutable.Set[Feature]): Constraint = {
      if (subFeat.size == 2) return Or(Bool(subFeat.head.identifier), Bool(subFeat.tail.head.identifier))
      Or(Bool(subFeat.head.identifier), subFeatureAnd(subFeat.tail))
    }

    // x  <=> f -> atLeastOnePart
    copris.add(Iff(subFeatureAnd(subFeatures), Bool(f.identifier)))

    // \bigvee_{i<j} \lnot(fi ^ fj) -> notMoreThanOnePart
    var subF = subFeatures.toVector
    for (i <- 0 until subF.size; j <- i + 1 until subF.size) {
      copris.add(Not(And(Bool(subF.apply(i).identifier), Bool(subF.apply(j).identifier))))
    }
  }

  /**
   * f1,..,fn sub-feature of f
   *
   * if fi is optional  f => fi
   * if fi is mandatory f <=> fi
   */
  def addRelationship(copris: jp.kobe_u.copris.sugar.Sugar, parent: Feature, subFeatures: scala.collection.mutable.Set[Feature]) = {

    for(fi <- subFeatures){
      // TODO
      
//      if (a.isOptional)
        // a optional sub-feature of f -->  a => f 
//        if(parent.isNumerical )
//          copris.add(Imp(Bool(a.identifier), Bool(parent.identifier)))
//        else
          //Iff(Ge(b,1),Lt(p(1),p(2)))
      if(!fi.isNumerical)
    	  copris.add(Imp(Eq(Var(parent.identifier),offValue), Eq(Var(fi.identifier),offValue)))
      else 
          copris.add(Imp(Eq(Var(parent.identifier),offValue), Eq(Var(fi.identifier),noValue)))
//           copris.add(Imp(Eq(Var(parent.identifier),onValue),  Gt(Var(fi.identifier),noValue)))
//           copris.add(Imp(Eq(parent.varia,noValue), Eq(a.varia,onValue)))
//          println(Iff(Eq(a.varia,onValue),Eq(f.varia,noValue)))
//      else if (!a.isNumerical)
//        // a mandatory sub-feature of f -->  a <=> f
//        copris.add(Iff(Bool(a.identifier), Bool(f.identifier))))
    }
  }

  /**
   * The desired boolean feature becomes mandatory in all configurations if select is true and is deselected if select is false.
   */
  def boolFeatureHasToBeSeleted(feature: Feature, select: Boolean) = {
    if (select) {
      additionalFeatureConstraints.put(feature, Iff(Bool(rootFeature.identifier), Bool(feature.identifier)))
    } else {
      additionalFeatureConstraints.put(feature, Iff(Bool(rootFeature.identifier), Not(Bool(feature.identifier))))
    }
    solutionSpaceNoMoreUpToDate = true
  }

  def printFeatureTree() = {
    allFeatures.map(a => println(a.toString()))
  }

  def constraintHasOnlyBooleanFeatures(constraint: String): Boolean = {
    var literals = constraint.split(" ");
    for (lit <- literals) {
      var trimmedLit = lit.trim()
      if (FeatureModel.allFeatures.contains(trimmedLit)) {
        if (FeatureModel.allFeatures(trimmedLit).isNumerical) {
          return false
        }
      }
    }
    return true
  }

  def createFeatureDependenciesByFeatureNames(): Unit = {
    val featureAlias_alias_org: scala.collection.mutable.Map[String, String] = scala.collection.mutable.Map()
    featureAlias_alias_org.put("omp", "useOMP")
    featureAlias_alias_org.put("mpi", "useMPI")

    for (a <- allFeatures) {
      if (a._1.contains("_")) {
        var parentName = a._1.substring(0, a._1.lastIndexOf("_"))
        if (allFeatures.contains(parentName)){
          addParentChildRelationship(allFeatures(parentName), a._2)
        }
        if (featureAlias_alias_org.contains(parentName)) {
          var parentFeature = allFeatures(featureAlias_alias_org(parentName))
          addParentChildRelationship(parentFeature, a._2)
        }
      }
    }
  }

  def addParentChildRelationship(parent: Feature, child: Feature): Unit = {
    if (parentChildRelationships.contains(parent)) {
      parentChildRelationships(parent).add(child)
    } else {
      var list: scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
      list.add(child)
      parentChildRelationships.put(parent, list)
    }
  }

} 
  


