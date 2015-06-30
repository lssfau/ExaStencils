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
import scala.reflect.macros.blackbox
import exastencils.knowledge.Knowledge

object FeatureModel {

  val parentChildRelationships : scala.collection.mutable.Map[Feature, scala.collection.mutable.Set[Feature]] = scala.collection.mutable.Map()
  val allFeatures : scala.collection.mutable.Map[String, Feature] = scala.collection.mutable.Map()

  val featureModelConstraints : scala.collection.mutable.Set[ModelConstraint] = scala.collection.mutable.Set()

  val splContraints : scala.collection.mutable.Set[SPLConstraint] = scala.collection.mutable.Set()

  var rootFeature : Feature = null
  val copris = new sugar.Sugar(Sat4j)

  var defaultConfig : Configuration = null

  private[this] var solutionOfDefaultConfig : jp.kobe_u.copris.Solution = null

  private[this] var allSolutions : scala.collection.mutable.Set[jp.kobe_u.copris.Solution] = scala.collection.mutable.Set()

  private[this] var additionalFeatureConstraints : scala.collection.mutable.Map[Feature, Constraint] = scala.collection.mutable.Map()

  private[this] var solutionSpaceNoMoreUpToDate = false

  val constants : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()

  def get(name : String) : Feature = {
    return allFeatures(name)
  }

  def getDefaultConfig() : Configuration = {
    if (defaultConfig != null)
      return defaultConfig

    // all selected boolean features
    var boolFeatures : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    allFeatures.filter(x => !x._2.isNumerical && x._2.defaultValue.isInstanceOf[Boolean] && x._2.defaultValue.asInstanceOf[Boolean]).foreach(y => boolFeatures += y._2)

    var numFeatures : scala.collection.mutable.Map[Feature, Double] = scala.collection.mutable.Map()
    allFeatures.filter(x => x._2.isNumerical).foreach(f => numFeatures.put(f._2, f._2.defaultValue.asInstanceOf[Double]))

    var XORFeatures : scala.collection.mutable.Map[Feature, String] = scala.collection.mutable.Map()
    allFeatures.filter(x => x._2.isXorFeature).foreach(y => XORFeatures.put(y._2, y._2.defaultValue.asInstanceOf[String]))

    defaultConfig = new Configuration()
    defaultConfig.readSolution(boolFeatures, numFeatures, XORFeatures)

    return defaultConfig
  }

  def filter(featureToConsider : scala.collection.mutable.Set[String]) = {

    allFeatures.foreach(x => { if (!featureToConsider.contains(x._1)) allFeatures -= x._1 })

  }

  /**
    *
    * The method returns one minimal configuration of the feature model. The minimality is defined by the number of
    * selected features and the number of numerical features not having their default value.
    *
    * As a consequence, we identify a configuration with the minimal number of selected boolean features and all numerical
    * features having a default value.
    *
    */
  def getMinimalConfig() : Configuration = {

    asPropositionalFormula()

    println("start finding")

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
  def getConfigForBoolFeature(feature : Feature) : Configuration = {

    asPropositionalFormula()
    copris.add(Imp(TRUE, Bool(feature.identifier)))
    copris.find

    var oneMinimalConfiguration : Configuration = null
    var differentSelectedBooleFeaturesToDefaultConfig : Integer = Integer.MAX_VALUE

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
      //      println(i + " "+ solution)
      i = i + 1
      println(i)
      //      var config = new Configuration()
      //      config.readSolution(solution)
      //      println(i + " " + config.toString)
    }
    println(i)
  }

  def countSelectedBooleanFeatures(solution : jp.kobe_u.copris.Solution) : Integer = {
    return solution.boolValues.filter(_._2).size
  }

  def countNumericalFeaturesNotAtDefaultValue(solution : jp.kobe_u.copris.Solution) : Integer = {
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

  def printToFile(f : java.io.File)(op : java.io.PrintWriter => Unit) = {
    if (!f.exists())
      f.createNewFile();
    val p = new java.io.PrintWriter(f)

    try { op(p) } finally { p.close() }
  }

  def featureHasOnlyNumericValues(content : Array[String]) : Boolean = {
    for (a <- 0 to content.length - 1) {
      if (!constants.contains(content(a)))
        if (!isNumeric(content(a)) && content(a).compareTo("inf") != 0)
          return false
    }
    return true
  }

  def isNumeric(input : String) : Boolean = input.forall(x => x.isDigit || x == '.' || x == '-')

  /**
    * Add one feature to the feature model. (no connections between features are created) => all features are
    * optional features with the "root feature" as parent feature
    *
    * Possible inputs:
    * var enum : Type = Value1 // [Value1|Value2|Value3]
    * var range : Int = 3 // [0~6|1]
    * var choose : Int = 6 // [6|26]
    * var bool : Boolean = true // [true|false]
    *
    */
  def addFeature_KnowledgeFile(content : String) : Unit = {
    if (content.contains("112~1000000000"))
      println()
    var parts = content.split("//")
    var name = parts(0).split(":")(0).trim().split(" ")(1)
    var dataType = parts(0).split(":")(1).trim().split(" ")(0)
    var default = parts(0).split("=")(1).trim()
    //    println(content)
    var values = parts(1).substring(parts(1).indexOf("[") + 1, parts(1).indexOf(']'))

    var feat : Feature = allFeatures.get(name).getOrElse(new Feature(name))
    allFeatures(name) = feat

    feat.dataType = dataType

    if (name.equals("l3tmp_numPost"))
      println("")

    // boolean values
    if (default.equals("true") || default.equals("false")) {
      feat.defaultValue = default.trim()
      return
    } else {
      // non-boolean features
      var stepsize = 1.0

      var computation = name + "+1"
      if (values.contains("§")) {
        computation = (values.split("[§]")(1).toString())
        values = values.split("[§]")(0).trim()
      }
      if (values.contains("$")) {
        stepsize = (values.split("[$]")(1).toString()).toDouble
        values = values.split("[$]")(0).trim()
      }
      var x = values.split("[\\|~]")
      var isNumericFeature = featureHasOnlyNumericValues(values.split("[\\|~]"))

      if (isNumericFeature) {
        feat.isOptional = false
        feat.isNumerical = true
        feat.defaultValue = default.trim()

        if (values.contains("~")) {

          // numerical values that are not enumerated
          feat.hasValuesRange = true

          feat.minValue = (augmentString(values.split("[~]")(0)).toDouble)

          var maxValueString = values.split("[~]")(1)

          if (maxValueString.equals("inf")) {
            //    FIXME    feat.maxValue = Double.MaxValue
            //			inf has to be defined
            feat.maxValue = 10000000;
          } else if (!FeatureModel.constants.contains((maxValueString))) {

            feat.maxValue = (augmentString(values.split("[~]")(1)).toDouble)
          } else {

            // Reflection magic
            var field = Knowledge.getClass.getDeclaredField(maxValueString)
            field.setAccessible(true)
            feat.maxValue = field.getInt(Knowledge).toDouble
          }
          feat.valueCalculation = computation
          if (name.equals("sisc2015_firstDim"))
            print()
          println(feat.identifier)
          // TODO annotation at the features whether the "numeric" feature is metric or not
          if (!feat.hasNValues(3) || feat.identifier.equals("poly_optLevel_fine")) {
            feat.isXorFeature = true
            feat.values = feat.getAllValuesAsString().toArray
          }

        } else {
          if (values.contains("|")) {
            feat.isXorFeature = true
            feat.values = values.split("\\|")
          } else {
            println("ERROR: consider " + content)
          }
        }

      } else {
        feat.isXorFeature = true
        feat.defaultValue = augmentString(default).toString.replace("\"", "")
        feat.values = values.split('|')
      }

    }
  }

  def addConstant(content : String) = {
    var parts = content.split("//")
    var name = parts(0).split(":")(0).trim().split(" ")(1)

    constants.add(name);

  }

  def addConstraint_KnowledgeFile(param : String, value : String, cond : String) : Unit = {
    var feature = param.split("\\.")(param.split("\\.").length - 1)

    // feature with constraint can not be modified by the SPL
    if (!allFeatures.contains(feature))
      return
    // non of the features in the condition and in the value computation can be modified by the SPL  
    if (!(atLeastOneFeatureInCondition(value) || atLeastOneFeatureInCondition(cond)))
      return

    // the remaining constraints are interesting for the SPL  

    // remove packages from value and condition
    var valueWithoutPackages = value.replace("scala.math.`package`.", "");
    valueWithoutPackages = valueWithoutPackages.replace("Knowledge.this.", "");

    var conditionWithoutPackages = cond.replace("scala.math.`package`.", "");
    conditionWithoutPackages = conditionWithoutPackages.replace("Knowledge.this.", "");

    println("feature: " + feature + " VALUE: " + valueWithoutPackages + " COND: " + conditionWithoutPackages)

  }

  /**
    * I have the assumption that all features (variability points) are defined within the Knowledge file.
    */
  def atLeastOneFeatureInCondition(value : String) : Boolean = {
    if (value.contains("Knowledge.this.")) {
      var firstPotentialFeature = value.substring(value.indexOf("Knowledge.this."))
      firstPotentialFeature = firstPotentialFeature.substring("Knowledge.this.".length()).split("[\\.\\)\\(]")(0)
      var rest = value.substring("Knowledge.this.".length() + firstPotentialFeature.size)
      return atLeastOneFeatureInCondition(rest) || allFeatures.contains(firstPotentialFeature)
    } else
      return false;
  }

  def FAMASyntax_InterpretCrossTreeConstraint(crossTreeConst : String) : Unit = {
    var constraint = crossTreeConst.replaceAll("%[a-zA-Z0-9\\s()]+%", "")

    if (constraint.trim().length() == 0)
      return

    if (constraintHasOnlyBooleanFeatures(constraint))
      featureModelConstraints.add(new BooleanConstraint(constraint))
    else
      featureModelConstraints.add(new NumericalConstraint(constraint))

  }

  var noValue : jp.kobe_u.copris.Term = Num(-1)
  var onValue : jp.kobe_u.copris.Term = Num(1)
  var offValue : jp.kobe_u.copris.Term = Num(0)

  def asPropositionalFormula() = {

    //init of copris
    copris.init

    // FIXME:: Dirty hack because of problems in defining constraints between numerical and bool features
    // for example numerical feature x can only be selected if bool feature b is selected
    //    onValue  = FeatureModel.copris.int(Var("on"),1,1)
    //    offValue = FeatureModel.copris.int(Var("off"),0,0)

    // add all features to the copris namespace
    allFeatures.foreach { a =>
      if (a._2.isNumerical && !a._2.isXorFeature) {
        //        if (a._2.values.length > 0)
        //          FeatureModel.copris.int(Var(a._1), a._2.valuesAsIntSet())
        //        else
        //          FeatureModel.copris.int(Var(a._1), a._2.minValue.toInt, a._2.maxValue.toInt)
      } //else FeatureModel.copris.bool(Bool(a._1)))
      else if (!a._2.isNumerical && !a._2.isXorFeature)
        FeatureModel.copris.bool(Bool(a._1))
      else
        // add xor features (for each feature value add a feature + add constraints between these features
        //because only one of these features can be selected at a time
        this.propositianlFormular_addXORFeature(a._2)

    }
    // add semantic of optional and mandatory parent sub-feature relationships
    parentChildRelationships
      .foreach(a =>
        if (a._1.isXorFeature)
          //          addXorRelationship(copris, a._1, a._2)
          println("TODO")
        else
          addRelationship(copris, a._1, a._2) //           println("TODO")
          )

    //    // add cross tree constaints
    //    featureModelConstraints
    //      .foreach(const => copris.add(const.asPropositionalFormular))

    // root feature is selected
    //copris.add(Imp(TRUE, Bool(rootFeature.identifier)))

    // cross tree constraints
    splContraints.foreach(a => FeatureModel.copris.add(a.asCoprisConstraint))
  }

  def propositianlFormular_addXORFeature(feature : Feature) = {
    var childFeatures : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()

    FeatureModel.copris.bool(Bool(feature.identifier))
    copris.add(Imp(TRUE, Bool(feature.identifier)))

    feature.values.foreach { a =>
      FeatureModel.copris.bool(Bool(feature.identifier + "__" + a))
      childFeatures.add(feature.identifier + "__" + a)
    }

    if (childFeatures.size > 1)
      addXorRelationshipString(FeatureModel.copris, feature, childFeatures)

  }

  /**
    *
    *  f1,..,fn alternative sub-features of f ->  (f1 v ... v fn <=> f) ^ \bigvee_{i<j} \lnot(fi ^ fj)
    */
  def addXorRelationship(copris : jp.kobe_u.copris.sugar.Sugar, f : Feature, subFeatures : scala.collection.mutable.Set[Feature]) = {

    // (f1 v ... v fn)  -> x
    def subFeatureAnd(subFeat : scala.collection.mutable.Set[Feature]) : Constraint = {
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

  def addXorRelationshipString(copris : jp.kobe_u.copris.sugar.Sugar, f : Feature, subFeatures : scala.collection.mutable.Set[String]) = {

    // (f1 v ... v fn)  -> x
    def subFeatureAnd(subFeat : scala.collection.mutable.Set[String]) : Constraint = {
      if (subFeat.size == 2) return Or(Bool(subFeat.head), Bool(subFeat.tail.head))
      Or(Bool(subFeat.head), subFeatureAnd(subFeat.tail))
    }

    // x  <=> f -> atLeastOnePart
    copris.add(Iff(subFeatureAnd(subFeatures), Bool(f.identifier)))

    // \bigvee_{i<j} \lnot(fi ^ fj) -> notMoreThanOnePart
    var subF = subFeatures.toVector
    for (i <- 0 until subF.size; j <- i + 1 until subF.size) {
      copris.add(Not(And(Bool(subF.apply(i)), Bool(subF.apply(j)))))
    }
  }

  /**
    * f1,..,fn sub-feature of f
    *
    * if fi is optional  f => fi
    * if fi is mandatory f <=> fi
    */
  def addRelationship(copris : jp.kobe_u.copris.sugar.Sugar, parent : Feature, subFeatures : scala.collection.mutable.Set[Feature]) = {

    for (fi <- subFeatures) {
      // TODO
      //      if (a.isOptional)
      // a optional sub-feature of f -->  a => f 
      //        if(parent.isNumerical )
      //          copris.add(Imp(Bool(a.identifier), Bool(parent.identifier)))
      //        else
      //Iff(Ge(b,1),Lt(p(1),p(2)))
      if (!parent.isNumerical)
        if (!fi.isNumerical) {
          //          println("parent: "+parent.identifier + "  child: "+fi.identifier )
          copris.add(Imp(Bool(fi.identifier), Bool(parent.identifier)))
        } //        else
        //          copris.add(Imp(Eq(Var(parent.identifier), offValue), Eq(Var(fi.identifier), noValue)))
        else {
          //        Logger.warn("Parent child relationship if parent is numerical feature is not considered by solver.")
          //      else if (!a.isNumerical)
          //        // a mandatory sub-feature of f -->  a <=> f
          //        copris.add(Iff(Bool(a.identifier), Bool(f.identifier))))
        }
    }
  }

  /**
    * The desired boolean feature becomes mandatory in all configurations if select is true and is deselected if select is false.
    */
  def boolFeatureHasToBeSeleted(feature : Feature, select : Boolean) = {
    if (select) {
      additionalFeatureConstraints.put(feature, Iff(Bool(rootFeature.identifier), Bool(feature.identifier)))
    } else {
      additionalFeatureConstraints.put(feature, Iff(Bool(rootFeature.identifier), Not(Bool(feature.identifier))))
    }
    solutionSpaceNoMoreUpToDate = true
  }

  def printFeatureTree() = {
    allFeatures.map(a => println(a._2.toString()))
  }

  def constraintHasOnlyBooleanFeatures(constraint : String) : Boolean = {
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

  def createFeatureDependenciesByFeatureNames() : Unit = {
    // TODO HACK
    val featureAlias_to_org : scala.collection.mutable.Map[String, String] = scala.collection.mutable.Map()
    featureAlias_to_org.put("omp", "omp_enabled")
    featureAlias_to_org.put("mpi", "mpi_enabled")
    //   featureAlias_to_org.put("poly","poly_usePolyOpt")

    for (a <- allFeatures) {
      if (a._1.contains("_")) {
        var parentName = a._1.substring(0, a._1.indexOf("_"))
        if (allFeatures.contains(parentName)) {
          addParentChildRelationship(allFeatures(parentName), a._2)
        }
        if (featureAlias_to_org.contains(parentName)) {
          var parentFeature = allFeatures(featureAlias_to_org(parentName))
          parentFeature.isOptional = false
          addParentChildRelationship(parentFeature, a._2)
        }
      }
    }
  }

  def addParentChildRelationship(parent : Feature, child : Feature) : Unit = {
    if (parent.equals(child))
      return
    if (parentChildRelationships.contains(parent)) {
      parentChildRelationships(parent).add(child)
    } else {
      var list : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
      list.add(child)
      parentChildRelationships.put(parent, list)
    }
  }

  def FAMASyntax_ReadFeatureModel(file : String) : Unit = {

    try {

      var featureModelTextRepresentation = Source.fromFile(file).mkString.split("% constraints %")

      FAMASyntax_InterpretFeatureModel(featureModelTextRepresentation(0)) // feature model section

      if (featureModelTextRepresentation.length > 1) // file has a constraint section
        featureModelTextRepresentation(1).split(";").map(f => FAMASyntax_InterpretCrossTreeConstraint(f));

    } catch {
      case ex : FileNotFoundException => println("Couldn't find feature model.")
      case ex : IOException           => println("IOException trying to read the model")
    }

  }

  def FAMASyntax_InterpretFeatureModel(tree : String) = {
    var statements = tree.split(";")
    for (i <- 0 until statements.length - 1) {
      var currStatement = statements(i)
      currStatement = currStatement.replaceAll("%[a-zA-Z0-9\\s()]+%", "") // remove comments

      var featureName = currStatement.split(":")(0).trim()
      var specialization = currStatement.split(":")(1).trim()

      var feat : Feature = allFeatures.get(featureName).getOrElse(new Feature(featureName))
      allFeatures(featureName) = feat

      // consider specialization of current feature
      if (specialization.contains("{")) { // numerical feature
        // numerical features are no parent features
        feat.updateNumericalValues(specialization)

      } else {
        var subfeatureNames = new Array[String](0)
        if (specialization.contains("|")) {

          feat.isXorFeature = true

          subfeatureNames = specialization.split("\\|")
        } else {
          subfeatureNames = specialization.split(" ")
        }
        var subFeatures : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
        for (j <- 0 until subfeatureNames.length) {
          var currSubFeatureName = subfeatureNames(j).trim()
          if (currSubFeatureName.length() > 0) {

            var isOptional = currSubFeatureName.startsWith("[")
            if (isOptional)
              currSubFeatureName = currSubFeatureName.substring(1, currSubFeatureName.length() - 1)

            var currSubFeature : Feature = allFeatures.get(currSubFeatureName).getOrElse(new Feature(currSubFeatureName))
            allFeatures(currSubFeatureName) = currSubFeature
            currSubFeature.isOptional = isOptional

            subFeatures.add(currSubFeature)
            currSubFeature.isChild = true
          }
        }
        parentChildRelationships.put(feat, subFeatures);
      }

    }
    // the feature that does not appear within one right hand side is the root

    // TODO if there is no root feature in the feature model or no binary feature.  
    //    FeatureModel.rootFeature = allFeatures.find(x => ( ! x._2.isNumerical ) && ( ! x._2.isChild )).get._2

  }

  def featureWiseConfig(featureName : String) : Configuration = {

    println("-----------------------")
    FeatureModel.asPropositionalFormula;
    copris.add(Imp(TRUE, Bool(featureName)))
    copris.find
    print(featureName + " ")
    var solution = copris.solutions.next

    var conf = new Configuration()
    conf.readSolution(solution)
    println(conf.boolFeatures.filter(x => x._2).size)
    println(conf.toString)

    return conf;

  }
  /**
    *
    * With this method, a set of [[exastencils.spl.Configuration]] can be filtered with a given set of [[exastencils.sp.Feature]].
    * All of the resulting configuration contain ALL features.
    *
    * @param a set of configurations
    * @param the set of white list boolean features
    * @return all configurations of the input set containing all feature of the white list
    *
    */
  def filterConfigurations(configurations : scala.collection.mutable.Set[exastencils.spl.Configuration],
    selectedBooleanFeatures : scala.collection.mutable.Set[Feature]) : scala.collection.mutable.Set[exastencils.spl.Configuration] = {
    var matchingConfigs : scala.collection.mutable.Set[exastencils.spl.Configuration] = scala.collection.mutable.Set()
    configurations.foreach(x => {
      var allBooleanFeatures = true
      selectedBooleanFeatures.foreach(y => if (!x.boolFeatures.contains(y)) allBooleanFeatures = false)
      if (allBooleanFeatures)
        matchingConfigs.add(x)
    })
    return matchingConfigs
  }

  def filterConfigurations(configurations : scala.collection.mutable.Set[exastencils.spl.Configuration],
    selectedNumericalFeatures : scala.collection.mutable.Map[Feature, Double]) : scala.collection.mutable.Set[exastencils.spl.Configuration] = {

    var matchingConfigs : scala.collection.mutable.Set[exastencils.spl.Configuration] = scala.collection.mutable.Set()
    configurations.foreach(x => {
      var allNumFeatures = true
      selectedNumericalFeatures.foreach(y =>
        if (!x.numericalFeatureValues.contains(y._1) || !y._2.equals(x.numericalFeatureValues(y._1)))
          allNumFeatures = false)
      if (allNumFeatures) {
        matchingConfigs.add(x)
      }
    })
    return matchingConfigs
  }

  def filterConfigurationsAtLeastOne(configurations : scala.collection.mutable.Set[exastencils.spl.Configuration],
    selectedNumericalFeatures : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature, Double]]) : scala.collection.mutable.Set[exastencils.spl.Configuration] = {

    var matchingConfigs : scala.collection.mutable.Set[exastencils.spl.Configuration] = scala.collection.mutable.Set()
    configurations.foreach(x => {

      selectedNumericalFeatures.foreach(y => {
        var allNumFeatures = true
        y.foreach(z => if (!x.numericalFeatureValues.contains(z._1) || !z._2.equals(x.numericalFeatureValues(z._1))) allNumFeatures = false)
        if (allNumFeatures)
          matchingConfigs.add(x)
      })
    })
    return matchingConfigs
  }

  def getFeature(name : String) : Feature = {
    if (allFeatures.contains(name))
      return allFeatures(name)
    else
      return null
  }

  def getSpecificFeatureDkPaperTest(featureType : String) : Feature = {

    // make it generic 
    if (featureType.equals("Smoother"))
      return allFeatures.get("JAC").get;

    if (featureType.equals("pre"))
      return allFeatures.get("pre").get

    if (featureType.equals("post"))
      return allFeatures.get("post").get

    if (featureType.equals("cores"))
      return allFeatures.get("cores").get

    if (featureType.equals("cgs"))
      return allFeatures.get("RED_AMG").get
    return null
  }

}

