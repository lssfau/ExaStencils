package exastencils.spl.heuristics

import exastencils.spl._

class FWHeuristic {

  var fWConfigs : scala.collection.mutable.Set[Configuration] = scala.collection.mutable.Set()

//  def generateFeatureWiseConfigs = {
//    FeatureModel.allFeatures.filter(x =>  (x._2.isNumerical == false) && (x._2.isSelectedInAllConfigs == false)).foreach(a => fWConfigs.add(FeatureModel.getConfigForBoolFeature(a._2)))
//  }

}