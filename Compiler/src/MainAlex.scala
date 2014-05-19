import exastencils.spl.FeatureModel
import exastencils.spl.heuristics.FWHeuristic
import exastencils.spl.test.PredictionTests
import java.io.File


object MainAlex {
  def main(args : Array[String]) : Unit = {

    
////////// read feature model text + get default config    
//    FeatureModel.readFeatureModel("E:/ScalaPrototyp/Compiler/src/exastencils/spl/featureModel/model_HSMGP_noCores.model")
    
//    FeatureModel.readFeatureModel("./featureModel/model_HSMGP_noCores_numerical.model")
//    
//
//    //FeatureModel.readFeatureModel("E:/ScalaPrototyp/ScalaExaStencil/Compiler/src/exastencils/knowledge/model.model")
//    //  FeatureModel.readFeatureModel("E:/ScalaPrototyp/ScalaExaStencil/Compiler/src/exastencils/spl/model_Small.model")
//    //    FeatureModel.readFeatureModel("E:/ScalaPrototyp/ScalaExaStencil/Compiler/src/exastencils/spl/model_HSMGP.model")
//
//    var configuration = FeatureModel.getMinimalConfig
//    println("defaultConfig: " + configuration)
//
//    
//    println(configuration.getValueOfNumericalFeature("pre"))
    
    
    
//    FeatureModel.printAllConfigurations()
    
/////////// test fwHeuristic     
//    var fwheuristic = new FWHeuristic()
//    fwheuristic.generateFeatureWiseConfigs    
//    
//    
//    
//////// predict nfpValue of configurations    
//    var prediction : PredictionTests = new PredictionTests()
//    prediction.readFile("E:/ScalaPrototyp/Compiler/src/exastencils/spl/test/P2D_minimalOnlyAvgTime_mini.txt")
//    prediction.predictPerformanceFW(fwheuristic.fWConfigs)

    // test feature unvertainty
    //prediction.predictPerformanceFWUncertainty(fwheuristic.fWConfigs)
    
    println("Done!");
  }
}
