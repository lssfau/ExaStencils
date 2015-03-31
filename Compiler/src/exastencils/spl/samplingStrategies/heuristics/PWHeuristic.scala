package exastencils.spl.samplingStrategies.heuristics

import exastencils.spl._

class PWHeuristic(binaryFeaturs : scala.collection.mutable.Set[Feature]) {

 
  
  
  // binary features being considered by the sampling
  var binaryFeaturesToConsider : scala.collection.mutable.Set[Feature] = binaryFeaturs
  
  def getPoints() : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature,String]] = {
    
    var pairWiseConfigs : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature, String]] = scala.collection.mutable.Set()
    
    var binaryFeatures = binaryFeaturesToConsider.toArray[Feature]    
    
    var defaultValues :scala.collection.mutable.Map[Feature,String] = scala.collection.mutable.Map()
    
    binaryFeaturesToConsider.foreach { x => defaultValues.put(x, x.defaultValue)}

    for(x <- 0 to binaryFeatures.length-1){
      for(y <- x+1 to binaryFeatures.length-1){
        var xValues : scala.collection.mutable.Set[String] = allNonDefaultValuesOneFeature(binaryFeatures(x)) 
        var yValues : scala.collection.mutable.Set[String] = allNonDefaultValuesOneFeature(binaryFeatures(y)) 
        
        xValues.foreach { xV => {
          yValues.foreach { yV => {
             var config : scala.collection.mutable.Map[Feature,String] = scala.collection.mutable.Map()
             config.put(binaryFeatures(x), xV)
             config.put(binaryFeatures(y), yV)
             
             addDefaultValuesOfOther(config, defaultValues)
             
             pairWiseConfigs.add(config)
          }}  
        }}
      }
    }
    return pairWiseConfigs
  }

  def allNonDefaultValuesOneFeature(feature: Feature) : scala.collection.mutable.Set[String] = {
    var values : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()
    
    if(feature.isXorFeature){
      feature.values.foreach { 
        y => 
          if(feature.dataType.equals("Int")){
            var default = feature.defaultValue.toInt.toString()
            if(!default.equals(y.toDouble.toInt.toString())){
              values.add(y.toDouble.toInt.toString())   
            }
          }
          else if(!feature.defaultValue.equals(y)){
            values.add(y)   
          }
      } 
    }else{
      if(feature.defaultValue == "false"){
            values.add("true")  
      }else{
            values.add("false")  
      }
    } 
    return values
  }
  
  def addDefaultValuesOfOther(config: scala.collection.mutable.Map[Feature,String], defaultValues: scala.collection.mutable.Map[Feature,String]) = {
      defaultValues.foreach(x => if(!config.contains(x._1)){ 
          config.put(x._1, x._2)
      })
  }
  
  
}