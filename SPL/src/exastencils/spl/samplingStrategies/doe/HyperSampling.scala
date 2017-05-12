package exastencils.spl.samplingStrategies.doe

import exastencils.spl.Feature

/**
 * This class represents a hyper sampling experimental design. The basic idea of this design is to minimize the value domain of the numeric 
 * configuration options to a given number. Afterwards, the remaining values are combined with each other, leading to x pow n sampling points. 
 * Where x is the number of remaining values for a configuration option and n is the number of configuration options. 
 */
class HyperSampling(numericFeaturs : scala.collection.mutable.Set[Feature]) {

  var numerofValues : Integer = 7;
  
  def getPoints() : scala.collection.mutable.Set[scala. collection.mutable.Map[Feature, Double]] = {
    var resultPoints : scala.collection.mutable.Set[scala. collection.mutable.Map[Feature, Double]] = scala.collection.mutable.Set()
   
    var valuesOfFeatures : scala.collection.mutable.Map[Feature, Array[Double]] = scala.collection.mutable.Map()
   
    // compute the values to consider for each numeric feature
    numericFeaturs.foreach(x => {
      valuesOfFeatures.put(x, x.getSampledValue(numerofValues))
    })
    
    var values : Array[Integer] = Array.fill(numericFeaturs.size)(0)
    
    
    while(true){
      var currConfig : scala.collection.mutable.Map[Feature, Double] = scala.collection.mutable.Map()
      var featureIndex : Integer  = 0
       
      valuesOfFeatures.foreach(x => {
        currConfig.put(x._1, x._2(values(featureIndex)))        
        featureIndex += 1
      })
      resultPoints.add(currConfig)
      values = incrementArray(values)
      
      if(values == null)
        return resultPoints
    }     
    
    return resultPoints;
  }
  
  def incrementArray(values : Array[Integer]) : Array[Integer] = {
      var updated : Boolean = false;
      var runningIndex : Integer = 0;
    
      while(!updated){
        if(values(runningIndex) == numerofValues -1){
          values(runningIndex) = 0
          runningIndex += 1
        }else{
          values(runningIndex)  = values(runningIndex) +1
          updated = true
          return values
        }
        
        if(runningIndex == this.numericFeaturs.size)
          return null
        
      }
      return values
  }
  
}