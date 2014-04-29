package harald.Continuous

import harald.Discrete
import harald.Discrete._
import harald.dsl._

case class ContOperatorType(inputfunctype: ContFunctionType, outputfunctype: ContFunctionType)  extends ContType {
  override def toString = s"(${inputfunctype}) -> (${outputfunctype})"
  
  override def discretize(datatype : String, location : String) : DiscrOperatorType = {
    return new DiscrOperatorType(inputfunctype.discretize(datatype : String, location : String), outputfunctype.discretize(datatype : String, location : String))
  }
}

case class ContOperator(nam : String, optype: ContOperatorType) extends ContObject {
  name = nam
  override def toString = s"${name} : ${optype}"
  

  override def discretize(datatype : String, location : String) : DiscrOperator = {
    return new Discrete.DiscrOperator(nam,optype.discretize(datatype : String, location : String))
  }

}