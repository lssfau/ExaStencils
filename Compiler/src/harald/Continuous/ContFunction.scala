package harald.Continuous

import scala.collection.mutable.ListBuffer
import harald.Discrete
import harald.Discrete._
import harald.dsl._

class ContType() {
  def discretize(datatype : String, location : String) : DiscrType = new Discrete.DiscrType()
}

class ContObject() {
  var name : String = ""
    
  def discretize(datatype : String, location : String) : DiscrObject = new Discrete.DiscrObject()
}

class ContConstType(domain : String, domainvectorlength : Int) extends ContType {
  override def toString = s"${domain}^${domainvectorlength}"  

  override def discretize(datatype : String, location : String) : DiscrConstType = {     
      new Discrete.DiscrConstType(datatype , domainvectorlength)
  }
}

case class ContFunctionType(domain : Tuple2[String, String], domaindimension : Int, domainvectorlength : Int, codomain : String, codomaindimension : Int, codomainvectorlength : Int) extends ContType {
  override def toString = {
    var str = ""
    if (!(domain._2.equals("")))
      str = "x ${domain._2}"
    s"(${domain}^${domaindimension}${str})^${domainvectorlength} -> (${codomain}^${codomaindimension})^${codomainvectorlength}"
  }
  
    override def discretize(datatype : String, location : String) : DiscrFunctionType = {
      
      new Discrete.DiscrFunctionType(domain,domaindimension,domainvectorlength,  DomainKnowledge.globaldatatype_L2 , codomaindimension, codomainvectorlength)
    }
}

case class ContConstant(nam : String, consttype: ContConstType, value : Double) extends ContObject  {
  
  name = nam
  
  override def toString = s"${name} : ${consttype.toString} = ${value}"

  override def discretize(datatype : String, location : String) : DiscrConstant = {    
      new Discrete.DiscrConstant(nam, consttype.discretize(datatype,location),value)
  }
}

case class ContFunction(nam : String, functype: ContFunctionType) extends ContObject  {
  
  var funcexpr : ListBuffer[ContEquation] = ListBuffer()
  name = nam
  override def toString = s"${name} : ${functype.toString} = ${funcexpr}"
  
  override def discretize(datatype : String, location : String) : DiscrFunction = {
    return new Discrete.DiscrFunction(nam,functype.discretize(datatype,location), DomainKnowledge.unknownlocation_L2)
  }
}