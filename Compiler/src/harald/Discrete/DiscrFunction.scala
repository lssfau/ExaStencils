package harald.Discrete

import scala.collection.mutable.ListBuffer
import harald.Abstract._

class DiscrType() {
  
}

case class DiscrFunctionType(domain : Tuple2[String, String], domaindimension : Int, domainvectorlength : Int, codomain : String, codomaindimension : Int, codomainvectorlength : Int) extends DiscrType {

    override def toString = {
      var str = ""
      if (!(domain._2.equals("")))
        str = "x ${domain._2}"

      s"((${domain}${str})^${domaindimension})^${domainvectorlength} -> (${codomain}^${codomaindimension})^${codomainvectorlength}"
    }

}

class DiscrObject() {
  var name : String = ""
    
}

case class DiscrFunction(nam : String, val functype: DiscrFunctionType, val evalpoints : String) extends DiscrObject  {

  
  var funcexpr : ListBuffer[DiscrEquation] = ListBuffer()
  name = nam
  override def toString = s"${name} : ${functype.toString} = ${funcexpr} at ${evalpoints}"
  
  def transform : AbstractField = {
    return new AbstractField(name, functype.codomain, functype.codomainvectorlength.toString, evalpoints)
  }
}

case class DiscrConstType(domain : String, domainvectorlength : Int) extends DiscrType {
  override def toString = s"${domain}^${domainvectorlength}"  
}

case class DiscrConstant(nam : String, consttype: DiscrConstType, value : Double) extends DiscrObject  {
  
  name = nam
  
  override def toString = s"${name} : ${consttype.toString} = ${value}"
}
