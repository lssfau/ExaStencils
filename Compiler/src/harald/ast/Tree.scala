package harald.ast

import scala.collection.mutable.ListBuffer
import harald.Abstract._
import harald.Impl
import harald.Impl._
import harald.dsl._


class TreeL2 {
  
  val exaFields: ListBuffer[AbstractField] = new ListBuffer()
  val exaOperators: ListBuffer[AbstractStencil] = new ListBuffer()

  val Fields: ListBuffer[ImplField] = new ListBuffer()
  val GhostFields: ListBuffer[ImplField] = new ListBuffer()
  val Stencils: ListBuffer[ImplStencil] = new ListBuffer()

  def isinFields(s : String) : Boolean = {
    for (f <- Fields)
      if (f.name.equals(s))
        return true
    return false    
  }
  
   // transformations
  def transformStencils {
    Stencils.clear
    for (e <- exaOperators)
      Stencils ++= e.transform
  }

  def transformFields {
    Fields.clear
    for (e <- exaFields)
      Fields ++= e.transform
  }

  val exaFunctions: ListBuffer[AbstractFunction] = new ListBuffer()
  val exaClasses: ListBuffer[AbstractClass] = new ListBuffer()
  val exaDefinitions: ListBuffer[AbstractDefinition] = new ListBuffer()

  var Functions: collection.mutable.Map[String, ImplFunction] = collection.mutable.Map()
  var GlobalDefines: collection.mutable.Map[String, ParameterInfo] = collection.mutable.Map()

  def transformFunctions {
    for (e <- exaFunctions) yield Functions ++= e.transform(this)
  }

  var callgraph: Map[String, Map[Int, Int]] = Map()

  var ExternalClasses: collection.mutable.Map[String, ImplClass] = collection.mutable.Map()
  var ExternalFunctions: collection.mutable.Map[String, ImplFunction] = collection.mutable.Map()

   def transformFunctions2 {
    for (f <- Functions) {
      for (s <- f._2.body)
        s match {
        case Impl.ImplExternalStatement(s) => 
        case _ =>
      }
    }
  }
}