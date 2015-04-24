package exastencils.parsers.l4

import exastencils.datastructures._
import exastencils.datastructures.l4.FunctionCallExpression
import exastencils.datastructures.l4.LeveledAccess
import exastencils.datastructures.l4.SingleLevelSpecification
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import exastencils.datastructures.l4.UnresolvedAccess
import exastencils.datastructures.l4.BasicAccess
import exastencils.datastructures.l4.FunctionStatement
import exastencils.datastructures.l4.FunctionCallExpression
import exastencils.logger._
import exastencils.datastructures.l4.FunctionCallStatement




object ValidationL4 {
  val s = DefaultStrategy("Validate L4 Input")
  def apply() = s.apply()
  
  
  var functioncalls = ListBuffer[String]()
  var functions = ListBuffer[String]()

  s += Transformation("find Function calls", {
    case f : FunctionCallExpression => {
      f.identifier match {
        case a : LeveledAccess => functioncalls += (f.identifier.name + a.level.asInstanceOf[SingleLevelSpecification].level)
        case a : UnresolvedAccess => functioncalls += (f.identifier.name + a.level.getOrElse("-1"))
        case a : BasicAccess => functioncalls += (f.identifier.name + "-1")
        case _ => println("something else: " + f.identifier)
      }
      f
    }
  })
  
  
  s += Transformation("check destroyGlobals", {
    case f : FunctionStatement if(f.identifier.name == "Application") => {
      var last = f.statements.last
      last match {
        case c : FunctionCallStatement  => if(c.call.identifier.name != "destroyGlobals") Logger.error("destroyGlobals has to be last statement in Application()")
        case _ =>
      }
      f
    }
  })
  
  
  s.apply()
  
}