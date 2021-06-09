package exastencils.waLBerla.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l4.L4_Datatype
import exastencils.base.l4.L4_DeclarationLevelSpecification
import exastencils.base.l4.L4_Function
import exastencils.base.l4.L4_FunctionDeclLike
import exastencils.base.l4.L4_Statement
import exastencils.base.l4.L4_UnitDatatype
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.IR_WaLBerlaLeveledFunction
import exastencils.waLBerla.ir.IR_WaLBerlaPlainFunction

case class L4_WaLBerlaFunctionDecl(
    var name : String,
    var levels : Option[L4_DeclarationLevelSpecification],
    var datatype: L4_Datatype,
    var parameters : ListBuffer[L4_Function.Argument],
    var body : ListBuffer[L4_Statement]
) extends L4_FunctionDeclLike {

  override def allowInlining : Boolean = false

  override def prettyprint(out : PpStream) = {
    out << "waLBerla Function " << name
    if (levels.isDefined) out << '@' << levels.get
    if (parameters.nonEmpty) out << " ( " <<< (parameters, ", ") << " )"
    if (datatype != L4_UnitDatatype) out << " : " << datatype
    out << " {\n" <<< (body, "\n") << "\n}"
  }

  override def progress = Logger.error(s"Trying to progress L4 waLBerla function decl $name; this is not supported")

  override def toFunction = {
    /*
    def maxFromLevSpec(lvls : Option[L4_LevelSpecification]) = L4_LevelSpecification.extractLevelListDefEmpty(lvls).max
    var maxLevel = if (levels.isEmpty) None else Some(maxFromLevSpec(levels))
    if (levels.isDefined) {
      val foundFunctions = L4_FunctionCollector.leveledWaLBerlaFunctions.keys.filter(_._1 == name)
      if (foundFunctions.nonEmpty)
        maxLevel = Some(foundFunctions.maxBy(k => k._2)._2)
    }
    */
    L4_WaLBerlaFunction(name, if (levels.isEmpty) None else Some(levels.get.resolveLevel), datatype, parameters, body)
  }
}

case class L4_WaLBerlaFunction(
    var name : String,
    var level : Option[Int], // level of the function, plain if undefined
    //var maxLevel : Option[Int],
    var datatype: L4_Datatype,
    var parameters : ListBuffer[L4_Function.Argument],
    var body : ListBuffer[L4_Statement]
) extends L4_Function {

  override def prettyprint(out : PpStream) = {
    out << "waLBerla Function " << name << (if (level.isDefined) s"@$level" else "")
    if (parameters.nonEmpty) out << " ( " <<< (parameters, ", ") << " )"
    if (datatype != L4_UnitDatatype) out << " : " << datatype
    out << " {\n" <<< (body, "\n") << "\n}"
  }

  override def progress = ProgressLocation {
    if (level.isDefined)
      IR_WaLBerlaLeveledFunction(name, level.get, datatype.progress, parameters.map(_.progress), body.map(_.progress))
    else
      IR_WaLBerlaPlainFunction(name, datatype.progress, parameters.map(_.progress), body.map(_.progress))
  }
}
