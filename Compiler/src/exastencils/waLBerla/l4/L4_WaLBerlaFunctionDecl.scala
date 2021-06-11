package exastencils.waLBerla.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4.L4_Datatype
import exastencils.base.l4.L4_DeclarationLevelSpecification
import exastencils.base.l4.L4_Function
import exastencils.base.l4.L4_FunctionCollector
import exastencils.base.l4.L4_FunctionDeclLike
import exastencils.base.l4.L4_LevelSpecification
import exastencils.base.l4.L4_Statement
import exastencils.base.l4.L4_UnitDatatype
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

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
    def maxFromLevSpec(lvls : Option[L4_LevelSpecification]) = L4_LevelSpecification.extractLevelListDefEmpty(lvls).max
    val level = if (levels.isEmpty) None else Some(levels.get.resolveLevel)
    var maxLevel = if (levels.isEmpty) None else Some(maxFromLevSpec(levels))
    if (levels.isDefined) {
      val foundFunctions = L4_FunctionCollector.leveledWaLBerlaFunctions.keys.filter(_._1 == name)
      if (foundFunctions.nonEmpty)
        maxLevel = Some(foundFunctions.maxBy(k => k._2)._2)
    }
    L4_WaLBerlaFunction(name, level, maxLevel, datatype, parameters, body)
  }
}
