package exastencils.waLBerla.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ExaRootNode
import exastencils.base.ProgressLocation
import exastencils.base.l4.L4_Datatype
import exastencils.base.l4.L4_DeclarationLevelSpecification
import exastencils.base.l4.L4_Function
import exastencils.base.l4.L4_FunctionDeclLike
import exastencils.base.l4.L4_LevelSpecification
import exastencils.base.l4.L4_Statement
import exastencils.base.l4.L4_UnitDatatype
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.IR_WaLBerlaLeveledFunctor
import exastencils.waLBerla.ir.IR_WaLBerlaPlainFunctor

case class L4_WaLBerlaFunctorDecl(
    var name : String,
    var levels : Option[L4_DeclarationLevelSpecification],
    var parameters : ListBuffer[L4_Function.Argument],
    var body : ListBuffer[L4_Statement]
) extends L4_FunctionDeclLike {

  override def allowInlining : Boolean = false

  override def prettyprint(out : PpStream) = {
    out << "waLBerla Functor " << name
    if (levels.isDefined) out << '@' << levels.get
    if (parameters.nonEmpty) out << " ( " <<< (parameters, ", ") << " )"
    out << " {\n" <<< (body, "\n") << "\n}"
  }

  override def progress = Logger.error(s"Trying to progress L4 waLBerla functor decl $name; this is not supported")

  override def datatype : L4_Datatype = L4_UnitDatatype

  override def toFunction = {
    def maxFromLevSpec(lvls : Option[L4_LevelSpecification]) = L4_LevelSpecification.extractLevelListDefEmpty(lvls).max
    var maxLevel = maxFromLevSpec(levels)

    // TODO check cost of this step
    var time = System.nanoTime()
    if (levels.isDefined) {
      ExaRootNode.l4_root.nodes.foreach {
        case decl : L4_WaLBerlaFunctorDecl if decl.name == this.name =>
          if (decl.levels.isDefined)
            maxLevel = math.max(maxLevel, maxFromLevSpec(decl.levels))
        case _ =>
      }
    }
    time = (System.nanoTime() - time) / 100000
    Logger.debug("transformationtimer;" + "L4FunctorToFunction" + ";" + "Get max level for functor decl" + ";" + time + "\\\\")

    L4_WaLBerlaFunctor(name,
      if (levels.isEmpty) None else Some(levels.get.resolveLevel),
      if (levels.isEmpty) None else Some(maxLevel),
      parameters,
      body
    )
  }
}

case class L4_WaLBerlaFunctor(
    var name : String,
    var level : Option[Int], // level of the functor, plain if undefined
    var maxLevel : Option[Int],
    var parameters : ListBuffer[L4_Function.Argument],
    var body : ListBuffer[L4_Statement]
) extends L4_Function {

  override def datatype : L4_Datatype = L4_UnitDatatype

  override def prettyprint(out : PpStream) = {
    out << "waLBerla Functor " << name << (if (level.isDefined) s"@$level" else "")
    if (parameters.nonEmpty) out << " ( " <<< (parameters, ", ") << " )"
    out << " {\n" <<< (body, "\n") << "\n}"
  }

  override def progress = ProgressLocation {
    if (level.isDefined && maxLevel.isDefined)
      IR_WaLBerlaLeveledFunctor(name, level.get, maxLevel.get, parameters.map(_.progress), body.map(_.progress))
    else if (level.isEmpty && maxLevel.isEmpty)
      IR_WaLBerlaPlainFunctor(name, parameters.map(_.progress), body.map(_.progress))
    else
      Logger.error("Invalid functor level and max level.")
  }
}
