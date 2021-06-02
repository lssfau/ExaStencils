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
import exastencils.waLBerla.ir.IR_WaLBerlaFunctor

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

  override def toFunction = L4_WaLBerlaFunctor(name, if (levels.isEmpty) None else Some(levels.get.resolveLevel), parameters, body)
}

case class L4_WaLBerlaFunctor(
    var name : String,
    var level : Option[Int],
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
    IR_WaLBerlaFunctor(name, level, parameters.map(_.progress), body.map(_.progress))
  }
}
