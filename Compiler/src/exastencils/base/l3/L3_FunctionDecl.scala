package exastencils.base.l3

import scala.collection.mutable.ListBuffer

import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_FunctionDecl

object L3_FunctionDecl {
  def apply(name : String, levels : Option[L3_DeclarationLevelSpecification], datatype : Option[L3_Datatype],
      parameters : Option[Option[List[L3_Function.Argument]]], body : List[L3_Statement]) =
    new L3_FunctionDecl(name, levels, datatype.getOrElse(L3_UnitDatatype),
      parameters.getOrElse(Some(List())).getOrElse(List()).to[ListBuffer], body.to[ListBuffer])
}

case class L3_FunctionDecl(
    var name : String,
    var levels : Option[L3_DeclarationLevelSpecification],
    var datatype : L3_Datatype,
    var parameters : ListBuffer[L3_Function.Argument],
    var body : ListBuffer[L3_Statement]) extends L3_Statement {

  override def prettyprint(out : PpStream) = {
    out << "Function " << name
    if (levels.isDefined) out << '@' << levels.get
    if (parameters.nonEmpty) out << " ( " <<< (parameters, ", ") << " )"
    if (datatype != L3_UnitDatatype) out << " : " << datatype
    out << " {\n" <<< (body, "\n") << "\n}"
  }

  override def progress = Logger.error(s"Trying to progress L3 function declaration for $name; this is not supported")

  def unfold = {
    if (levels.isEmpty) Logger.warn(s"Unfolding un-leveled function declaration for $name")

    val levelList = L3_LevelSpecification.extractLevelListDefAll(levels)
    levelList.map(level => {
      val newDecl = Duplicate(this)
      newDecl.levels = Some(L3_SingleLevel(level))
      newDecl
    })
  }

  def toFunction = {
    if (levels.isEmpty)
      L3_PlainFunction(name, datatype, parameters, body)
    else
      L3_LeveledFunction(name, levels.get.resolveLevel, datatype, parameters, body)
  }
}

/// L3_UnfoldKnowledgeDeclarations

object L3_UnfoldFunctionDeclarations extends DefaultStrategy("Unfold leveled L3 function declarations") {
  this += Transformation("Process new declarations", {
    case decl : L3_FunctionDecl if decl.levels.isDefined => decl.unfold
  })
}

/// L3_ProcessFunctionDeclarations

object L3_ProcessFunctionDeclarations extends DefaultStrategy("Process L3 function declarations") {
  this += Transformation("Process function declarations", {
    case decl : L3_FunctionDecl => decl.toFunction
  })
}
