package exastencils.base.l3

import scala.collection.mutable._

import exastencils.base.l4._
import exastencils.prettyprinting._

/// L3_FunctionArgument

object L3_FunctionArgument {
  // generate declaration corresponding to given access
  def apply(access : L3_VariableAccess) = new L3_FunctionArgument(access.name, access.datatype)
}

case class L3_FunctionArgument(var name : String, var datatype : L3_Datatype) extends L3_Node with PrettyPrintable with L3_Progressable {
  override def prettyprint(out : PpStream) = out << name << " : " << datatype
  override def progress = L4_FunctionArgument(name, datatype.progress)
}

/// L3_Function

object L3_Function {
  def apply(name : String, levels : Option[L3_DeclarationLevelSpecification], returntype : Option[L3_Datatype], arguments : Option[Option[List[L3_FunctionArgument]]], statements : List[L3_Statement]) : L3_Function =
    L3_Function(name, levels, returntype.getOrElse(L3_UnitDatatype), arguments.getOrElse(Some(List())).getOrElse(List()).to[ListBuffer], statements.to[ListBuffer])
}

// TODO: split into function and function declaration
case class L3_Function(
    var name : String,
    var levels : Option[L3_DeclarationLevelSpecification],
    var returntype : L3_Datatype,
    var arguments : ListBuffer[L3_FunctionArgument],
    var statements : ListBuffer[L3_Statement]) extends L3_Statement {

  override def prettyprint(out : PpStream) = { out << "-- FIXME --" }
  override def progress : L4_Function = {
    L4_Function(
      if (levels.isDefined) L4_LeveledIdentifier(name, levels.get.progress) else L4_BasicIdentifier(name),
      returntype.progress,
      arguments.map(_.progress),
      statements.map(_.progress))
  }
}

/// L3_FunctionCall

object L3_FunctionCall {
  def apply(identifier : L3_Access, arguments : Option[List[L3_Expression]]) =
    new L3_FunctionCall(identifier, arguments.getOrElse(List()).to[ListBuffer])
}

case class L3_FunctionCall(var identifier : L3_Access, var arguments : ListBuffer[L3_Expression]) extends L3_Expression {
  def prettyprint(out : PpStream) = { out << identifier << " ( " <<< (arguments, ", ") << " )" }
  def progress = L4_FunctionCall(identifier.progress, arguments.map(_.progress))
}

/// L3_Return

case class L3_Return(var expr : Option[L3_Expression]) extends L3_Statement {
  override def prettyprint(out : PpStream) = {
    out << "return"
    if (expr.isDefined) out << ' ' << expr.get.prettyprint()
  }

  override def progress : L4_Return = {
    L4_Return(expr.map(_.progress))
  }
}

///// L3_UnfoldLeveledFunctions
//
//object L3_UnfoldLeveledFunctions extends DefaultStrategy("Unfold leveled functions") {
//  this += Transformation("Process unresolved field and operator accesses", {
//    case access : L3_UnresolvedAccess if access.level.isEmpty => {
//      // check if access without level is a field
//      if (L3_FieldCollection.exists(access.name))
//        access.level = Some(L3_SingleLevelSpecification(baseLevel))
//      // ... or an operator
//      else if (OperatorCollection.exists(access.name))
//        access.level = Some(L3_SingleLevelSpecification(baseLevel))
//      // ... or a function
//      else if (StateManager.findFirst({ fct : L3_Function => access.name == fct.name && fct.levels.isDefined }).isDefined)
//        access.level = Some(L3_SingleLevelSpecification(baseLevel))
//
//      access
//    }
//  })
//
//  this += Transformation("Duplicate function statements", {
//    case fct : L3_Function if fct.levels.isDefined => {
//      fct.levels.get match {
//        case list : ListLevelSpecification =>
//          var unfoldedFcts = ListBuffer[L3_Function]()
//          for (level <- list.levels.toList.sortBy(_.asInstanceOf[SingleLevelSpecification].level)) {
//            var newFct = Duplicate(fct)
//            newFct.levels = Some(level)
//            HandleLocalLevelSpecifiers.baseLevel = level.asInstanceOf[SingleLevelSpecification].level
//            HandleLocalLevelSpecifiers.applyStandalone(newFct)
//            unfoldedFcts += newFct
//          }
//          unfoldedFcts
//
//        case level : SingleLevelSpecification =>
//          HandleLocalLevelSpecifiers.baseLevel = level.level
//          HandleLocalLevelSpecifiers.applyStandalone(fct)
//          fct
//
//        case level =>
//          Logger.warn(s"Found l3 function with invalid level specification $level")
//          fct
//      }
//    }
//  })
//
//  object HandleLocalLevelSpecifiers extends QuietDefaultStrategy("Handle local level specifiers") {
//    var baseLevel : Int = -1
//    this += Transformation("Process current, coarser and finer", {
//      case L3_CurrentLevelSpecification => L3_SingleLevelSpecification(baseLevel)
//      case L3_CoarserLevel              => L3_SingleLevelSpecification(baseLevel - 1)
//      case L3_FinerLevelSpecification   => L3_SingleLevelSpecification(baseLevel + 1)
//    })
//  }
//
//}
