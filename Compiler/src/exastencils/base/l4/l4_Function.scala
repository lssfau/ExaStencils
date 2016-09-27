package exastencils.base.l4

import scala.collection.mutable._

import exastencils.base.ir.{ IR_FunctionCall, _ }
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L4_FunctionArgument

case class L4_FunctionArgument(var name : String, var datatype : L4_Datatype) extends L4_Node with PrettyPrintable with L4_Progressable {
  override def prettyprint(out : PpStream) = out << name << " : " << datatype
  override def progress = IR_FunctionArgument(name, datatype.progress)
}

/// L4_Function

object L4_Function {
  def apply(identifier : L4_Identifier, returntype : L4_Datatype, arguments : List[L4_FunctionArgument], statements : List[L4_Statement], allowInlining : Boolean) =
    new L4_Function(identifier, returntype, arguments.to[ListBuffer], statements.to[ListBuffer], allowInlining)
}

case class L4_Function(
    override var identifier : L4_Identifier,
    var returntype : L4_Datatype,
    var arguments : ListBuffer[L4_FunctionArgument],
    var statements : ListBuffer[L4_Statement],
    var allowInlining : Boolean = true) extends L4_Statement with L4_HasIdentifier {

  override def prettyprint(out : PpStream) = {
    out << "Function " << identifier << " (" <<< (arguments, ", ") << " )" << " : " << returntype << " {\n"
    out <<< statements
    out << "}\n"
  }

  override def progress = IR_Function(returntype.progress, identifier.fullName, arguments.map(s => s.progress), statements.map(s => s.progress), allowInlining)
}

/// L4_FunctionCall

object L4_FunctionCall {
  def apply(function : L4_Access, arguments : L4_Expression*) = new L4_FunctionCall(function, arguments.to[ListBuffer])
}

case class L4_FunctionCall(var function : L4_Access, var arguments : ListBuffer[L4_Expression]) extends L4_Expression {
  def prettyprint(out : PpStream) = out << function << " ( " <<< (arguments, ", ") << " )"
  def progress : IR_FunctionCall = {
    function match {
      case access : L4_FunctionAccess   =>
        IR_FunctionCall(access.progress, arguments.map(s => s.progress))
      case access : L4_UnresolvedAccess =>
        Logger.warn("Found function call without resolved access " + access.name)
        // FIXME: access.name
        IR_FunctionCall(access.name, arguments.map(s => s.progress))
    }
  }
}

/// L4_Return

case class L4_Return(var expr : Option[L4_Expression]) extends L4_Statement {
  override def prettyprint(out : PpStream) = {
    out << "return"
    if (expr.isDefined) out << ' ' << expr.get.prettyprint()
    out << '\n'
  }

  override def progress = IR_Return(L4_ProgressOption(expr)(_.progress))
}

/// L4_UnfoldLeveledFunctions

object L4_UnfoldLeveledFunctions extends DefaultStrategy("Unfold leveled functions") {
  var functions = HashSet[(String, Integer)]()

  // find all functions that are defined with an explicit level specification
  this += new Transformation("Find explicitly leveled functions", {
    case function @ L4_Function(L4_LeveledIdentifier(fctName, L4_SingleLevel(level)), _, _, _, _) =>
      functions += ((fctName, level))
      function
  })

  // unfold function declarations
  this += new Transformation("Unfold leveled Function declarations", {
    case decl @ L4_Function(L4_LeveledIdentifier(_, levels), _, _, _, _) => doDuplicate(decl, levels)
  })

  def doDuplicate(toDuplicate : L4_Function, level : L4_LevelSpecification) : ListBuffer[L4_Function] = {
    def duplicateInstance(newLevel : L4_LevelSpecification) = {
      val newInstance = Duplicate(toDuplicate)
      newInstance.identifier = L4_LeveledIdentifier(newInstance.identifier.name, newLevel)
      newInstance
    }

    var duplicated = ListBuffer[L4_Function]()
    level match {
      case level @ (L4_SingleLevel(_) | L4_CurrentLevel | L4_CoarserLevel | L4_FinerLevel) =>
        duplicated += duplicateInstance(level)
      case level : L4_LevelList                                                            =>
        level.levels.foreach(level => duplicated ++= doDuplicate(toDuplicate, level))
      case level : L4_LevelRange                                                           =>
        val (begin, end) = (level.begin.resolveLevel, level.end.resolveLevel)
        for (level <- math.min(begin, end) to math.max(begin, end))
          if (!functions.contains(toDuplicate.identifier.name, level)) {
            duplicated += duplicateInstance(L4_SingleLevel(level))
            functions += ((toDuplicate.identifier.name, level))
          } else {
            Logger.warn("Potentially shadowed function " + toDuplicate.identifier.name + " on level " + level)
          }
      case _                                                                               =>
        Logger.error(s"Invalid level specification for Value $toDuplicate: $level")
    }

    duplicated
  }
}
