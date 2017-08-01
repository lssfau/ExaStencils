package exastencils.base.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ExaRootNode
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_FunctionDecl

object L4_FunctionDecl {
  def apply(name : String, levels : Option[L4_DeclarationLevelSpecification], datatype : Option[L4_Datatype],
      parameters : Option[Option[List[L4_Function.Argument]]], body : List[L4_Statement], allowInlining : Boolean) =
    new L4_FunctionDecl(name, levels, datatype.getOrElse(L4_UnitDatatype),
      parameters.getOrElse(Some(List())).getOrElse(List()).to[ListBuffer], body.to[ListBuffer], allowInlining)
}

case class L4_FunctionDecl(
    var name : String,
    var levels : Option[L4_DeclarationLevelSpecification],
    var datatype : L4_Datatype,
    var parameters : ListBuffer[L4_Function.Argument],
    var body : ListBuffer[L4_Statement],
    var allowInlining : Boolean = true) extends L4_Statement {

  override def prettyprint(out : PpStream) = {
    out << "Function " << name
    if (levels.isDefined) out << '@' << levels.get
    if (parameters.nonEmpty) out << " ( " <<< (parameters, ", ") << " )"
    if (datatype != L4_UnitDatatype) out << " : " << datatype
    out << " {\n" <<< (body, "\n") << "\n}"
  }

  override def progress = Logger.error(s"Trying to progress L4 function declaration for $name; this is not supported")

  def unfold = {
    if (levels.isEmpty) Logger.warn(s"Unfolding un-leveled function declaration for $name")

    val levelList = L4_LevelSpecification.extractLevelListDefAll(levels)
    levelList.map(level => {
      val newDecl = Duplicate(this)
      newDecl.levels = Some(L4_SingleLevel(level))
      newDecl
    })
  }

  def toFunction = {
    if (levels.isEmpty)
      L4_PlainFunction(name, datatype, parameters, body)
    else
      L4_LeveledFunction(name, levels.get.resolveLevel, datatype, parameters, body)
  }
}

/// L4_UnfoldKnowledgeDeclarations

object L4_UnfoldFunctionDeclarations extends DefaultStrategy("Unfold leveled L4 function declarations") {
  this += Transformation("Process new declarations", {
    case decl : L4_FunctionDecl if decl.levels.isDefined => decl.unfold
  })
}

/// L4_ProcessFunctionDeclarations

object L4_ProcessFunctionDeclarations extends DefaultStrategy("Process L4 function declarations") {
  this += Transformation("Process function declarations", {
    case decl : L4_FunctionDecl => decl.toFunction
  })
}

/// L4_ReplaceLevelsInFunctions

object L4_ReplaceLevelsInFunctionDecls extends DefaultStrategy("Replace explicit levels with current, coarser and finer in functions") {
  this += new Transformation("Replace levels", {
    case fct @ L4_FunctionDecl(_, Some(L4_SingleLevel(level)), _, _, body, _) =>
      L4_ReplaceExplicitLevelsWithCurrent.curLevel = level
      L4_ReplaceExplicitLevelsWithCurrent.applyStandalone(L4_Scope(body))
      fct
  })
}

/// L4_CombineLeveledFunctions

object L4_CombineLeveledFunctionDecls extends DefaultStrategy("Combine single functions into leveled functions") {
  var functions = ListBuffer[(L4_FunctionDecl, Int)]()

  override def apply(applyAtNode : Option[Node]) = {
    functions.clear

    // gather all functions
    super.apply(applyAtNode)

    // re-add combined functions
    val combinedFcts = ListBuffer[L4_FunctionDecl]()
    while (functions.nonEmpty) {
      val (curFct, curLevel) = functions.remove(0)

      val levels = ListBuffer[Int]()
      levels += curLevel
      // only regard functions with the same name
      for ((cmpFct, cmpLevel) <- functions.filter(_._1.name == curFct.name)) {
        if (cmpFct.body.map(_.prettyprint()).mkString("\n") == curFct.body.map(_.prettyprint()).mkString("\n")
          && cmpFct.parameters.map(_.prettyprint()).mkString(", ") == curFct.parameters.map(_.prettyprint()).mkString(", ")) {
          levels += cmpLevel
          functions -= ((cmpFct, cmpLevel))
        }
      }

      curFct.levels = Some(L4_LevelList(levels.sorted.map(L4_SingleLevel).toList))
      combinedFcts += curFct
    }

    ExaRootNode.l4_root.nodes ++= combinedFcts
  }

  this += new Transformation("Gather functions", {
    case fct @ L4_FunctionDecl(_, Some(L4_SingleLevel(level)), _, _, _, _) =>
      functions += ((fct, level))
      None // remove fct
  })
}
