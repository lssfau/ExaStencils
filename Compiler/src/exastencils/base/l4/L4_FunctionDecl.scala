//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

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


trait L4_FunctionDeclLike extends L4_Statement {
  def name : String
  def datatype : L4_Datatype
  def parameters : ListBuffer[L4_Function.Argument]
  def body : ListBuffer[L4_Statement]
  def allowInlining : Boolean

  var levels : Option[L4_DeclarationLevelSpecification]

  def unfold : List[L4_FunctionDeclLike] = {
    if (levels.isEmpty) Logger.warn(s"Unfolding un-leveled function declaration for $name")

    val levelList = L4_LevelSpecification.extractLevelListDefAll(levels)
    levelList.map(level => {
      val newDecl = Duplicate(this)
      newDecl.levels = Some(L4_SingleLevel(level))
      newDecl
    })
  }

  def toFunction : L4_Statement
}

case class L4_FunctionDecl(
    var name : String,
    var levels : Option[L4_DeclarationLevelSpecification],
    var datatype : L4_Datatype,
    var parameters : ListBuffer[L4_Function.Argument],
    var body : ListBuffer[L4_Statement],
    var allowInlining : Boolean = true) extends L4_FunctionDeclLike {

  override def prettyprint(out : PpStream) = {
    out << (if (!allowInlining) "noinline " else "") << "Function " << name
    if (levels.isDefined) out << '@' << levels.get
    if (parameters.nonEmpty) out << " ( " <<< (parameters, ", ") << " )"
    if (datatype != L4_UnitDatatype) out << " : " << datatype
    out << " {\n" <<< (body, "\n") << "\n}"
  }

  if (L4_SpecialFunctionReferences.contains(name))
    Logger.error(s"Function name $name is already taken. Please use another function name.")

  override def progress = Logger.error(s"Trying to progress L4 function declaration for $name; this is not supported")

  def toFunction = {
    if (levels.isEmpty)
      L4_PlainFunction(name, datatype, parameters, body, allowInlining)
    else
      L4_LeveledFunction(name, levels.get.resolveLevel, datatype, parameters, body, allowInlining)
  }
}

/// L4_UnfoldKnowledgeDeclarations

object L4_UnfoldFunctionDeclarations extends DefaultStrategy("Unfold leveled L4 function declarations") {
  this += Transformation("Process new declarations", {
    case decl : L4_FunctionDeclLike if decl.levels.isDefined => decl.unfold
  })
}

/// L4_ProcessFunctionDeclarations

object L4_ProcessFunctionDeclarations extends DefaultStrategy("Process L4 function declarations") {
  this += Transformation("Process function declarations", {
    case decl : L4_FunctionDeclLike => decl.toFunction
  })
}

/// L4_ReplaceLevelsInFunctions

object L4_ReplaceLevelsInFunctionDecls extends DefaultStrategy("Replace explicit levels with current, coarser and finer in functions") {
  this += new Transformation("Replace levels", {
    case fct : L4_FunctionDeclLike if fct.levels.isDefined && fct.levels.get.isInstanceOf[L4_SingleLevel] =>
      L4_ReplaceExplicitLevelsWithCurrent.curLevel = fct.levels.get.asInstanceOf[L4_SingleLevel].level
      L4_ReplaceExplicitLevelsWithCurrent.applyStandalone(L4_Scope(fct.body))
      fct
  })
}

/// L4_CombineLeveledFunctions

object L4_CombineLeveledFunctionDecls extends DefaultStrategy("Combine single functions into leveled functions") {
  var functions = ListBuffer[(L4_FunctionDeclLike, Int)]()

  override def apply(applyAtNode : Option[Node]) = {
    functions.clear

    // gather all functions
    super.apply(applyAtNode)

    // re-add combined functions
    val combinedFcts = ListBuffer[L4_FunctionDeclLike]()
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
    case fct : L4_FunctionDeclLike if fct.levels.isDefined && fct.levels.get.isInstanceOf[L4_SingleLevel] =>
      functions += ((fct, fct.levels.get.asInstanceOf[L4_SingleLevel].level))
      None // remove fct
  })
}
