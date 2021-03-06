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

package exastencils.parsers.l4

import scala.collection.immutable.HashSet
import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.baseExt.l4._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.interfacing.l4.L4_ExternalFieldDecl
import exastencils.logger._

/// L4_Validation

object L4_Validation extends DefaultStrategy("Validate L4 input") {
  // rename identifiers that happen to have the same name as C/C++ keywords or start with "_"
  // identifiers starting with "_" are protected for internal use
  val protectedkeywords = HashSet("alignas", "alignof", "and", "and_eq",
    "asm", "auto", "bitand", "bitor", "bool", "break", "case", "catch", "char", "char16_t", "char32_t", "class", "compl",
    "const", "constexpr", "const_cast", "continue", "decltype", "default", "delete", "do", "double", "dynamic_cast",
    "else", "enum", "explicit", "export", "extern", "false", "float", "for", "friend", "goto", "if", "inline", "int",
    "long", "mutable", "namespace", "new", "noexcept", "not", "not_eq", "nullptr", "operator", "or", "or_eq", "private",
    "protected", "public", "register", "reinterpret_cast", "return", "short", "signed", "sizeof", "static", "static_assert",
    "static_cast", "struct", "switch", "template", "this", "thread_local", "throw", "true", "try", "typedef",
    "typeid", "typename", "union", "unsigned", "using", "virtual", "void", "volatile", "wchar_t", "while", "xor", "xor_eq")

  // No need to transform Domain- and LayoutDeclarationStatements because their names are not printed.
  this += Transformation("EscapeCppKeywordsAndInternalIdentifiers", {
    case x : L4_UnresolvedAccess if protectedkeywords.contains(x.name) && !x.hasAnnotation("NO_PROTECT_THIS") =>
      x.name = "user_" + x.name; x
    case x : L4_UnresolvedAccess if x.name.startsWith("_")                                                    =>
      x.name = "user_" + x.name; x
    case x : L4_ExternalFieldDecl if protectedkeywords.contains(x.name)                                       =>
      x.name = "user_" + x.name; x
    case x : L4_ExternalFieldDecl if x.name.startsWith("_")                                                   =>
      x.name = "user_" + x.name; x
  })

  val functionCalls = ListBuffer[String]()
  val functions = ListBuffer[String]()

  this += Transformation("find Function calls", {
    case f : L4_FunctionCall =>
      f.function match {
        case a : L4_PlainFunctionReference      => functionCalls += a.name
        case a : L4_LeveledFunctionReference    => functionCalls += (a.name + a.level)
        case a : L4_UnresolvedFunctionReference => functionCalls += (a.name + a.level.getOrElse("-1"))
        case _                                  => println("something else: " + f.function)
      }
      f
  })

  this += Transformation("check destroyGlobals", {
    case f : L4_Function if f.name == "Application" =>
      f.body.last match {
        case c : L4_FunctionCall =>
          if (c.function.name != "destroyGlobals")
            Logger.error("destroyGlobals has to be last statement in Application()")
        case _                   =>
      }
      f
  })

  this += Transformation("Check assignment of vectors and matrices", {
    case x : L4_VariableDeclaration if x.datatype.isInstanceOf[L4_VectorDatatype] && x.initialValue.isDefined && x.initialValue.get.isInstanceOf[L4_VectorExpression] =>
      if (x.datatype.asInstanceOf[L4_VectorDatatype].numElements != x.initialValue.get.asInstanceOf[L4_VectorExpression].length)
        Logger.error("Sizes of vectors must match for assignments!")
      x

    case x : L4_VariableDeclaration if x.datatype.isInstanceOf[L4_MatrixDatatype] && x.initialValue.isDefined && x.initialValue.get.isInstanceOf[L4_MatrixExpression] =>
      if (x.datatype.asInstanceOf[L4_MatrixDatatype].numRows != x.datatype.asInstanceOf[L4_MatrixDatatype].numRows || x.datatype.asInstanceOf[L4_MatrixDatatype].numColumns != x.datatype.asInstanceOf[L4_MatrixDatatype].numColumns)
        Logger.error("Sizes of matrices must match for assignments!")
      x
  })
}
