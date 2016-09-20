package exastencils.parsers.l4

import scala.collection.immutable.HashSet
import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.baseExt.l4._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.interfacing.l4.L4_ExternalFieldDecl
import exastencils.logger._

object ValidationL4 {
  val s = DefaultStrategy("Validate L4 Input")
  def apply() = s.apply()

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

  // No need to transform Domain- and LayoutDeclarationStatements because their names are not outputted.
  s += Transformation("EscapeCppKeywordsAndInternalIdentifiers", {
    case x : Identifier if (protectedkeywords.contains(x.name))                                              =>
      x.name = "user_" + x.name; x
    case x : Identifier if (x.name.startsWith("_"))                                                          =>
      x.name = "user_" + x.name; x
    case x : UnresolvedAccess if (protectedkeywords.contains(x.name) && !x.hasAnnotation("NO_PROTECT_THIS")) =>
      x.name = "user_" + x.name; x
    case x : UnresolvedAccess if (x.name.startsWith("_"))                                                    =>
      x.name = "user_" + x.name; x
    case x : L4_ExternalFieldDecl if (protectedkeywords.contains(x.identifier))                              =>
      x.identifier = "user_" + x.identifier; x
    case x : L4_ExternalFieldDecl if (x.identifier.startsWith("_"))                                          =>
      x.identifier = "user_" + x.identifier; x
  })

  var functioncalls = ListBuffer[String]()
  var functions = ListBuffer[String]()

  s += Transformation("find Function calls", {
    case f : L4_FunctionCall => {
      f.identifier match {
        case a : LeveledAccess    => functioncalls += (f.identifier.name + a.level.asInstanceOf[SingleLevelSpecification].level)
        case a : UnresolvedAccess => functioncalls += (f.identifier.name + a.level.getOrElse("-1"))
        case a : BasicAccess      => functioncalls += (f.identifier.name + "-1")
        case _                    => println("something else: " + f.identifier)
      }
      f
    }
  })

  s += Transformation("check destroyGlobals", {
    case f : L4_Function if (f.identifier.name == "Application") => {
      var last = f.statements.last
      last match {
        case c : L4_FunctionCall => if (c.identifier.name != "destroyGlobals") Logger.error("destroyGlobals has to be last statement in Application()")
        case _                   =>
      }
      f
    }
  })

  s += Transformation("Check assignment of vectors and matrices", {
    case x : ValueDeclarationStatement if (x.datatype.isInstanceOf[L4_VectorDatatype] && x.expression.isInstanceOf[VectorExpression]) =>
      if (x.datatype.asInstanceOf[L4_VectorDatatype].numElements != x.expression.asInstanceOf[VectorExpression].length) Logger.error("Sizes of vectors must match for assignments!"); x
    case x : ValueDeclarationStatement if (x.datatype.isInstanceOf[L4_MatrixDatatype] && x.expression.isInstanceOf[MatrixExpression]) =>
      if (x.datatype.asInstanceOf[L4_MatrixDatatype].numRows != x.datatype.asInstanceOf[L4_MatrixDatatype].numRows || x.datatype.asInstanceOf[L4_MatrixDatatype].numColumns != x.datatype.asInstanceOf[L4_MatrixDatatype].numColumns) Logger.error("Sizes of matrices must match for assignments!"); x
    case x : VariableDeclarationStatement if (x.datatype.isInstanceOf[L4_VectorDatatype])                                             =>
      if (x.expression.isDefined && x.expression.get.isInstanceOf[VectorExpression] && x.expression.get.asInstanceOf[VectorExpression].length != x.expression.get.asInstanceOf[VectorExpression].length) Logger.error("Sizes of vectors must match for assignments!"); x
    case x : VariableDeclarationStatement /*(_, mat : L4_MatrixDatatype, exp)*/ if (x.datatype.isInstanceOf[L4_MatrixDatatype])       =>
      if (x.expression.isDefined && x.expression.get.isInstanceOf[MatrixExpression] && (x.datatype.asInstanceOf[L4_MatrixDatatype].numRows != x.expression.get.asInstanceOf[MatrixExpression].rows || x.datatype.asInstanceOf[L4_MatrixDatatype].numColumns != x.expression.get.asInstanceOf[MatrixExpression].columns)) Logger.error("Sizes of matrices must match for assignments!"); x
  })

  s.apply()

}
