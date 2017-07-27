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

object L4_Validation {
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
    case x : L4_Identifier if protectedkeywords.contains(x.name)                                              =>
      x.name = "user_" + x.name; x
    case x : L4_Identifier if x.name.startsWith("_")                                                          =>
      x.name = "user_" + x.name; x
    case x : L4_UnresolvedAccess if protectedkeywords.contains(x.name) && !x.hasAnnotation("NO_PROTECT_THIS") =>
      x.name = "user_" + x.name; x
    case x : L4_UnresolvedAccess if x.name.startsWith("_")                                                    =>
      x.name = "user_" + x.name; x
    case x : L4_ExternalFieldDecl if protectedkeywords.contains(x.name)                                       =>
      x.name = "user_" + x.name; x
    case x : L4_ExternalFieldDecl if x.name.startsWith("_")                                                   =>
      x.name = "user_" + x.name; x
  })

  var functionCalls = ListBuffer[String]()
  var functions = ListBuffer[String]()

  s += Transformation("find Function calls", {
    case f : L4_FunctionCall =>
      f.function match {
        case a : L4_PlainFunctionReference      => functionCalls += a.name
        case a : L4_LeveledFunctionReference    => functionCalls += (a.name + a.level)
        case a : L4_UnresolvedFunctionReference => functionCalls += (a.name + a.level.getOrElse("-1"))
        case _                                  => println("something else: " + f.function)
      }
      f
  })

  s += Transformation("check destroyGlobals", {
    case f : L4_Function if f.name == "Application" =>
      var last = f.body.last
      last match {
        case c : L4_FunctionCall => if (c.function.name != "destroyGlobals") Logger.error("destroyGlobals has to be last statement in Application()")
        case _                   =>
      }
      f
  })

  s += Transformation("Check assignment of vectors and matrices", {
    case x : L4_VariableDeclaration if x.datatype.isInstanceOf[L4_VectorDatatype] && x.initialValue.isDefined && x.initialValue.get.isInstanceOf[L4_VectorExpression] =>
      if (x.datatype.asInstanceOf[L4_VectorDatatype].numElements != x.initialValue.get.asInstanceOf[L4_VectorExpression].length)
        Logger.error("Sizes of vectors must match for assignments!");
      x

    case x : L4_VariableDeclaration if x.datatype.isInstanceOf[L4_MatrixDatatype] && x.initialValue.isDefined && x.initialValue.get.isInstanceOf[L4_MatrixExpression] =>
      if (x.datatype.asInstanceOf[L4_MatrixDatatype].numRows != x.datatype.asInstanceOf[L4_MatrixDatatype].numRows || x.datatype.asInstanceOf[L4_MatrixDatatype].numColumns != x.datatype.asInstanceOf[L4_MatrixDatatype].numColumns)
        Logger.error("Sizes of matrices must match for assignments!");
      x
  })

  s.apply()

}
