package exastencils.simd

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_MathFunctions

/// SIMD_MathFunctions

object SIMD_MathFunctions {

  private val functionNameMapping = new HashMap[String, String]()
  private lazy val functions = IR_UserFunctions.get // there must be a IR_UserFunctions object

  def isAllowed(func : String) : Boolean = {
    IR_MathFunctions.signatures.contains(func)
  }

  def addUsage(func : String) : String = {
    val nrArgs = IR_MathFunctions.signatures(func)._1.length // expected fail if !isAllowed(func)
    functionNameMapping.getOrElseUpdate(func, {
      val funcStmt : IR_FunctionLike = SIMD_MathFunc(func, nrArgs)
      functions += funcStmt
      funcStmt.name
    })
  }
}

/// SIMD_MathFunc

case class SIMD_MathFunc(libmName : String, nrArgs : Int) extends IR_FunctionLike {
  override var name : String = "_simd_" + libmName
  isHeaderOnly = true

  override def prettyprint(out : PpStream) : Unit = {
    Platform.simd_mathLibrary match {
      case "none" =>
        scalarFunc(out)

      case "vecmathlib" =>
        val func = "vecmathlib::" + libmName
        val conv = "vecmathlib::realvec<" + IR_RealDatatype.prettyprint() + ", " + Platform.simd_vectorSize + ">"
        defineMacro(out, func, conv)

      case "vectorclass" =>
        val func = "VCL_NAMESPACE::" + libmName
        val conv = "VCL_NAMESPACE::Vec" + Platform.simd_vectorSize + (if (Knowledge.useDblPrecision) "d" else "f")
        defineMacro(out, func, conv)

      case "svml" =>
        val prec = if (Knowledge.useDblPrecision) 'd' else 's'
        val func =
          Platform.simd_instructionSet match {
            case "SSE3"         => "_mm_" + libmName + "_p" + prec
            case "AVX" | "AVX2" => "_mm256_" + libmName + "_p" + prec
            case "AVX512"       => "_mm512_" + libmName + "_p" + prec
            case _              => Logger.error("intel svml only supports SSE* and AVX*")
          }
        val conv = "" // no conversion required
        defineMacro(out, func, conv)

      case "mass_simd" =>
        val func = libmName + (if (Knowledge.useDblPrecision) "d" else "f") + "4"
        val conv = "" // no conversion required
        defineMacro(out, func, conv)
    }
  }

  override def prettyprint_decl() : String = "\n --- NOT VALID ; no prototype for " + name + "\n"

  private def scalarFunc(out : PpStream) : Unit = {
    val arrDt = IR_ArrayDatatype(IR_ArrayDatatype(IR_RealDatatype, Platform.simd_vectorSize), nrArgs)
    val aDecls = IR_VariableDeclaration(arrDt, "a")
    aDecls.alignment = Platform.simd_vectorSize
    def aVAcc(argi : Int) = IR_ArrayAccess(IR_VariableAccess(aDecls.name, arrDt), argi)
    def aSAcc(argi : Int, i : Int) = IR_ArrayAccess(aVAcc(argi), i)
    val args = (0 until nrArgs).map("v" + _)

    out << "static inline " << SIMD_RealDatatype << " " << name << '('
    for (arg <- args)
      out << SIMD_RealDatatype << ' ' << arg << ", "
    out.removeLast(2) // last comma and space
    out << ") {\n"
    out << aDecls << '\n'
    for ((arg, i) <- args.view.zipWithIndex)
      out << SIMD_Store(aVAcc(i), IR_VariableAccess(arg, SIMD_RealDatatype), true) << '\n'
    for (i <- 0 until Platform.simd_vectorSize)
      out << IR_Assignment(aSAcc(0, i), IR_FunctionCall(libmName, (0 until nrArgs).view.map(aSAcc(_, i) : IR_Expression).to[ListBuffer])) << '\n'
    out << IR_Return(SIMD_Load(aVAcc(0), true)) << '\n'
    out << '}'
  }

  private def defineMacro(out : PpStream, libFunc : String, conversion : String) : Unit = {
    val args = (0 until nrArgs).map("v" + _)
    out << "#define " << name << '(' << args.mkString(", ") << ")  "
    out << libFunc << '(' << conversion << '(' << args.mkString("), " + conversion + '(') << "))"
  }
}
