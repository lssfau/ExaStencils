package exastencils.performance

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.core.StateManager
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge.Knowledge
import exastencils.knowledge.Platform
import exastencils.logger.Logger
import exastencils.multiGrid.MultiGridFunctions
import exastencils.prettyprinting.PpStream

object SIMD_MathFunctions {

  private val numberArgs = Map("exp" -> 1, "exp2" -> 1, "exp10" -> 1, "log" -> 1, "log10" -> 1, "ldexp" -> 2, "pow" -> 2, "sqrt" -> 1,
    "sin" -> 1, "cos" -> 1, "tan" -> 1, "asin" -> 1, "acos" -> 1, "atan" -> 1, "sinh" -> 1, "cosh" -> 1, "tanh" -> 1, "atan2" -> 2)

  private val functionNameMapping = new HashMap[String, String]()
  private lazy val multigridCollection = StateManager.findFirst[MultiGridFunctions].get // there must be a MultiGridFunctions object

  def isAllowed(func : String) : Boolean = {
    return numberArgs.contains(func)
  }

  def addUsage(func : String) : String = {
    val nrArgs = numberArgs(func) // expected fail if !isAllowed(func)
    return functionNameMapping.getOrElseUpdate(func, {
      val funcStmt : AbstractFunctionStatement = new SIMD_MathFunc(func, nrArgs)
      multigridCollection.functions += funcStmt
      funcStmt.name
    })
  }
}

case class SIMD_MathFunc(libmName : String, nrArgs : Int) extends AbstractFunctionStatement(true) {
  override val name : String = "_simd_" + libmName

  override def prettyprint(out : PpStream) : Unit = {
    Platform.simd_mathLibrary match {
      case "none" =>
        scalarFunc(out)

      case "vecmathlib" =>
        val func = "vecmathlib::" + libmName
        val conv = "vecmathlib::realvec<" + RealDatatype.prettyprint() + ", " + Platform.simd_vectorSize + ">"
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

  override def prettyprint_decl() : String = "NOT VALID ; no prototype for " + name + "\n"

  private def scalarFunc(out : PpStream) : Unit = {
    val arrDt = ArrayDatatype(ArrayDatatype(RealDatatype, Platform.simd_vectorSize), nrArgs)
    val aDecls = new VariableDeclarationStatement(arrDt, "a")
    aDecls.alignment = Platform.simd_vectorSize
    def aVAcc(argi : Int) = new ArrayAccess(new VariableAccess(aDecls.name, arrDt), argi)
    def aSAcc(argi : Int, i : Int) = new ArrayAccess(aVAcc(argi), i)
    val args = (0 until nrArgs).map("v" + _)

    out << "static inline " << SIMD_RealDatatype << " " << name << '('
    for (arg <- args)
      out << SIMD_RealDatatype << ' ' << arg << ", "
    out.removeLast(2) // last comma and space
    out << ") {\n"
    out << aDecls << '\n'
    for ((arg, i) <- args.view.zipWithIndex)
      out << new SIMD_StoreStatement(aVAcc(i), new VariableAccess(arg, SIMD_RealDatatype), true) << '\n'
    for (i <- 0 until Platform.simd_vectorSize)
      out << new AssignmentStatement(aSAcc(0, i), new FunctionCallExpression(libmName, (0 until nrArgs).view.map(aSAcc(_, i) : Expression).to[ListBuffer]))
    out << new ReturnStatement(SIMD_LoadExpression(aVAcc(0), true)) << '\n'
    out << '}'
  }

  private def defineMacro(out : PpStream, libFunc : String, conversion : String) : Unit = {
    val args = (0 until nrArgs).map("v" + _)
    out << "#define " << name << '(' << args.mkString(", ") << ")  "
    out << libFunc << '(' << conversion << '(' << args.mkString("), " + conversion + '(') << "))"
  }
}

case object NEONDivision extends AbstractFunctionStatement(true) {
  override def prettyprint(out : PpStream) : Unit = {
    out << s"""static inline float32x4_t ${name}(const float32x4_t &a, const float32x4_t &b) {
  // get an initial estimate of 1/b.
  float32x4_t reciprocal = vrecpeq_f32(b);

  // use a couple Newton-Raphson steps to refine the estimate.  Depending on your
  // application's accuracy requirements, you may be able to get away with only
  // one refinement (instead of the two used here).  Be sure to test!
  reciprocal = vmulq_f32(vrecpsq_f32(b, reciprocal), reciprocal);
  reciprocal = vmulq_f32(vrecpsq_f32(b, reciprocal), reciprocal);

  // and finally, compute a/b = a*(1/b)
  return vmulq_f32(a,reciprocal);
}"""
  }
  override def prettyprint_decl() : String = "NOT VALID ; no prototype for vdivq_f32\n"
  override def name = "vdivq_f32"
}
