package exastencils.performance

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.core.StateManager
import exastencils.knowledge._
import exastencils.logger.Logger
import exastencils.multiGrid.MultiGridFunctions
import exastencils.prettyprinting.PpStream
import exastencils.simd._
import exastencils.util.ir.IR_MathFunctions

object SIMD_MathFunctions {

  private val functionNameMapping = new HashMap[String, String]()
  private lazy val multigridCollection = StateManager.findFirst[MultiGridFunctions].get // there must be a MultiGridFunctions object

  def isAllowed(func : String) : Boolean = {
    return IR_MathFunctions.signatures.contains(func)
  }

  def addUsage(func : String) : String = {
    val nrArgs = IR_MathFunctions.signatures(func)._1.length // expected fail if !isAllowed(func)
    return functionNameMapping.getOrElseUpdate(func, {
      val funcStmt : IR_AbstractFunction = new SIMD_MathFunc(func, nrArgs)
      multigridCollection.functions += funcStmt
      funcStmt.name
    })
  }
}

case class SIMD_MathFunc(libmName : String, nrArgs : Int) extends IR_AbstractFunction(true) {
  override val name : String = "_simd_" + libmName

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
    def aVAcc(argi : Int) = new IR_ArrayAccess(IR_VariableAccess(aDecls.name, arrDt), argi)
    def aSAcc(argi : Int, i : Int) = new IR_ArrayAccess(aVAcc(argi), i)
    val args = (0 until nrArgs).map("v" + _)

    out << "static inline " << IR_SIMD_RealDatatype << " " << name << '('
    for (arg <- args)
      out << IR_SIMD_RealDatatype << ' ' << arg << ", "
    out.removeLast(2) // last comma and space
    out << ") {\n"
    out << aDecls << '\n'
    for ((arg, i) <- args.view.zipWithIndex)
      out << new IR_SIMD_Store(aVAcc(i), IR_VariableAccess(arg, IR_SIMD_RealDatatype), true) << '\n'
    for (i <- 0 until Platform.simd_vectorSize)
      out << new IR_Assignment(aSAcc(0, i), new IR_FunctionCall(libmName, (0 until nrArgs).view.map(aSAcc(_, i) : IR_Expression).to[ListBuffer])) << '\n'
    out << IR_Return(IR_SIMD_Load(aVAcc(0), true)) << '\n'
    out << '}'
  }

  private def defineMacro(out : PpStream, libFunc : String, conversion : String) : Unit = {
    val args = (0 until nrArgs).map("v" + _)
    out << "#define " << name << '(' << args.mkString(", ") << ")  "
    out << libFunc << '(' << conversion << '(' << args.mkString("), " + conversion + '(') << "))"
  }
}

case object NEONDivision extends IR_AbstractFunction(true) {
  override def prettyprint(out : PpStream) : Unit = {
    out <<
      s"""static inline float32x4_t ${ name }(const float32x4_t &a, const float32x4_t &b) {
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
  override def prettyprint_decl() : String = "\n --- NOT VALID ; no prototype for vdivq_f32\n"
  override def name = "vdivq_f32"
}
