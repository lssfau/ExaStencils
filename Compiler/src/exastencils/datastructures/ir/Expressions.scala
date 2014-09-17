package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._

trait Expression extends Node with CppPrettyPrintable {
  def ~(exp : Expression) : ConcatenationExpression = {
    new ConcatenationExpression(ListBuffer(this, exp))
  }
  def ~~(exp : Expression) : SpacedConcatenationExpression = {
    new SpacedConcatenationExpression(ListBuffer(this, exp))
  }

  import BinaryOperators._
  def +(other : Expression) = new AdditionExpression(this, other)
  def -(other : Expression) = new SubtractionExpression(this, other)
  def *(other : Expression) = new MultiplicationExpression(this, other)
  def /(other : Expression) = new DivisionExpression(this, other)
  def Pow(other : Expression) = new PowerExpression(this, other)
  def Mod(other : Expression) = new ModuloExpression(this, other)
  def Modulo(other : Expression) = new ModuloExpression(this, other)
  def And(other : Expression) = new AndAndExpression(this, other)
  def AndAnd(other : Expression) = new AndAndExpression(this, other)
  def Or(other : Expression) = new OrOrExpression(this, other)
  def OrOr(other : Expression) = new OrOrExpression(this, other)
  def EqEq(other : Expression) = new EqEqExpression(this, other)
  def IsNeq(other : Expression) = new NeqNeqExpression(this, other)
  def <(other : Expression) = new LowerExpression(this, other)
  def <=(other : Expression) = new LowerEqualExpression(this, other)
  def >(other : Expression) = new GreaterExpression(this, other)
  def >=(other : Expression) = new GreaterEqualExpression(this, other)
}

object BinaryOperators extends Enumeration {
  type BinaryOperators = Value
  val Addition = Value("+")
  val Subtraction = Value("-")
  val Multiplication = Value("*")
  val Division = Value("/")
  val Power = Value("**") // FIXME
  val Modulo = Value("%")
  
  val AndAnd = Value("&&")
  val OrOr = Value("||")
  val Negation = Value("!")
  val EqEq = Value("==")
  val NeqNeq = Value("!=")
  val Lower = Value("<")
  val LowerEqual = Value("<=")
  val Greater = Value(">")
  val GreaterEqual = Value(">=")
  val BitwiseAnd = Value("&")

  exastencils.core.Duplicate.registerImmutable(this.getClass())

  //  Conversions for Enumeration:
  // BinaryOperators -> String:  op.toString()
  // String -> BinaryOperators:  BinaryOperators.withName(op)

  def CreateExpression(op : String, left : Expression, right : Expression) : Expression = CreateExpression(withName(op), left, right)
  def CreateExpression(op : Value, left : Expression, right : Expression) : Expression = op match {
    case Addition       => return new AdditionExpression(left, right)
    case Subtraction    => return new SubtractionExpression(left, right)
    case Multiplication => return new MultiplicationExpression(left, right)
    case Division       => return new DivisionExpression(left, right)
    case Power          => return new PowerExpression(left, right)
    case Modulo         => return new ModuloExpression(left, right)
    
    case AndAnd         => return new AndAndExpression(left, right)
    case OrOr           => return new OrOrExpression(left, right)
    case Negation       => return new NegationExpression(left)
    case EqEq           => return new EqEqExpression(left, right)
    case NeqNeq         => return new NeqNeqExpression(left, right)
    case Lower          => return new LowerExpression(left, right)
    case LowerEqual     => return new LowerEqualExpression(left, right)
    case Greater        => return new GreaterExpression(left, right)
    case GreaterEqual   => return new GreaterEqualExpression(left, right)
    case BitwiseAnd     => return new BitwiseAndExpression(left, right)
  }
}

object UnaryOperators extends Enumeration {
  type UnaryOperators = Value
  val Positive = Value("")
  val Negative = Value("-")
  val Not = Value("!")
  val AddressOf = Value("&")
  val BitwiseNegation = Value("~")

  exastencils.core.Duplicate.registerImmutable(this.getClass())
}

trait Access extends Expression
trait Number extends Expression {
  def value : AnyVal
}

case object NullExpression extends Expression {
  exastencils.core.Duplicate.registerConstant(this)
  override def cpp(out : CppStream) : Unit = ()
}

case class ConcatenationExpression(var expressions : ListBuffer[Expression]) extends Expression {
  override def cpp(out : CppStream) : Unit = out <<< expressions

  override def ~(exp : Expression) : ConcatenationExpression = {
    expressions += exp
    this
  }
}

case class SpacedConcatenationExpression(var expressions : ListBuffer[Expression]) extends Expression {
  override def cpp(out : CppStream) : Unit = out <<< (expressions, " ")

  override def ~~(exp : Expression) : SpacedConcatenationExpression = {
    expressions += exp
    this
  }
}

case class StringConstant(var value : String) extends Expression {
  override def cpp(out : CppStream) : Unit = out << value
}

case class IntegerConstant(var v : Long) extends Number {
  override def cpp(out : CppStream) : Unit = out << v
  override def value = v
}

case class FloatConstant(var v : Double) extends Number {
  override def cpp(out : CppStream) : Unit = {
    out << String.format(java.util.Locale.US, "%e", Double.box(value)) // ensure the compiler can parse the string
  }

  override def value = v
}

case class BooleanConstant(var value : Boolean) extends Expression {
  override def cpp(out : CppStream) : Unit = out << value
}

case class VariableAccess(var name : String, var dType : Option[Datatype] = None) extends Access {
  override def cpp(out : CppStream) : Unit = out << name
}

case class ArrayAccess(var base : Expression, var index : Expression) extends Access {
  override def cpp(out : CppStream) : Unit = {
    index match {
      case ind : MultiIndex => out << base << ind
      case ind : Expression => out << base << '[' << ind << ']'
    }
  }
}

case class OffsetIndex(var minOffset : Int, var maxOffset : Int, var index : Expression, var offset : Expression) extends Expression {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = OffsetIndex\n"

  def expandSpecial : AdditionExpression = {
    index + offset
  }
}

case class MultiIndex(
  var index_0 : Expression = null,
  var index_1 : Expression = null,
  var index_2 : Expression = null,
  var index_3 : Expression = null)
    extends Expression with Iterable[Expression] {
  def this(indices : Array[Expression]) = this(
    if (indices.length > 0) indices(0) else null,
    if (indices.length > 1) indices(1) else null,
    if (indices.length > 2) indices(2) else null,
    if (indices.length > 3) indices(3) else null)
  def this(indices : Array[Int]) = this(
    (if (indices.length > 0) IntegerConstant(indices(0)) else null) : Expression,
    (if (indices.length > 1) IntegerConstant(indices(1)) else null) : Expression,
    (if (indices.length > 2) IntegerConstant(indices(2)) else null) : Expression,
    (if (indices.length > 3) IntegerConstant(indices(3)) else null) : Expression)
  def this(names : String*) = this(
    (if (names.size > 0) VariableAccess(names(0), Some(IntegerDatatype())) else null) : Expression,
    (if (names.size > 1) VariableAccess(names(1), Some(IntegerDatatype())) else null) : Expression,
    (if (names.size > 2) VariableAccess(names(2), Some(IntegerDatatype())) else null) : Expression,
    (if (names.size > 3) VariableAccess(names(3), Some(IntegerDatatype())) else null) : Expression)
  def this(left : MultiIndex, right : MultiIndex, f : (Expression, Expression) => Expression) = this(
    if (left(0) != null && right(0) != null) { Duplicate(f(left(0), right(0))) } else null,
    if (left(1) != null && right(1) != null) { Duplicate(f(left(1), right(1))) } else null,
    if (left(2) != null && right(2) != null) { Duplicate(f(left(2), right(2))) } else null,
    if (left(3) != null && right(3) != null) { Duplicate(f(left(3), right(3))) } else null)

  override def cpp(out : CppStream) : Unit = {
    out << '[' <<< (this, ", ") << ']'
  }

  def apply(i : Int) : Expression = {
    i match {
      case 0 => index_0
      case 1 => index_1
      case 2 => index_2
      case 3 => index_3
    }
  }

  def update(i : Int, up : Expression) : Unit = {
    i match {
      case 0 => index_0 = up
      case 1 => index_1 = up
      case 2 => index_2 = up
      case 3 => index_3 = up
    }
  }

  def +(that : MultiIndex) : MultiIndex = new MultiIndex(this, that, _ + _)

  override def iterator() : scala.collection.Iterator[Expression] = {
    return new Iterator[Expression]() {
      private var pointer : Int = 0
      override def hasNext : Boolean = pointer < 4 && apply(pointer) != null
      override def next() : Expression = {
        val res = apply(pointer)
        pointer += 1
        return res
      }
    }
  }
}

case class DirectFieldAccess(var fieldSelection : FieldSelection, var index : MultiIndex) extends Expression {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = FieldAccess\n"

  def linearize : LinearizedFieldAccess = {
    new LinearizedFieldAccess(fieldSelection, Mapping.resolveMultiIdx(fieldSelection.layout, index))
  }
}

case class FieldAccess(var fieldSelection : FieldSelection, var index : MultiIndex) extends Expression {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = FieldAccess\n"

  def linearize : LinearizedFieldAccess = {
    new LinearizedFieldAccess(fieldSelection, Mapping.resolveMultiIdx(fieldSelection.layout, new MultiIndex(index, fieldSelection.referenceOffset, _ + _)))
  }
}

case class ExternalFieldAccess(var name : Expression, var field : ExternalField, var index : MultiIndex) extends Expression {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = ExternalFieldAccess\n"

  def linearize : ArrayAccess = {
    new ArrayAccess(name, Mapping.resolveMultiIdx(field.layout, index))
  }
}

case class LinearizedFieldAccess(var fieldSelection : FieldSelection, var index : Expression) extends Expression with Expandable {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = ExternalFieldAccess\n"

  override def expand : Output[Expression] = {
    new ArrayAccess(new iv.FieldData(fieldSelection.field, fieldSelection.level, fieldSelection.slot, fieldSelection.fragIdx), index)
  }
}

case class StencilAccess(var stencil : Stencil) extends Expression {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = StencilAccess\n"
}

case class StencilFieldAccess(var stencilFieldSelection : StencilFieldSelection, var index : MultiIndex) extends Expression {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = StencilFieldAccess\n"

  def buildStencil : Stencil = {
    var entries : ListBuffer[StencilEntry] = ListBuffer()
    for (e <- 0 until stencilFieldSelection.stencil.entries.size) {
      var stencilFieldIdx = Duplicate(index)
      stencilFieldIdx(Knowledge.dimensionality) = e
      var fieldSel = stencilFieldSelection.toFieldSelection
      fieldSel.arrayIndex = e
      entries += new StencilEntry(stencilFieldSelection.stencil.entries(e).offset, new FieldAccess(fieldSel, stencilFieldIdx))
    }
    new Stencil("GENERATED_PLACEHOLDER_STENCIL", stencilFieldSelection.stencil.level, entries)
  }
}

case class MemberAccess(var base : Access, var varAcc : VariableAccess) extends Access {
  override def cpp(out : CppStream) : Unit = out << base << '.' << varAcc
}

case class DerefAccess(var base : Access) extends Access {
  override def cpp(out : CppStream) : Unit = out << "(*" << base << ')'
}

case class UnaryExpression(var operator : UnaryOperators.Value, var expression : Expression) extends Expression {
  override def cpp(out : CppStream) : Unit = out << '(' << operator << expression << ')'
}

case class AdditionExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp(out : CppStream) : Unit = out << '(' << left << '+' << right << ')'
}

case class SubtractionExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp(out : CppStream) : Unit = out << '(' << left << '-' << right << ')'
}

case class MultiplicationExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp(out : CppStream) : Unit = out << '(' << left << '*' << right << ')'
}

case class DivisionExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp(out : CppStream) : Unit = out << '(' << left << '/' << right << ')'
}

case class ModuloExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp(out : CppStream) : Unit = out << '(' << left << '%' << right << ')'
}

case class PowerExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp(out : CppStream) : Unit = out << "pow(" << left << ", " << right << ')' // FIXME: check for integer constant => use pown
}

case class EqEqExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp(out : CppStream) : Unit = out << '(' << left << "==" << right << ')'
}

case class NeqNeqExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp(out : CppStream) : Unit = out << '(' << left << "!=" << right << ')'
}

case class AndAndExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp(out : CppStream) : Unit = out << '(' << left << "&&" << right << ')'
}

case class OrOrExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp(out : CppStream) : Unit = out << '(' << left << "||" << right << ')'
}

case class NegationExpression(var left : Expression) extends Expression {
  override def cpp(out : CppStream) : Unit = out << '!' << '(' << left << ')'
}

case class LowerExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp(out : CppStream) : Unit = out << '(' << left << '<' << right << ')'
}

case class GreaterExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp(out : CppStream) : Unit = out << '(' << left << '>' << right << ')'
}

case class LowerEqualExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp(out : CppStream) : Unit = out << '(' << left << "<=" << right << ')'
}

case class GreaterEqualExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp(out : CppStream) : Unit = out << '(' << left << ">=" << right << ')'
}

case class BitwiseAndExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp(out : CppStream) : Unit = out << '(' << left << '&' << right << ')'
}

private object MinMaxPrinter {
  def cppsb(out : CppStream, args : ListBuffer[Expression], method : String) : Unit = {
    if (args.length == 1)
      out << args(0)

    else if (Knowledge.supports_initializerList)
      out << method << "({ " <<< (args, ", ") << " })"

    else {
      for (i <- 0 until args.length - 1)
        out << method << '('
      val it : Iterator[Expression] = args.iterator
      out << it.next()
      while (it.hasNext)
        out << ", " << it.next() << ')'
    }
  }
}

case class MinimumExpression(var args : ListBuffer[Expression]) extends Expression {
  override def cpp(out : CppStream) : Unit = {
    MinMaxPrinter.cppsb(out, args, "std::min")
  }
}

case class MaximumExpression(var args : ListBuffer[Expression]) extends Expression {
  override def cpp(out : CppStream) : Unit = {
    MinMaxPrinter.cppsb(out, args, "std::max")
  }
}

case class FunctionCallExpression(var name : Expression, var arguments : ListBuffer[Expression]) extends Expression {
  def this(name : Expression, argument : Expression) = this(name, ListBuffer(argument))

  override def cpp(out : CppStream) : Unit = out << name << '(' <<< (arguments, ", ") << ')'
}

case class InitializerList(var arguments : ListBuffer[Expression]) extends Expression {
  def this(argument : Expression) = this(ListBuffer(argument))

  override def cpp(out : CppStream) : Unit = out << "{ " <<< (arguments, ", ") << " }"
}

case class MemberFunctionCallExpression(var objectName : Expression, var name : Expression, var arguments : ListBuffer[Expression]) extends Expression {
  override def cpp(out : CppStream) : Unit = out << objectName << '.' << name << '(' <<< (arguments, ", ") << ')'
}

case class TernaryConditionExpression(var condition : Expression, var trueBody : Expression, var falseBody : Expression) extends Expression {
  override def cpp(out : CppStream) : Unit = out << '(' << condition << " ? " << trueBody << " : " << falseBody << ')'
}

case class Reduction(var op : BinaryOperators.Value, var target : Expression) extends Expression {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = Reduction\n"

  def getOMPClause : String = {
    val str = new CppStream()
    str << "reduction(" << op << ':' << target << ')'
    return str.toString()
  }
}

//////////////////////////// SIMD Expressions \\\\\\\\\\\\\\\\\\\\\\\\\\\\

case class SIMD_LoadExpression(var mem : Expression, var aligned : Boolean) extends Expression {
  override def cpp(out : CppStream) : Unit = {
    Knowledge.simd_instructionSet match {
      case "SSE3"         => if (aligned) out << "_mm_load_pd" else out << "_mm_loadu_pd"
      case "AVX" | "AVX2" => if (aligned) out << "_mm256_load_pd" else out << "_mm256_loadu_pd"
    }
    out << '(' << mem << ')'
  }
}

case class SIMD_Load1Expression(var mem : Expression) extends Expression {
  override def cpp(out : CppStream) : Unit = {
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << "_mm_load1_pd"
      case "AVX" | "AVX2" => out << "_mm256_broadcast_sd"
    }
    out << '(' << mem << ')'
  }
}

case class SIMD_NegateExpresseion(var vect : Expression) extends Expression {
  override def cpp(out : CppStream) : Unit = {
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << "_mm_xor_pd(" << vect << ", _mm_set1_pd(-0.f))"
      case "AVX" | "AVX2" => out << "_mm256_xor_pd(" << vect << ", _mm256_set1_pd(-0.f))"
    }
  }
}

case class SIMD_AdditionExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp(out : CppStream) : Unit = {
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << "_mm_add_pd"
      case "AVX" | "AVX2" => out << "_mm256_add_pd"
    }
    out << '(' << left << ", " << right << ')'
  }
}

case class SIMD_SubtractionExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp(out : CppStream) : Unit = {
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << "_mm_sub_pd"
      case "AVX" | "AVX2" => out << "_mm256_sub_pd"
    }
    out << '(' << left << ", " << right << ')'
  }
}

case class SIMD_MultiplicationExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp(out : CppStream) : Unit = {
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << "_mm_mul_pd"
      case "AVX" | "AVX2" => out << "_mm256_mul_pd"
    }
    out << '(' << left << ", " << right << ')'
  }
}

case class SIMD_MultiplyAddExpression(var factor1 : Expression, var factor2 : Expression,
    var summand : Expression) extends Expression {

  override def cpp(out : CppStream) : Unit = {
    Knowledge.simd_instructionSet match {
      case "SSE3" => out << "_mm_add_pd(_mm_mul_pd(" << factor1 << ", " << factor2 << "), " << summand << ')'
      case "AVX"  => out << "_mm256_add_pd(_mm256_mul_pd(" << factor1 << ", " << factor2 << "), " << summand << ')'
      case "AVX2" => out << "_mm256_fmadd_pd(" << factor1 << ", " << factor2 << ", " << summand << ')'
    }
  }
}

case class SIMD_MultiplySubExpression(var factor1 : Expression, var factor2 : Expression,
    var summand : Expression) extends Expression {

  override def cpp(out : CppStream) : Unit = {
    Knowledge.simd_instructionSet match {
      case "SSE3" => out << "_mm_sub_pd(_mm_mul_pd(" << factor1 << ", " << factor2 << "), " << summand << ')'
      case "AVX"  => out << "_mm256_sub_pd(_mm256_mul_pd(" << factor1 << ", " << factor2 << "), " << summand << ')'
      case "AVX2" => out << "_mm256_fmsub_pd(" << factor1 << ", " << factor2 << ", " << summand << ')'
    }
  }
}

case class SIMD_DivisionExpression(var left : Expression, var right : Expression) extends Expression {
  override def cpp(out : CppStream) : Unit = {
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << "_mm_div_pd"
      case "AVX" | "AVX2" => out << "_mm256_div_pd"
    }
    out << '(' << left << ", " << right << ')'
  }
}

case class SIMD_FloatConstant(var value : Double) extends Expression {
  // ensure the compiler can parse the string
  override def cpp(out : CppStream) : Unit = {
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << "_mm_set1_pd"
      case "AVX" | "AVX2" => out << "_mm256_set1_pd"
    }
    out << '(' << String.format(java.util.Locale.US, "%e", Double.box(value)) << ')'
  }
}

case class SIMD_Scalar2VectorExpression(var scalar : String, var dType : Datatype,
    var increment : Boolean) extends Expression {

  override def cpp(out : CppStream) : Unit = {
    if (increment) {
      Knowledge.simd_instructionSet match {
        case "SSE3"         => out << "_mm_set_pd"
        case "AVX" | "AVX2" => out << "_mm256_set_pd"
      }
      out << '('
      for (i <- Knowledge.simd_vectorSize - 1 to 1 by -1)
        out << scalar << '+' << i << ','
      out << scalar << ')'

    } else {
      Knowledge.simd_instructionSet match {
        case "SSE3"         => out << "_mm_set1_pd"
        case "AVX" | "AVX2" => out << "_mm256_set1_pd"
      }
      out << '(' << scalar << ')'
    }
  }
}
