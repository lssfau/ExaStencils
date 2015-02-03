package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.prettyprinting._
import exastencils.strategies._

trait Expression extends Node with PrettyPrintable {
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
  override def prettyprint(out : PpStream) : Unit = ()
}

case class ConcatenationExpression(var expressions : ListBuffer[Expression]) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out <<< expressions

  override def ~(exp : Expression) : ConcatenationExpression = {
    expressions += exp
    this
  }
}

case class SpacedConcatenationExpression(var expressions : ListBuffer[Expression]) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out <<< (expressions, " ")

  override def ~~(exp : Expression) : SpacedConcatenationExpression = {
    expressions += exp
    this
  }
}

case class StringConstant(var value : String) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << value
}

case class IntegerConstant(var v : Long) extends Number {
  override def prettyprint(out : PpStream) : Unit = out << v
  override def value = v
}

case class FloatConstant(var v : Double) extends Number {
  override def prettyprint(out : PpStream) : Unit = {
    out << String.format(java.util.Locale.US, "%s", Double.box(value)) // ensure the compiler can parse the string
    if (!Knowledge.useDblPrecision) out << "f"
  }

  override def value = v
}

case class BooleanConstant(var value : Boolean) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << value
}

case class Allocation(var datatype : Datatype, var size : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "new" << ' ' << datatype << "[" << size << "]"
}

case class SizeOfExpression(var datatype : Datatype) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "sizeof" << "(" << datatype << ")"
}

case class CastExpression(var datatype : Datatype, var toCast : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "((" << datatype << ")" << toCast << ")"
}

case class VariableAccess(var name : String, var dType : Option[Datatype] = None) extends Access {
  override def prettyprint(out : PpStream) : Unit = out << name
}

case class ArrayAccess(var base : Expression, var index : Expression) extends Access {
  override def prettyprint(out : PpStream) : Unit = {
    index match {
      case ind : MultiIndex => out << base << ind
      case ind : Expression => out << base << '[' << ind << ']'
    }
  }
}

case class OffsetIndex(var minOffset : Int, var maxOffset : Int, var index : Expression, var offset : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = OffsetIndex\n"

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

  override def prettyprint(out : PpStream) : Unit = {
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
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = FieldAccess\n"

  def linearize : LinearizedFieldAccess = {
    new LinearizedFieldAccess(fieldSelection, Mapping.resolveMultiIdx(fieldSelection.fieldLayout, index))
  }
}

case class FieldAccess(var fieldSelection : FieldSelection, var index : MultiIndex) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = FieldAccess\n"

  def expandSpecial() : DirectFieldAccess = {
    DirectFieldAccess(fieldSelection, index + fieldSelection.referenceOffset)
  }
}

case class ExternalFieldAccess(var name : Expression, var field : ExternalField, var index : MultiIndex) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = ExternalFieldAccess\n"

  def linearize : ArrayAccess = {
    new ArrayAccess(name, Mapping.resolveMultiIdx(field.fieldLayout, index))
  }
}

case class LinearizedFieldAccess(var fieldSelection : FieldSelection, var index : Expression) extends Expression with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LinearizedFieldAccess\n"

  override def expand : Output[Expression] = {
    new ArrayAccess(new iv.FieldData(fieldSelection.field, fieldSelection.level, fieldSelection.slot, fieldSelection.fragIdx), index)
  }
}

case class StencilAccess(var stencil : Stencil) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = StencilAccess\n"
}

case class StencilFieldAccess(var stencilFieldSelection : StencilFieldSelection, var index : MultiIndex) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = StencilFieldAccess\n"

  def buildStencil : Stencil = {
    var entries : ListBuffer[StencilEntry] = ListBuffer()
    for (e <- 0 until stencilFieldSelection.stencil.entries.size) {
      var stencilFieldIdx = Duplicate(index)
      stencilFieldIdx(Knowledge.dimensionality) = e
      var fieldSel = stencilFieldSelection.toFieldSelection
      fieldSel.arrayIndex = Some(e)
      entries += new StencilEntry(stencilFieldSelection.stencil.entries(e).offset, new FieldAccess(fieldSel, stencilFieldIdx))
    }
    new Stencil("GENERATED_PLACEHOLDER_STENCIL", stencilFieldSelection.stencil.level, entries)
  }
}

case class MemberAccess(var base : Access, var varAcc : VariableAccess) extends Access {
  override def prettyprint(out : PpStream) : Unit = out << base << '.' << varAcc
}

case class DerefAccess(var base : Access) extends Access {
  override def prettyprint(out : PpStream) : Unit = out << "(*" << base << ')'
}

case class UnaryExpression(var operator : UnaryOperators.Value, var expression : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << operator << expression << ')'
}

case class AdditionExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '+' << right << ')'
}

case class SubtractionExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '-' << right << ')'
}

case class MultiplicationExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '*' << right << ')'
}

case class DivisionExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '/' << right << ')'
}

case class ModuloExpression(var left : Expression, var right : Expression) extends Expression {
  // assumes "left >= 0"   if not, generate something like "(left%right + right) % right"
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '%' << right << ')'
}

case class PowerExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "pow(" << left << ", " << right << ')' // FIXME: check for integer constant => use pown
}

case class EqEqExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "==" << right << ')'
}

case class NeqNeqExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "!=" << right << ')'
}

case class AndAndExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "&&" << right << ')'
}

case class OrOrExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "||" << right << ')'
}

case class NegationExpression(var left : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '!' << '(' << left << ')'
}

case class LowerExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '<' << right << ')'
}

case class GreaterExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '>' << right << ')'
}

case class LowerEqualExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "<=" << right << ')'
}

case class GreaterEqualExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << ">=" << right << ')'
}

case class BitwiseAndExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '&' << right << ')'
}

case class PreDecrementExpression(var left : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "--" << left
}

case class PostDecrementExpression(var left : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << left << "--"
}

case class PreIncrementExpression(var left : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "++" << left
}

case class PostIncrementExpression(var left : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << left << "++"
}

private object MinMaxPrinter {
  def prettyprintsb(out : PpStream, args : ListBuffer[Expression], method : String) : Unit = {
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
  override def prettyprint(out : PpStream) : Unit = {
    MinMaxPrinter.prettyprintsb(out, args, "std::min")
  }
}

case class MaximumExpression(var args : ListBuffer[Expression]) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    MinMaxPrinter.prettyprintsb(out, args, "std::max")
  }
}

case class FunctionCallExpression(var name : Expression, var arguments : ListBuffer[Expression]) extends Expression {
  def this(name : Expression, argument : Expression) = this(name, ListBuffer(argument))

  override def prettyprint(out : PpStream) : Unit = out << name << '(' <<< (arguments, ", ") << ')'
}

case class InitializerList(var arguments : ListBuffer[Expression]) extends Expression {
  def this(argument : Expression) = this(ListBuffer(argument))

  override def prettyprint(out : PpStream) : Unit = out << "{ " <<< (arguments, ", ") << " }"
}

case class MemberFunctionCallExpression(var objectName : Expression, var name : Expression, var arguments : ListBuffer[Expression]) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << objectName << '.' << name << '(' <<< (arguments, ", ") << ')'
}

case class TernaryConditionExpression(var condition : Expression, var trueBody : Expression, var falseBody : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << condition << " ? " << trueBody << " : " << falseBody << ')'
}

case class Reduction(var op : String, var target : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = Reduction\n"

  def getOMPClause : String = {
    val str = new PpStream()
    str << "reduction(" << op << " : " << target << ')'
    return str.toString()
  }
}

case class StencilConvolution(var stencil : Stencil, var fieldAccess : FieldAccess) extends Expression with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = StencilConvolution\n"

  def resolveEntry(idx : Int) : Expression = {
    stencil.entries(idx).coefficient * new FieldAccess(fieldAccess.fieldSelection, fieldAccess.index + stencil.entries(idx).offset)
  }

  def expand : Output[Expression] = {
    var ret : Expression = (0 until stencil.entries.size).toArray.map(idx => resolveEntry(idx)).toArray[Expression].reduceLeft(_ + _)
    SimplifyStrategy.doUntilDoneStandalone(ret)
    ret
  }
}

case class StencilFieldConvolution(var stencilFieldAccess : StencilFieldAccess, var fieldAccess : FieldAccess) extends Expression with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = StencilConvolution\n"

  def resolveEntry(idx : Int) : Expression = {
    var stencilFieldIdx = Duplicate(stencilFieldAccess.index)
    stencilFieldIdx(Knowledge.dimensionality) = idx

    FieldAccess(stencilFieldAccess.stencilFieldSelection.toFieldSelection, stencilFieldIdx) *
      new FieldAccess(fieldAccess.fieldSelection, fieldAccess.index + stencilFieldAccess.stencilFieldSelection.stencil.entries(idx).offset)
  }

  def expand : Output[Expression] = {
    var ret : Expression = (0 until stencilFieldAccess.stencilFieldSelection.stencil.entries.size).toArray.map(idx => resolveEntry(idx)).toArray[Expression].reduceLeft(_ + _)
    SimplifyStrategy.doUntilDoneStandalone(ret)
    ret
  }
}

case class StencilStencilConvolution(var stencilLeft : Stencil, var stencilRight : Stencil) extends Expression with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = StencilStencilConvolution\n"

  def expand : Output[StencilAccess] = {
    var entries : ListBuffer[StencilEntry] = ListBuffer()

    for (re <- stencilRight.entries) {
      for (le <- stencilLeft.entries) {
        var rightOffset = Duplicate(re.offset)
        //            if (stencilRight.level < stencilLeft.level) {
        //              for (d <- 0 until Knowledge.dimensionality)
        //                rightOffset(d) = (dimToString(d) : Expression) * 2 + rightOffset(d)
        //            }

        var leftOffset = Duplicate(le.offset)
        if (stencilRight.level > stencilLeft.level) {
          for (d <- 0 until Knowledge.dimensionality)
            leftOffset(d) = (dimToString(d) : Expression) / 2 + leftOffset(d)
        } else {
          for (d <- 0 until Knowledge.dimensionality)
            leftOffset(d) = (dimToString(d) : Expression) + leftOffset(d)
        }

        var combOff = leftOffset
        ResolveCoordinates.replacement = rightOffset
        ResolveCoordinates.doUntilDoneStandalone(combOff)

        var combCoeff : Expression = (re.coefficient * le.coefficient)
        SimplifyStrategy.doUntilDoneStandalone(combOff)
        SimplifyStrategy.doUntilDoneStandalone(combCoeff)
        var addToEntry = entries.find(e => e.offset match { case o if (combOff == o) => true; case _ => false })
        if (addToEntry.isDefined) {
          combCoeff += addToEntry.get.coefficient
          SimplifyStrategy.doUntilDoneStandalone(combCoeff)
          addToEntry.get.coefficient = combCoeff
        } else entries += new StencilEntry(combOff, combCoeff)
      }
    }

    StencilAccess(Stencil(stencilLeft.identifier + "_" + stencilRight.identifier, stencilLeft.level, entries))
  }
}

case class StencilFieldStencilConvolution(var stencilLeft : StencilFieldAccess, var stencilRight : Stencil) extends Expression with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = StencilFieldStencilConvolution\n"

  def expand : Output[StencilAccess] = {
    var entries : ListBuffer[StencilEntry] = ListBuffer()

    for (re <- stencilRight.entries) {
      for (e <- 0 until stencilLeft.stencilFieldSelection.stencil.entries.size) {
        var stencilFieldIdx = Duplicate(stencilLeft.index)
        stencilFieldIdx(Knowledge.dimensionality) = e
        for (dim <- 0 until Knowledge.dimensionality)
          stencilFieldIdx(dim) += re.offset(dim)
        var fieldSel = stencilLeft.stencilFieldSelection.toFieldSelection
        fieldSel.arrayIndex = Some(e)

        var rightOffset = Duplicate(re.offset)

        var leftOffset = Duplicate(stencilLeft.stencilFieldSelection.stencil.entries(e).offset)
        if (stencilRight.level > stencilLeft.stencilFieldSelection.stencil.level) {
          for (d <- 0 until Knowledge.dimensionality)
            leftOffset(d) = (dimToString(d) : Expression) / 2 + leftOffset(d)
        } else {
          for (d <- 0 until Knowledge.dimensionality)
            leftOffset(d) = (dimToString(d) : Expression) + leftOffset(d)
        }

        var combOff = leftOffset
        ResolveCoordinates.replacement = rightOffset
        ResolveCoordinates.doUntilDoneStandalone(combOff)

        var combCoeff : Expression = (re.coefficient * new FieldAccess(fieldSel, stencilFieldIdx))
        SimplifyStrategy.doUntilDoneStandalone(combOff)
        SimplifyStrategy.doUntilDoneStandalone(combCoeff)
        var addToEntry = entries.find(e => e.offset match { case o if (combOff == o) => true; case _ => false })
        if (addToEntry.isDefined) {
          combCoeff += addToEntry.get.coefficient
          SimplifyStrategy.doUntilDoneStandalone(combCoeff)
          addToEntry.get.coefficient = combCoeff
        } else entries += new StencilEntry(combOff, combCoeff)
      }
    }

    StencilAccess(Stencil(stencilLeft.stencilFieldSelection.stencil.identifier + "_" + stencilRight.identifier, stencilLeft.stencilFieldSelection.stencil.level, entries))
  }
}

//////////////////////////// SIMD Expressions \\\\\\\\\\\\\\\\\\\\\\\\\\\\

case class SIMD_LoadExpression(var mem : Expression, var aligned : Boolean) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    Knowledge.simd_instructionSet match {
      case "SSE3"         => if (aligned) out << "_mm_load_pd(" else out << "_mm_loadu_pd("
      case "AVX" | "AVX2" => if (aligned) out << "_mm256_load_pd(" else out << "_mm256_loadu_pd("
      case "QPX"          => if (aligned) out << "vec_lda(0," else out << "NOT VALID ; unaligned load for QPX: ("
    }
    out << mem << ')'
  }
}

case class SIMD_Load1Expression(var mem : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << "_mm_load1_pd("
      case "AVX" | "AVX2" => out << "_mm256_broadcast_sd("
      case "QPX"          => out << "vec_ldsa(0,"
    }
    out << mem << ')'
  }
}

case class SIMD_ConcShift(var left : VariableAccess, var right : VariableAccess, var offset : Int) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    Knowledge.simd_instructionSet match {
      case "SSE3" =>
        offset match {
          case 1 => out << "_mm_shuffle_pd(" << left << ", " << right << ", 1);"
        }
      case "AVX" | "AVX2" =>
        offset match {
          case 1 => out << "_mm256_shuffle_pd(" << left << ", _mm256_permute2f128_pd(" << left << ", " << right << ", 33), 5)"
          case 2 => out << "_mm256_permute2f128_pd(" << left << ", " << right << ", 33)"
          case 3 => out << "_mm256_shuffle_pd(_mm256_permute2f128_pd(" << left << ", " << right << ", 33), " << right << ", 5)"
        }
      case "QPX" => out << "vec_sldw(" << left << ", " << right << ", " << offset << ")"
    }
  }
}

case class SIMD_NegateExpresseion(var vect : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << "_mm_xor_pd(" << vect << ", _mm_set1_pd(-0.f))"
      case "AVX" | "AVX2" => out << "_mm256_xor_pd(" << vect << ", _mm256_set1_pd(-0.f))"
      case "QPX"          => out << "vec_neg(" << vect << ')'
    }
  }
}

case class SIMD_AdditionExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << "_mm_add_pd"
      case "AVX" | "AVX2" => out << "_mm256_add_pd"
      case "QPX"          => out << "vec_add"
    }
    out << '(' << left << ", " << right << ')'
  }
}

case class SIMD_SubtractionExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << "_mm_sub_pd"
      case "AVX" | "AVX2" => out << "_mm256_sub_pd"
      case "QPX"          => out << "vec_sub"
    }
    out << '(' << left << ", " << right << ')'
  }
}

case class SIMD_MultiplicationExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << "_mm_mul_pd"
      case "AVX" | "AVX2" => out << "_mm256_mul_pd"
      case "QPX"          => out << "vec_mul"
    }
    out << '(' << left << ", " << right << ')'
  }
}

case class SIMD_MultiplyAddExpression(var factor1 : Expression, var factor2 : Expression,
    var summand : Expression) extends Expression {

  override def prettyprint(out : PpStream) : Unit = {
    Knowledge.simd_instructionSet match {
      case "SSE3" => out << "_mm_add_pd(_mm_mul_pd(" << factor1 << ", " << factor2 << "), " << summand << ')'
      case "AVX"  => out << "_mm256_add_pd(_mm256_mul_pd(" << factor1 << ", " << factor2 << "), " << summand << ')'
      case "AVX2" => out << "_mm256_fmadd_pd(" << factor1 << ", " << factor2 << ", " << summand << ')'
      case "QPX"  => out << "vec_madd(" << factor1 << ", " << factor2 << ", " << summand << ')'
    }
  }
}

case class SIMD_MultiplySubExpression(var factor1 : Expression, var factor2 : Expression,
    var summand : Expression) extends Expression {

  override def prettyprint(out : PpStream) : Unit = {
    Knowledge.simd_instructionSet match {
      case "SSE3" => out << "_mm_sub_pd(_mm_mul_pd(" << factor1 << ", " << factor2 << "), " << summand << ')'
      case "AVX"  => out << "_mm256_sub_pd(_mm256_mul_pd(" << factor1 << ", " << factor2 << "), " << summand << ')'
      case "AVX2" => out << "_mm256_fmsub_pd(" << factor1 << ", " << factor2 << ", " << summand << ')'
      case "QPX"  => out << "vec_msub(" << factor1 << ", " << factor2 << ", " << summand << ')'
    }
  }
}

case class SIMD_DivisionExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << "_mm_div_pd"
      case "AVX" | "AVX2" => out << "_mm256_div_pd"
      case "QPX"          => out << "vec_swdiv_nochk"
    }
    out << '(' << left << ", " << right << ')'
  }
}

case class SIMD_FloatConstant(var value : Double) extends Expression {
  // ensure the compiler can parse the string
  override def prettyprint(out : PpStream) : Unit = {
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << "_mm_set1_pd"
      case "AVX" | "AVX2" => out << "_mm256_set1_pd"
      case "QPX"          => out << "vec_splats"
    }
    out << '(' << String.format(java.util.Locale.US, "%e", Double.box(value)) << ')'
  }
}

case class SIMD_Scalar2VectorExpression(var scalar : String, var dType : Datatype,
    var increment : Boolean) extends Expression {

  override def prettyprint(out : PpStream) : Unit = {
    if (increment) {
      Knowledge.simd_instructionSet match {
        case "SSE3"         => out << "_mm_set_pd"
        case "AVX" | "AVX2" => out << "_mm256_set_pd"
        case "QPX" =>
          out << "NOT VALID ; SIMD_Scalar2VectorExpression(., ., true) for BG/Q not yet implemented"
          return
      }
      out << '('
      for (i <- Knowledge.simd_vectorSize - 1 to 1 by -1)
        out << scalar << '+' << i << ','
      out << scalar << ')'

    } else {
      Knowledge.simd_instructionSet match {
        case "SSE3"         => out << "_mm_set1_pd"
        case "AVX" | "AVX2" => out << "_mm256_set1_pd"
        case "QPX"          => out << "vec_splats"
      }
      out << '(' << scalar << ')'
    }
  }
}
