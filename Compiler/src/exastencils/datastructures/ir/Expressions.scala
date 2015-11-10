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
    new ConcatenationExpression(this, exp)
  }
  def ~~(exp : Expression) : SpacedConcatenationExpression = {
    new SpacedConcatenationExpression(this, exp)
  }

  import BinaryOperators._
  def +(other : Expression) = new AdditionExpression(this, other)
  def :+(other : Expression) = new ElementwiseAdditionExpression(this, other) // Scala does not allow .+ and fails with Dot+
  def -(other : Expression) = new SubtractionExpression(this, other)
  def :-(other : Expression) = new ElementwiseSubtractionExpression(this, other) // Scala does not allow .- and fails with Dot-
  def *(other : Expression) = new MultiplicationExpression(this, other)
  def :*(other : Expression) = new ElementwiseMultiplicationExpression(this, other) // Scala does not allow .* and fails with Dot*
  def /(other : Expression) = new DivisionExpression(this, other)
  def :/(other : Expression) = new ElementwiseDivisionExpression(this, other) // Scala does not allow ./ and fails with Dot/
  def Pow(other : Expression) = new PowerExpression(this, other)
  def DotPow(other : Expression) = new ElementwisePowerExpression(this, other) // Scala does not allow .% and fails with Dot% and fails with :%
  def Mod(other : Expression) = new ModuloExpression(this, other)
  def Modulo(other : Expression) = new ModuloExpression(this, other)
  def DotMod(other : Expression) = new ElementwiseModuloExpression(this, other)
  def DotModulo(other : Expression) = new ElementwiseModuloExpression(this, other)
  def And(other : Expression) = new AndAndExpression(this, other)
  def AndAnd(other : Expression) = new AndAndExpression(this, other)
  def Or(other : Expression) = new OrOrExpression(this, other)
  def OrOr(other : Expression) = new OrOrExpression(this, other)
  def EqEq(other : Expression) = new EqEqExpression(this, other)
  def Neq(other : Expression) = new NeqExpression(this, other)
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

  val ElementwiseAddition = Value(".+")
  val ElementwiseSubtraction = Value(".-")
  val ElementwiseMultiplication = Value(".*")
  val ElementwiseDivision = Value("./")
  val ElementwisePower = Value(".**")
  val ElementwiseModulo = Value(".%")

  val AndAnd = Value("&&")
  val AndAndWritten = Value("and")
  val OrOr = Value("||")
  val OrOrWritten = Value("or")
  val Negation = Value("!")
  val EqEq = Value("==")
  val Neq = Value("!=")
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
    case Addition                  => return new AdditionExpression(left, right)
    case Subtraction               => return new SubtractionExpression(left, right)
    case Multiplication            => return new MultiplicationExpression(left, right)
    case Division                  => return new DivisionExpression(left, right)
    case Power                     => return new PowerExpression(left, right)
    case Modulo                    => return new ModuloExpression(left, right)

    case ElementwiseAddition       => return new ElementwiseAdditionExpression(left, right)
    case ElementwiseSubtraction    => return new ElementwiseSubtractionExpression(left, right)
    case ElementwiseMultiplication => return new ElementwiseMultiplicationExpression(left, right)
    case ElementwiseDivision       => return new ElementwiseDivisionExpression(left, right)
    case ElementwisePower          => return new ElementwisePowerExpression(left, right)
    case ElementwiseModulo         => return new ElementwiseModuloExpression(left, right)

    case AndAnd | AndAndWritten    => return new AndAndExpression(left, right)
    case OrOr | OrOrWritten        => return new OrOrExpression(left, right)
    case Negation                  => return new NegationExpression(left)
    case EqEq                      => return new EqEqExpression(left, right)
    case Neq                       => return new NeqExpression(left, right)
    case Lower                     => return new LowerExpression(left, right)
    case LowerEqual                => return new LowerEqualExpression(left, right)
    case Greater                   => return new GreaterExpression(left, right)
    case GreaterEqual              => return new GreaterEqualExpression(left, right)
    case BitwiseAnd                => return new BitwiseAndExpression(left, right)
  }
}

object UnaryOperators extends Enumeration {
  type UnaryOperators = Value
  //  val Positive = Value("")
  val Negative = Value("-")
  val Not = Value("!")
  val AddressOf = Value("&")
  val Indirection = Value("*")
  //  val BitwiseNegation = Value("~")

  exastencils.core.Duplicate.registerImmutable(this.getClass())

  def CreateExpression(op : String, exp : Expression) : Expression = CreateExpression(withName(op), exp)
  def CreateExpression(op : Value, exp : Expression) : Expression = op match {
    case Negative => return new NegativeExpression(exp)
    case Not      => return new NegationExpression(exp)
  }
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
  def this(exprs : Expression*) = this(exprs.to[ListBuffer])

  override def prettyprint(out : PpStream) : Unit = out <<< expressions

  override def ~(exp : Expression) : ConcatenationExpression = {
    expressions += exp
    this
  }
}

case class SpacedConcatenationExpression(var expressions : ListBuffer[Expression]) extends Expression {
  def this(exprs : Expression*) = this(exprs.to[ListBuffer])

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
    out << value // this uses value.toString(), which is Locale-independent and the string can be parsed without a loss of precision later
    if (!Knowledge.useDblPrecision) out << "f"
  }

  override def value = v
}

case class BooleanConstant(var value : Boolean) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << value
}

case class VectorExpression(var datatype : Option[Datatype], var expressions : ListBuffer[Expression], var rowVector : Option[Boolean]) extends Expression {
  def length = expressions.length

  def apply(i : Integer) = expressions(i)
  def isConstant = expressions.filter(e => e.isInstanceOf[Number]).length == expressions.length
  def innerType : Option[Datatype] = {
    if (datatype.isEmpty) {
      if (!isConstant) {
        None
      } else {
        expressions.foreach(e => e match {
          case x : FloatConstant => datatype = Some(RealDatatype)
          case _                 =>
        })
        datatype = Some(IntegerDatatype)
        return datatype
      }
    } else {
      return datatype
    }
  }
  def prettyprintInner(out : PpStream) : Unit = {
    out << "{"
    expressions.foreach(e => { e.prettyprint(out); out << ',' })
    out.removeLast() // remove last comma
    out << "}"
  }
  override def prettyprint(out : PpStream) : Unit = {
    out << "Matrix<"
    datatype.getOrElse(RealDatatype).prettyprint(out)
    out << ", "
    if (rowVector.getOrElse(true)) {
      out << "1, " << length << "> (" // row vector
    } else {
      out << length << ", 1> ("
    }
    prettyprintInner(out)
    out << ")"
  }
}

case class MatrixExpression(var datatype : Option[Datatype], var expressions : ListBuffer[ListBuffer[Expression]]) extends Expression {
  def prettyprintInner(out : PpStream) : Unit = {
    out << "{"
    expressions.foreach(f => f.foreach(e => { e.prettyprint(out); out << ',' }))
    out.removeLast() // remove last comma
    out << "}"
  }

  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) "double" else "float"

    out << "Matrix<"
    if (isInteger) out << "int, "; else out << prec << ", "
    out << rows << ", " << columns << "> ("
    prettyprintInner(out)
    out << ")"
  }
  def rows = expressions.length
  def columns = expressions(0).length

  def apply(i : Integer) = expressions(i)
  def isConstant = expressions.flatten.filter(e => e.isInstanceOf[Number]).length == expressions.flatten.length
  def isInteger = expressions.flatten.filter(e => e.isInstanceOf[IntegerConstant]).length == expressions.flatten.length
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
  def this(n : String, dT : Datatype) = this(n, Option(dT))

  override def prettyprint(out : PpStream) : Unit = out << name

  def printDeclaration() : String = dType.get.resolveUnderlyingDatatype.prettyprint + " " + name + dType.get.resolvePostscript
}

case class ArrayAccess(var base : Expression, var index : Expression, var alignedAccessPossible : Boolean = false) extends Access {
  override def prettyprint(out : PpStream) : Unit = {
    index match {
      case ind : MultiIndex => out << base << ind
      case ind : Expression => out << base << '[' << ind << ']'
    }
  }
}

//TODO specific expression for reading from fragment data file
case class ReadValueFrom(var datatype : Datatype, data : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "readValue<" << datatype << '>' << "(" << data << ")"
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
  def this(indices : Array[Double]) = this(
    (if (indices.length > 0) FloatConstant(indices(0)) else null) : Expression,
    (if (indices.length > 1) FloatConstant(indices(1)) else null) : Expression,
    (if (indices.length > 2) FloatConstant(indices(2)) else null) : Expression,
    (if (indices.length > 3) FloatConstant(indices(3)) else null) : Expression)
  def this(names : String*) = this(
    (if (names.size > 0) VariableAccess(names(0), Some(IntegerDatatype)) else null) : Expression,
    (if (names.size > 1) VariableAccess(names(1), Some(IntegerDatatype)) else null) : Expression,
    (if (names.size > 2) VariableAccess(names(2), Some(IntegerDatatype)) else null) : Expression,
    (if (names.size > 3) VariableAccess(names(3), Some(IntegerDatatype)) else null) : Expression)
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

case class TempBufferAccess(var buffer : iv.TmpBuffer, var index : MultiIndex, var strides : MultiIndex) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = TempBufferAccess\n"

  def linearize : ArrayAccess = {
    new ArrayAccess(buffer,
      Mapping.resolveMultiIdx(index, strides),
      false && Knowledge.data_alignTmpBufferPointers /* change here if aligned vector operations are possible for tmp buffers */ )
  }
}

case class DirectFieldAccess(var fieldSelection : FieldSelection, var index : MultiIndex) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = DirectFieldAccess\n"

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

case class VirtualFieldAccess(var fieldName : String,
    var level : Expression,
    var index : MultiIndex,
    var arrayIndex : Option[Int] = None,
    var fragIdx : Expression = LoopOverFragments.defIt) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = VirtualFieldAccess\n"

}

case class ExternalFieldAccess(var name : Expression, var field : ExternalField, var index : MultiIndex) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = ExternalFieldAccess\n"

  def x = new VariableAccess("x", IntegerDatatype)
  def y = new VariableAccess("y", IntegerDatatype)
  def z = new VariableAccess("z", IntegerDatatype)
  def w = new VariableAccess("w", IntegerDatatype)

  def linearize : ArrayAccess = {
    if (Knowledge.generateFortranInterface) // Fortran requires multi-index access to multidimensional arrays
      (Knowledge.dimensionality + (if (field.vectorSize > 1) 1 else 0)) match {
        case 1 => new ArrayAccess(x, false)
        case 2 => new ArrayAccess(new ArrayAccess(name, y, false), x, false)
        case 3 => new ArrayAccess(new ArrayAccess(new ArrayAccess(name, z, false), y, false), x, false)
        case 4 => new ArrayAccess(new ArrayAccess(new ArrayAccess(new ArrayAccess(name, w, false), z, false), y, false), x, false)
      }
    else
      new ArrayAccess(name, Mapping.resolveMultiIdx(field.fieldLayout, index), false)
  }
}

case class LinearizedFieldAccess(var fieldSelection : FieldSelection, var index : Expression) extends Expression with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LinearizedFieldAccess\n"

  override def expand : Output[Expression] = {
    new ArrayAccess(new iv.FieldData(fieldSelection.field, fieldSelection.level, fieldSelection.slot, fieldSelection.fragIdx), index, Knowledge.data_alignFieldPointers)
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

case class ElementwiseAdditionExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '+' << right << ')'
}

case class ElementwiseSubtractionExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '+' << right << ')'
}

case class ElementwiseMultiplicationExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '*' << right << ')'
}

case class ElementwiseDivisionExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '/' << right << ')'
}

case class ElementwiseModuloExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '.' << '%' << right << ')'
}

case class ElementwisePowerExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "dotpow(" << left << ", " << right << ')' // FIXME: check for integer constant => use pown
}

case class EqEqExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "==" << right << ')'
}

case class NeqExpression(var left : Expression, var right : Expression) extends Expression {
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
  override def prettyprint(out : PpStream) : Unit = out << "(--" << left << ')'
}

case class PostDecrementExpression(var left : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "--)"
}

case class PreIncrementExpression(var left : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "(++" << left << ')'
}

case class PostIncrementExpression(var left : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "++)"
}

case class NegativeExpression(var left : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "(-" << left << ')'
}

case class AddressofExpression(var left : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "(&" << left << ')'
}

private object MinMaxPrinter {
  def prettyprintsb(out : PpStream, args : ListBuffer[Expression], method : String) : Unit = {
    if (args.length == 1)
      out << args(0)

    else if (Knowledge.supports_initializerList)
      out << method << "({" <<< (args, ",") << "})"

    else {
      for (i <- 0 until args.length - 1)
        out << method << '('
      val it : Iterator[Expression] = args.iterator
      out << it.next()
      while (it.hasNext)
        out << ',' << it.next() << ')'
    }
  }
}

case class MinimumExpression(var args : ListBuffer[Expression]) extends Expression {
  def this(varargs : Expression*) = this(varargs.to[ListBuffer])

  override def prettyprint(out : PpStream) : Unit = {
    MinMaxPrinter.prettyprintsb(out, args, "std::min")
  }
}

case class MaximumExpression(var args : ListBuffer[Expression]) extends Expression {
  def this(varargs : Expression*) = this(varargs.to[ListBuffer])

  override def prettyprint(out : PpStream) : Unit = {
    MinMaxPrinter.prettyprintsb(out, args, "std::max")
  }
}

case class FunctionCallExpression(var name : String, var arguments : ListBuffer[Expression]) extends Expression {
  def this(name : String, args : Expression*) = this(name, args.to[ListBuffer])

  override def prettyprint(out : PpStream) : Unit = out << name << '(' <<< (arguments, ", ") << ')'
}

case class InitializerList(var arguments : ListBuffer[Expression]) extends Expression {
  def this(args : Expression*) = this(args.to[ListBuffer])

  override def prettyprint(out : PpStream) : Unit = out << "{ " <<< (arguments, ", ") << " }"
}

case class MemberFunctionCallExpression(var objectName : Expression, var name : String, var arguments : ListBuffer[Expression]) extends Expression {
  def this(objectName : Expression, name : String, args : Expression*) = this(objectName, name, args.to[ListBuffer])

  override def prettyprint(out : PpStream) : Unit = out << objectName << '.' << name << '(' <<< (arguments, ", ") << ')'
}

case class TernaryConditionExpression(var condition : Expression, var trueBody : Expression, var falseBody : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << condition << " ? " << trueBody << " : " << falseBody << ')'
}

case class Reduction(var op : String, var target : VariableAccess) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = Reduction\n"
}

case class StencilConvolution(var stencil : Stencil, var fieldAccess : FieldAccess) extends Expression with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = StencilConvolution\n"

  def resolveEntry(idx : Int) : Expression = {
    stencil.entries(idx).coefficient * new FieldAccess(fieldAccess.fieldSelection, fieldAccess.index + stencil.entries(idx).offset)
  }

  override def expand : Output[Expression] = {
    var ret : Expression = (0 until stencil.entries.size).toArray.map(idx => Duplicate(resolveEntry(idx))).toArray[Expression].reduceLeft(_ + _)
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

  override def expand : Output[Expression] = {
    var ret : Expression = (0 until stencilFieldAccess.stencilFieldSelection.stencil.entries.size).toArray.map(idx => Duplicate(resolveEntry(idx))).toArray[Expression].reduceLeft(_ + _)
    SimplifyStrategy.doUntilDoneStandalone(ret)
    ret
  }
}

case class StencilStencilConvolution(var stencilLeft : Stencil, var stencilRight : Stencil) extends Expression with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = StencilStencilConvolution\n"

  override def expand : Output[StencilAccess] = {
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

  override def expand : Output[StencilAccess] = {
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

case class SIMD_LoadExpression(var mem : Expression, val aligned : Boolean) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    val alig = if (aligned) "" else "u"
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << "_mm_load" << alig << "_p" << prec << '('
      case "AVX" | "AVX2" => out << "_mm256_load" << alig << "_p" << prec << '('
      case "QPX"          => if (aligned) out << "vec_lda(0," else throw new InternalError("QPX does not support unaligned loads")
      case "NEON"         => out << "vld1q_f32(" // TODO: only unaligned?
    }
    out << mem << ')'
  }
}

case class SIMD_Load1Expression(var mem : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << "_mm_load1_p" << prec << '('
      case "AVX" | "AVX2" => out << "_mm256_broadcast_s" << prec << '('
      case "QPX"          => out << "vec_lds(0," // vec_ldsa is only for complex data types (two values)
      case "NEON"         => out << "vld1q_dup_f32(" // TODO: only unaligned?
    }
    out << mem << ')'
  }
}

case class SIMD_ConcShift(var left : VariableAccess, var right : VariableAccess, val offset : Int) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    Knowledge.simd_instructionSet match {
      case "SSE3" =>
        if (Knowledge.useDblPrecision) offset match {
          case 1 => out << "_mm_shuffle_pd(" << left << ", " << right << ", 1);"
        }
        else offset match {
          case 1 => out << "_mm_shuffle_ps(" << left << ", _mm_shuffle_ps(" << right << ", " << left << ", 0x30), 0x29)"
          case 2 => out << "_mm_shuffle_ps(" << left << ", " << right << ", 0x4E)"
          case 3 => out << "_mm_shuffle_ps(_mm_shuffle_ps(" << left << ", " << right << ", 0x3), " << right << ", 0x98)"
        }
      case "AVX" | "AVX2" =>
        if (Knowledge.useDblPrecision) offset match {
          case 1 => out << "_mm256_shuffle_pd(" << left << ", _mm256_permute2f128_pd(" << left << ", " << right << ", 0x21), 0x5)"
          case 2 => out << "_mm256_permute2f128_pd(" << left << ", " << right << ", 0x21)"
          case 3 => out << "_mm256_shuffle_pd(_mm256_permute2f128_pd(" << left << ", " << right << ", 0x21), " << right << ", 0x5)"
        }
        else offset match {
          case 1 => out << "_mm256_permute_ps(_mm256_blend_ps(" << left << ",_mm256_permute2f128_ps(" << left << ", " << right << ", 0x21), 0x11), 0x39)"
          case 2 => out << "_mm256_shuffle_ps(" << left << ", _mm256_permute2f128_ps(" << left << ", " << right << ", 0x21), 0x4E)"
          case 3 => out << "_mm256_permute_ps(_mm256_blend_ps(" << left << ",_mm256_permute2f128_ps(" << left << ", " << right << ", 0x21), 0x77), 0x93)"
          case 4 => out << "_mm256_permute2f128_ps(" << left << ", " << right << ", 0x21)"
          case 5 => out << "_mm256_permute_ps(_mm256_blend_ps(_mm256_permute2f128_ps(" << left << ", " << right << ", 0x21), " << right << ", 0x11), 0x39)"
          case 6 => out << "_mm256_shuffle_ps(_mm256_permute2f128_ps(" << left << ", " << right << ", 0x21), " << right << ", 0x4E)"
          case 7 => out << "_mm256_permute_ps(_mm256_blend_ps(_mm256_permute2f128_ps(" << left << ", " << right << ", 0x21), " << right << ", 0x77), 0x93)"
        }
      case "QPX"  => out << "vec_sldw(" << left << ", " << right << ", " << offset << ")"
      case "NEON" => out << "vextq_f32(" << left << ", " << right << ", " << offset << ")" // TODO: only single precision?
    }
  }
}

case class SIMD_NegateExpression(var vect : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << "_mm_xor_p" << prec << '(' << vect << ", _mm_set1_p" << prec << "(-0.f))"
      case "AVX" | "AVX2" => out << "_mm256_xor_p" << prec << '(' << vect << ", _mm256_set1_p" << prec << "(-0.f))"
      case "QPX"          => out << "vec_neg(" << vect << ')'
      case "NEON"         => out << "vnegq_f32(" << vect << ')'
    }
  }
}

case class SIMD_AdditionExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << "_mm_add_p" << prec
      case "AVX" | "AVX2" => out << "_mm256_add_p" << prec
      case "QPX"          => out << "vec_add"
      case "NEON"         => out << "vaddq_f32"
    }
    out << '(' << left << ", " << right << ')'
  }
}

case class SIMD_SubtractionExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << "_mm_sub_p" << prec
      case "AVX" | "AVX2" => out << "_mm256_sub_p" << prec
      case "QPX"          => out << "vec_sub"
      case "NEON"         => out << "vsubq_f32"
    }
    out << '(' << left << ", " << right << ')'
  }
}

case class SIMD_MultiplicationExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << "_mm_mul_p" << prec
      case "AVX" | "AVX2" => out << "_mm256_mul_p" << prec
      case "QPX"          => out << "vec_mul"
      case "NEON"         => out << "vmulq_f32"
    }
    out << '(' << left << ", " << right << ')'
  }
}

case class SIMD_MultiplyAddExpression(var factor1 : Expression, var factor2 : Expression,
    var summand : Expression) extends Expression {

  override def prettyprint(out : PpStream) : Unit = {
    FusedPrinterHelper.prettyprint(out, factor1, factor2, summand, "add")
  }
}

case class SIMD_MultiplySubExpression(var factor1 : Expression, var factor2 : Expression,
    var summand : Expression) extends Expression {

  override def prettyprint(out : PpStream) : Unit = {
    FusedPrinterHelper.prettyprint(out, factor1, factor2, summand, "sub")
  }
}

private object FusedPrinterHelper {
  def prettyprint(out : PpStream, factor1 : Expression, factor2 : Expression, summand : Expression, addSub : String) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Knowledge.simd_instructionSet match {
      case "SSE3" => out << "_mm_" << addSub << "_p" << prec << "(_mm_mul_p" << prec << '(' << factor1 << ", " << factor2 << "), " << summand << ')'
      case "AVX"  => out << "_mm256_" << addSub << "_p" << prec << "(_mm256_mul_p" << prec << '(' << factor1 << ", " << factor2 << "), " << summand << ')'
      case "AVX2" => out << "_mm256_fm" << addSub << "_p" << prec << '(' << factor1 << ", " << factor2 << ", " << summand << ')'
      case "QPX"  => out << "vec_m" << addSub << '(' << factor1 << ", " << factor2 << ", " << summand << ')'
      case "NEON" =>
        if (addSub == "add")
          out << "vmlaq_f32(" << summand << ", " << factor1 << ", " << factor2 << ')' // use unfused for compatibility with gcc 4.7 and older
        else // vmlsq_f32(a,b,c) is a-b*c and not a*b-c; thanks ARM  -.-
          out << "vsubq_f32(vmulq_f32(" << factor1 << ", " << factor2 << "), " << summand << ')'
    }
  }
}

case class SIMD_DivisionExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << "_mm_div_p" << prec
      case "AVX" | "AVX2" => out << "_mm256_div_p" << prec
      case "QPX"          => out << "vec_swdiv_nochk" // double precision division performed here, single precision would also be possible... what's better?
      case "NEON"         => out << "vdivq_f32"
    }
    out << '(' << left << ", " << right << ')'
  }
}

case class SIMD_FloatConstant(var value : Double) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << "_mm_set1_p" << prec
      case "AVX" | "AVX2" => out << "_mm256_set1_p" << prec
      case "QPX"          => out << "vec_splats"
      case "NEON"         => out << "vdupq_n_f32"
    }
    out << '(' << value << ')' // this uses value.toString(), which is Locale-independent and the string can be parsed without a loss of precision later
  }
}

case class SIMD_Scalar2VectorExpression(var scalar : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Knowledge.simd_instructionSet match {
      case "SSE3"         => out << "_mm_set1_p" << prec
      case "AVX" | "AVX2" => out << "_mm256_set1_p" << prec
      case "QPX"          => out << "vec_splats"
      case "NEON"         => out << "vdupq_n_f32"
    }
    out << '(' << scalar << ')'
  }
}
