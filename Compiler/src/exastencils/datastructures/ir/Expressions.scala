package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.logger._
import exastencils.prettyprinting._
import exastencils.strategies._

trait Expression extends Node with PrettyPrintable {
  @deprecated("should be removed completely, since it complicates AST analysis for transformations/optimization; please, don't use it in new code", "14.04.2016")
  def ~(exp : Expression) : ConcatenationExpression = {
    new ConcatenationExpression(this, exp)
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
  def <<(other : Expression) = new LeftShiftExpression(this, other)
}

object BinaryOperators extends Enumeration {
  type BinaryOperators = Value
  val Addition = Value("+")
  val Subtraction = Value("-")
  val Multiplication = Value("*")
  val Division = Value("/")
  val Power = Value("**")
  val Power_Alt = Value("^")
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
  val Maximum = Value("max")
  val Minimum = Value("min")
  val BitwiseAnd = Value("&")
  val LeftShift = Value("<<")

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
    case Power_Alt                 => return new PowerExpression(left, right)
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
    case Maximum                   => return new MaximumExpression(left, right)
    case Minimum                   => return new MinimumExpression(left, right)
    case BitwiseAnd                => return new BitwiseAndExpression(left, right)
    case LeftShift                 => return new LeftShiftExpression(left, right)
  }

  def opAsIdent(op : String) = {
    op match {
      case "+"   => "Addition"
      case "-"   => "Subtraction"
      case "*"   => "Multiplication"
      case "/"   => "Division"
      case "**"  => "Power"
      case "^"   => "Power_Alt"
      case "%"   => "Modulo"

      case ".+"  => "ElementwiseAddition"
      case ".-"  => "ElementwiseSubtraction"
      case ".*"  => "ElementwiseMultiplication"
      case "./"  => "ElementwiseDivision"
      case ".**" => "ElementwisePower"
      case ".%"  => "ElementwiseModulo"

      case "&&"  => "AndAnd"
      case "and" => "AndAndWritten"
      case "||"  => "OrOr"
      case "or"  => "OrOrWritten"
      case "!"   => "Negation"
      case "=="  => "EqEq"
      case "!="  => "Neq"
      case "<"   => "Lower"
      case "<="  => "LowerEqual"
      case ">"   => "Greater"
      case ">="  => "GreaterEqual"
      case "max" => "Maximum"
      case "min" => "Minimum"
      case "&"   => "BitwiseAnd"

      case _     => Logger.warn(s"Unknown op $op"); op
    }
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

@deprecated("should be removed completely, since it complicates AST analysis for transformations/optimization; please, don't use it in new code", "14.04.2016")
case class ConcatenationExpression(var expressions : ListBuffer[Expression]) extends Expression {
  def this(exprs : Expression*) = this(exprs.to[ListBuffer])

  override def prettyprint(out : PpStream) : Unit = out <<< expressions

  @deprecated("should be removed completely, since it complicates AST analysis for transformations/optimization; please, don't use it in new code", "14.04.2016")
  override def ~(exp : Expression) : ConcatenationExpression = {
    expressions += exp
    this
  }
}

case class StringLiteral(var value : String) extends Expression {
  def this(s : StringConstant) = this(s.value)
  override def prettyprint(out : PpStream) : Unit = out << value
  override def toString : String = value
}

case class StringConstant(var value : String) extends Expression {
  def this(s : StringLiteral) = this(s.value)
  override def prettyprint(out : PpStream) : Unit = out << '"' << value << '"'
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
  def isConstant = expressions.forall(e => e.isInstanceOf[Number])

  def prettyprintInner(out : PpStream) : Unit = {
    out << (if (Platform.targetCompiler == "GCC") "std::move((" else "((")
    out << datatype.getOrElse(RealDatatype) << "[]){" <<< (expressions, ",") << "})"
  }
  override def prettyprint(out : PpStream) : Unit = {
    out << "Matrix<" << datatype.getOrElse(RealDatatype) << ", "
    if (rowVector.getOrElse(true)) {
      out << "1, " << length << "> (" // row vector
    } else {
      out << length << ", 1> ("
    }
    prettyprintInner(out)
    out << ')'
  }
}

case class MatrixExpression(var datatype : Option[Datatype], var expressions : ListBuffer[ListBuffer[Expression]]) extends Expression {
  def prettyprintInner(out : PpStream) : Unit = {
    out << (if (Platform.targetCompiler == "GCC") "std::move((" else "((")
    out << datatype.getOrElse(RealDatatype) << "[]){" <<< (expressions.flatten, ",") << "})"
  }
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) "double" else "float"

    out << "Matrix<" << (if (isInteger) "int" else prec) << ", " << rows << ", " << columns << "> ("
    prettyprintInner(out)
    out << ")"
  }

  def rows = expressions.length
  def columns = expressions(0).length

  def apply(i : Integer) = expressions(i)
  def isConstant = expressions.flatten.forall(e => e.isInstanceOf[Number])
  def isInteger = expressions.flatten.forall(e => e.isInstanceOf[IntegerConstant])
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
  def this(decl : VariableDeclarationStatement) = this(decl.name, decl.dataType)

  override def prettyprint(out : PpStream) : Unit = out << name

  def printDeclaration() : String = dType.get.resolveDeclType.prettyprint + " " + name + dType.get.resolveDeclPostscript
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

case class BoundedExpression(var min : Long, var max : Long, var expr : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = BoundedExpression(" << expr << ')'

  def expandSpecial() : Expression = {
    return expr
  }
}

case class MultiIndex(var indices : Array[Expression]) extends Expression with Iterable[Expression] {
  def this(indices : Expression*) = this(indices.toArray)
  def this(indices : Array[Int]) = this(indices.map(IntegerConstant(_) : Expression)) // legacy support
  def this(indices : Array[Long]) = this(indices.map(IntegerConstant(_) : Expression)) // legacy support
  def this(left : MultiIndex, right : MultiIndex, f : (Expression, Expression) => Expression) =
    this((0 until math.min(left.indices.length, right.indices.length)).map(i => Duplicate(f(left(i), right(i)))).toArray)

  // FIXME: add variable accesses to begin with...
  for (i <- 0 until length) {
    this(i) = this(i) match {
      case StringLiteral(s) => VariableAccess(s, Some(IntegerDatatype))
      case _                => this(i)
    }
  }

  override def prettyprint(out : PpStream) : Unit = {
    out << '[' <<< (this, ", ") << ']'
  }

  def +(that : MultiIndex) : MultiIndex = new MultiIndex(this, that, _ + _)
  def -(that : MultiIndex) : MultiIndex = new MultiIndex(this, that, _ - _)

  override def equals(other : Any) : Boolean = {
    if (this eq other.asInstanceOf[AnyRef])
      return true
    return other match {
      case MultiIndex(oIndices) => java.util.Arrays.equals(this.indices.asInstanceOf[Array[Object]], oIndices.asInstanceOf[Array[Object]])
      case _                    => false
    }
  }

  override def hashCode() : Int = {
    return java.util.Arrays.hashCode(indices.asInstanceOf[Array[Object]]) * 31 + 42 // random modification to ensure the hashcode of this element differs from the hashcode of the array itself
  }

  // expose array functions
  override def iterator() : scala.collection.Iterator[Expression] = indices.iterator

  def apply(i : Int) = indices.apply(i)
  def update(i : Int, x : Expression) = indices.update(i, x)
  def length = indices.length
}

case class TempBufferAccess(var buffer : iv.TmpBuffer, var index : MultiIndex, var strides : MultiIndex) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = TempBufferAccess\n"

  def linearize : ArrayAccess = {
    new ArrayAccess(buffer,
      Mapping.resolveMultiIdx(index, strides),
      false) // Knowledge.data_alignTmpBufferPointers) // change here if aligned vector operations are possible for tmp buffers
  }
}

case class ReductionDeviceDataAccess(var data : iv.ReductionDeviceData, var index : MultiIndex, var strides : MultiIndex) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = ReductionDeviceDataAccess\n"

  def linearize : ArrayAccess = {
    new ArrayAccess(data, Mapping.resolveMultiIdx(index, strides), false)
  }
}

case class LoopCarriedCSBufferAccess(var buffer : iv.LoopCarriedCSBuffer, var index : MultiIndex) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopCarriedCSEBufferAccess\n"

  def linearize() : ArrayAccess = {
    if (buffer.dimSizes.isEmpty)
      return new ArrayAccess(buffer, IntegerConstant(0), false)

    return new ArrayAccess(buffer, Mapping.resolveMultiIdx(index, buffer.dimSizes), false)
  }
}

abstract class FieldAccessLike extends Expression {
  def fieldSelection : FieldSelection
  def index : MultiIndex
}

case class DirectFieldAccess(var fieldSelection : FieldSelection, var index : MultiIndex) extends FieldAccessLike {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = DirectFieldAccess\n"

  def linearize : LinearizedFieldAccess = {
    new LinearizedFieldAccess(fieldSelection, Mapping.resolveMultiIdx(fieldSelection.fieldLayout, index))
  }
}

case class FieldAccess(var fieldSelection : FieldSelection, var index : MultiIndex) extends FieldAccessLike {
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
    if (Knowledge.generateFortranInterface) { // Fortran requires multi-index access to multidimensional arrays
      val it = LoopOverDimensions.defIt(field.fieldLayout.numDimsData)
      var ret = name
      for (dim <- field.fieldLayout.numDimsData - 1 to 0)
        ret = new ArrayAccess(ret, it(dim), false)
      ret.asInstanceOf[ArrayAccess]
    } else
      new ArrayAccess(name, Mapping.resolveMultiIdx(field.fieldLayout, index), false)
  }
}

case class LinearizedFieldAccess(var fieldSelection : FieldSelection, var index : Expression) extends Expression with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LinearizedFieldAccess\n"

  override def expand() : Output[Expression] = {
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
      val stencilFieldIdx = Duplicate(index)
      stencilFieldIdx(stencilFieldSelection.stencilField.field.fieldLayout.numDimsData - 1) = e // TODO: assumes last index is vector dimension
      val fieldSel = stencilFieldSelection.toFieldSelection
      fieldSel.arrayIndex = Some(e)
      entries += new StencilEntry(stencilFieldSelection.stencil.entries(e).offset, new FieldAccess(fieldSel, stencilFieldIdx))
    }
    new Stencil("GENERATED_PLACEHOLDER_STENCIL", stencilFieldSelection.stencil.level, entries)
  }
}

case class MemberAccess(var base : Access, var member : String) extends Access {
  override def prettyprint(out : PpStream) : Unit = out << base << '.' << member
}

case class DerefAccess(var base : Access) extends Access {
  override def prettyprint(out : PpStream) : Unit = out << "(*" << base << ')'
}

case class AdditionExpression(var summands : ListBuffer[Expression]) extends Expression {
  def this(left : Expression, right : Expression) = this(ListBuffer(left, right))
  override def prettyprint(out : PpStream) : Unit = out << '(' <<< (summands, "+") << ')'
}

case class SubtractionExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << '-' << right << ')'
}

case class MultiplicationExpression(var factors : ListBuffer[Expression]) extends Expression {
  def this(left : Expression, right : Expression) = this(ListBuffer(left, right))
  override def prettyprint(out : PpStream) : Unit = out << '(' <<< (factors, "*") << ')'
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

case class LeftShiftExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "<<" << right << ')'
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

    else if (Platform.supports_initializerList && PrintEnvironment.CPP == out.env)
      out << method << "({" <<< (args, ",") << "})"

    else {
      for (i <- 0 until args.length - 1)
        out << method << '('
      val it : Iterator[Expression] = args.iterator
      out << "static_cast<double>(" << it.next() << ')'
      while (it.hasNext)
        out << ", static_cast<double>(" << it.next() << ')' << ')'
    }
  }
}

case class MinimumExpression(var args : ListBuffer[Expression]) extends Expression {
  def this(varargs : Expression*) = this(varargs.to[ListBuffer])

  override def prettyprint(out : PpStream) : Unit = {
    import PrintEnvironment._
    val name = if (out.env == CUDA) "fmin" else "std::min"
    MinMaxPrinter.prettyprintsb(out, args, name)
  }
}

case class MaximumExpression(var args : ListBuffer[Expression]) extends Expression {
  def this(varargs : Expression*) = this(varargs.to[ListBuffer])

  override def prettyprint(out : PpStream) : Unit = {
    import PrintEnvironment._
    val name = if (out.env == CUDA) "fmax" else "std::max"
    MinMaxPrinter.prettyprintsb(out, args, name)
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

  override def expand() : Output[Expression] = {
    val ret : Expression = stencil.entries.indices.view.map(idx => Duplicate(resolveEntry(idx))).reduceLeft(_ + _)
    SimplifyStrategy.doUntilDoneStandalone(ret)
    ret
  }
}

// TODO: update convolutions with new dimensionality logic

case class StencilFieldConvolution(var stencilFieldAccess : StencilFieldAccess, var fieldAccess : FieldAccess) extends Expression with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = StencilConvolution\n"

  def resolveEntry(idx : Int) : Expression = {
    val stencilFieldIdx = Duplicate(stencilFieldAccess.index)
    stencilFieldIdx(Knowledge.dimensionality) = idx

    FieldAccess(stencilFieldAccess.stencilFieldSelection.toFieldSelection, stencilFieldIdx) *
      new FieldAccess(fieldAccess.fieldSelection, fieldAccess.index + stencilFieldAccess.stencilFieldSelection.stencil.entries(idx).offset)
  }

  override def expand() : Output[Expression] = {
    val ret : Expression = stencilFieldAccess.stencilFieldSelection.stencil.entries.indices.view.map(idx => Duplicate(resolveEntry(idx))).reduceLeft(_ + _)
    SimplifyStrategy.doUntilDoneStandalone(ret)
    ret
  }
}

case class StencilStencilConvolution(var stencilLeft : Stencil, var stencilRight : Stencil) extends Expression with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = StencilStencilConvolution\n"

  override def expand() : Output[StencilAccess] = {
    var entries : ListBuffer[StencilEntry] = ListBuffer()

    for (re <- stencilRight.entries) {
      for (le <- stencilLeft.entries) {
        val rightOffset = Duplicate(re.offset)

        val leftOffset = Duplicate(le.offset)
        if (stencilRight.level > stencilLeft.level) {
          for (d <- 0 until Knowledge.dimensionality)
            leftOffset(d) = (dimToString(d) : Expression) / 2 + leftOffset(d)
        } else {
          for (d <- 0 until Knowledge.dimensionality)
            leftOffset(d) = (dimToString(d) : Expression) + leftOffset(d)
        }

        val combOff = leftOffset
        ResolveCoordinates.replacement = rightOffset
        ResolveCoordinates.doUntilDoneStandalone(combOff)

        var combCoeff : Expression = (re.coefficient * le.coefficient)
        SimplifyStrategy.doUntilDoneStandalone(combOff)
        SimplifyStrategy.doUntilDoneStandalone(combCoeff)
        val addToEntry = entries.find(e => e.offset match { case o if (combOff == o) => true; case _ => false })
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

  override def expand() : Output[StencilAccess] = {
    var entries : ListBuffer[StencilEntry] = ListBuffer()

    for (re <- stencilRight.entries) {
      for (e <- 0 until stencilLeft.stencilFieldSelection.stencil.entries.size) {
        val stencilFieldIdx = Duplicate(stencilLeft.index)
        stencilFieldIdx(Knowledge.dimensionality) = e
        for (dim <- 0 until Knowledge.dimensionality)
          stencilFieldIdx(dim) += re.offset(dim)
        val fieldSel = stencilLeft.stencilFieldSelection.toFieldSelection
        fieldSel.arrayIndex = Some(e)

        val rightOffset = Duplicate(re.offset)

        val leftOffset = Duplicate(stencilLeft.stencilFieldSelection.stencil.entries(e).offset)
        if (stencilRight.level > stencilLeft.stencilFieldSelection.stencil.level) {
          for (d <- 0 until Knowledge.dimensionality)
            leftOffset(d) = (dimToString(d) : Expression) / 2 + leftOffset(d)
        } else {
          for (d <- 0 until Knowledge.dimensionality)
            leftOffset(d) = (dimToString(d) : Expression) + leftOffset(d)
        }

        val combOff = leftOffset
        ResolveCoordinates.replacement = rightOffset
        ResolveCoordinates.doUntilDoneStandalone(combOff)

        var combCoeff : Expression = (re.coefficient * new FieldAccess(fieldSel, stencilFieldIdx))
        SimplifyStrategy.doUntilDoneStandalone(combOff)
        SimplifyStrategy.doUntilDoneStandalone(combCoeff)
        val addToEntry = entries.find(e => e.offset match { case o if (combOff == o) => true; case _ => false })
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
    Platform.simd_instructionSet match {
      case "SSE3"         => out << "_mm_load" << alig << "_p" << prec << '('
      case "AVX" | "AVX2" => out << "_mm256_load" << alig << "_p" << prec << '('
      case "AVX512"       => out << "_mm512_load" << alig << "_p" << prec << '('
      case "IMCI"         => if (aligned) out << "_mm512_load_p" << prec << '(' else throw new InternalError("IMCI does not support unaligned loads")
      case "QPX"          => if (aligned) out << "vec_lda(0," else throw new InternalError("QPX does not support unaligned loads")
      case "NEON"         => out << "vld1q_f32(" // TODO: only unaligned?
    }
    out << mem << ')'
  }
}

case class SIMD_Load1Expression(var mem : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Platform.simd_instructionSet match {
      case "SSE3"         => out << "_mm_load1_p" << prec << '('
      case "AVX" | "AVX2" => out << "_mm256_broadcast_s" << prec << '('
      case "AVX512"       => out << "_mm512_set1_p" << prec << "(*" // TODO: check again: no direct load possible?
      case "IMCI"         => out << "_mm512_extload_p" << prec << '('
      case "QPX"          => out << "vec_lds(0," // vec_ldsa is only for complex data types (two values)
      case "NEON"         => out << "vld1q_dup_f32(" // TODO: only unaligned?
    }
    out << mem
    if (Platform.simd_instructionSet == "IMCI") {
      if (Knowledge.useDblPrecision)
        out << "_MM_UPCONV_PD_NONE, _MM_BROADCAST_1X8, 0"
      else
        out << "_MM_UPCONV_PS_NONE, _MM_BROADCAST_1X16, 0"
    }
    out << ')'
  }
}

case class SIMD_ConcShift(var left : VariableAccess, var right : VariableAccess, val offset : Int) extends Expression {
  private var shiftIV : iv.VecShiftIndex = null

  Platform.simd_instructionSet match {
    case "AVX512" | "IMCI" => shiftIV = new iv.VecShiftIndex(offset)
    case _                 =>
  }

  override def prettyprint(out : PpStream) : Unit = {
    Platform.simd_instructionSet match {
      case "SSE3" =>
        if (Knowledge.useDblPrecision) offset match {
          case 1 => out << "_mm_shuffle_pd(" << left << ", " << right << ", 1)"
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

      case "AVX512" =>
        if (offset <= 0 || offset >= Platform.simd_vectorSize)
          throw new InternalError("offset for SIMD_ConcShift out of bounds: " + offset)
        val prec = if (Knowledge.useDblPrecision) 'd' else 's'
        out << "_mm512_permutex2var_p" << prec << '(' << left << ", " << shiftIV << ", " << right << ')'

      case "IMCI" =>
        if (Knowledge.useDblPrecision) offset match {
          case 1 => out << "_mm512_castsi512_pd(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castpd_si512(_mm512_mask_blend_pd(0x01, " << left << ", " << right << "))))"
          case 2 => out << "_mm512_castsi512_pd(_mm512_permute4f128_epi32(_mm512_castpd_si512(_mm512_mask_blend_pd(0x03, " << left << ", " << right << ")), _MM_PERM_ADCB))"
          case 3 => out << "_mm512_castsi512_pd(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castpd_si512(_mm512_mask_blend_pd(0x07, " << left << ", " << right << "))))"
          case 4 => out << "_mm512_castsi512_pd(_mm512_permute4f128_epi32(_mm512_castpd_si512(_mm512_mask_blend_pd(0x0F, " << left << ", " << right << ")), _MM_PERM_BADC))"
          case 5 => out << "_mm512_castsi512_pd(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castpd_si512(_mm512_mask_blend_pd(0x1F, " << left << ", " << right << "))))"
          case 6 => out << "_mm512_castsi512_pd(_mm512_permute4f128_epi32(_mm512_castpd_si512(_mm512_mask_blend_pd(0x3F, " << left << ", " << right << ")), _MM_PERM_CBAD))"
          case 7 => out << "_mm512_castsi512_pd(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castpd_si512(_mm512_mask_blend_pd(0x7F, " << left << ", " << right << "))))"
        }
        else offset match {
          case 1  => out << "_mm512_castsi512_ps(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castps_si512(_mm512_mask_blend_ps(0x0001, " << left << ", " << right << "))))"
          case 2  => out << "_mm512_castsi512_ps(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castps_si512(_mm512_mask_blend_ps(0x0003, " << left << ", " << right << "))))"
          case 3  => out << "_mm512_castsi512_ps(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castps_si512(_mm512_mask_blend_ps(0x0007, " << left << ", " << right << "))))"
          case 4  => out << "_mm512_castsi512_ps(_mm512_permute4f128_epi32(_mm512_castpd_si512(_mm512_mask_blend_pd(0x000F, " << left << ", " << right << ")), _MM_PERM_ADCB))"
          case 5  => out << "_mm512_castsi512_ps(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castps_si512(_mm512_mask_blend_ps(0x001F, " << left << ", " << right << "))))"
          case 6  => out << "_mm512_castsi512_ps(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castps_si512(_mm512_mask_blend_ps(0x003F, " << left << ", " << right << "))))"
          case 7  => out << "_mm512_castsi512_ps(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castps_si512(_mm512_mask_blend_ps(0x007F, " << left << ", " << right << "))))"
          case 8  => out << "_mm512_castsi512_ps(_mm512_permute4f128_epi32(_mm512_castpd_si512(_mm512_mask_blend_pd(0x00FF, " << left << ", " << right << ")), _MM_PERM_BADC))"
          case 9  => out << "_mm512_castsi512_ps(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castps_si512(_mm512_mask_blend_ps(0x01FF, " << left << ", " << right << "))))"
          case 10 => out << "_mm512_castsi512_ps(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castps_si512(_mm512_mask_blend_ps(0x03FF, " << left << ", " << right << "))))"
          case 11 => out << "_mm512_castsi512_ps(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castps_si512(_mm512_mask_blend_ps(0x07FF, " << left << ", " << right << "))))"
          case 12 => out << "_mm512_castsi512_ps(_mm512_permute4f128_epi32(_mm512_castpd_si512(_mm512_mask_blend_pd(0x0FFF, " << left << ", " << right << ")), _MM_PERM_CBAD))"
          case 13 => out << "_mm512_castsi512_ps(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castps_si512(_mm512_mask_blend_ps(0x1FFF, " << left << ", " << right << "))))"
          case 14 => out << "_mm512_castsi512_ps(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castps_si512(_mm512_mask_blend_ps(0x3FFF, " << left << ", " << right << "))))"
          case 15 => out << "_mm512_castsi512_ps(_mm512_permutevar_epi32(" << shiftIV << ", _mm512_castps_si512(_mm512_mask_blend_ps(0x7FFF, " << left << ", " << right << "))))"
        }

      case "QPX"  => out << "vec_sldw(" << left << ", " << right << ", " << offset << ")"
      case "NEON" => out << "vextq_f32(" << left << ", " << right << ", " << offset << ")" // TODO: only single precision?
    }
  }
}

case class SIMD_NegateExpression(var vect : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    val (prec, ts, fp) = if (Knowledge.useDblPrecision) ('d', "d", 'd') else ('s', "", 'f')
    Platform.simd_instructionSet match {
      case "SSE3"         => out << "_mm_xor_p" << prec << '(' << vect << ", _mm_set1_p" << prec << "(-0." << fp << "))"
      case "AVX" | "AVX2" => out << "_mm256_xor_p" << prec << '(' << vect << ", _mm256_set1_p" << prec << "(-0." << fp << "))"
      case "AVX512"       => out << "_mm512_xor_p" << prec << '(' << vect << ", _mm512_set1_p" << prec << "(-0." << fp << "))"
      case "IMCI"         => out << "_mm512_sub_p" << prec << "((__m512" << ts << ") 0, " << vect << ")" // TODO: is there a more efficient version?
      case "QPX"          => out << "vec_neg(" << vect << ')'
      case "NEON"         => out << "vnegq_f32(" << vect << ')'
    }
  }
}

case class SIMD_AdditionExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Platform.simd_instructionSet match {
      case "SSE3"            => out << "_mm_add_p" << prec
      case "AVX" | "AVX2"    => out << "_mm256_add_p" << prec
      case "AVX512" | "IMCI" => out << "_mm512_add_p" << prec
      case "QPX"             => out << "vec_add"
      case "NEON"            => out << "vaddq_f32"
    }
    out << '(' << left << ", " << right << ')'
  }
}

case class SIMD_SubtractionExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Platform.simd_instructionSet match {
      case "SSE3"            => out << "_mm_sub_p" << prec
      case "AVX" | "AVX2"    => out << "_mm256_sub_p" << prec
      case "AVX512" | "IMCI" => out << "_mm512_sub_p" << prec
      case "QPX"             => out << "vec_sub"
      case "NEON"            => out << "vsubq_f32"
    }
    out << '(' << left << ", " << right << ')'
  }
}

case class SIMD_MultiplicationExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Platform.simd_instructionSet match {
      case "SSE3"            => out << "_mm_mul_p" << prec
      case "AVX" | "AVX2"    => out << "_mm256_mul_p" << prec
      case "AVX512" | "IMCI" => out << "_mm512_mul_p" << prec
      case "QPX"             => out << "vec_mul"
      case "NEON"            => out << "vmulq_f32"
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
    Platform.simd_instructionSet match {
      case "SSE3"            => out << "_mm_" << addSub << "_p" << prec << "(_mm_mul_p" << prec << '(' << factor1 << ", " << factor2 << "), " << summand << ')'
      case "AVX"             => out << "_mm256_" << addSub << "_p" << prec << "(_mm256_mul_p" << prec << '(' << factor1 << ", " << factor2 << "), " << summand << ')'
      case "AVX2"            => out << "_mm256_fm" << addSub << "_p" << prec << '(' << factor1 << ", " << factor2 << ", " << summand << ')'
      case "AVX512" | "IMCI" => out << "_mm512_fm" << addSub << "_p" << prec << '(' << factor1 << ", " << factor2 << ", " << summand << ')'
      case "QPX"             => out << "vec_m" << addSub << '(' << factor1 << ", " << factor2 << ", " << summand << ')'
      case "NEON" =>
        if (addSub == "add")
          out << "vmlaq_f32(" << summand << ", " << factor1 << ", " << factor2 << ')' // use unfused for compatibility with gcc 4.7 and older
        else // vmlsq_f32(a,b,c) is a-b*c and not a*b-c; thanks ARM  -.-
          out << "vnegq_f32(vmlsq_f32(" << summand << ", " << factor1 << ", " << factor2 << "))"
    }
  }
}

case class SIMD_DivisionExpression(var left : Expression, var right : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Platform.simd_instructionSet match {
      case "SSE3"         => out << "_mm_div_p" << prec
      case "AVX" | "AVX2" => out << "_mm256_div_p" << prec
      case "AVX512"       => out << "_mm512_div_p" << prec
      case "IMCI"         => throw new InternalError("not yet supported...") // TODO: support it! but there is no div :(
      case "QPX"          => out << "vec_swdiv_nochk" // double precision division performed here, single precision would also be possible... what's better?
      case "NEON"         => out << "vdivq_f32"
    }
    out << '(' << left << ", " << right << ')'
  }
}

case class SIMD_FloatConstant(var value : Double) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Platform.simd_instructionSet match {
      case "SSE3"         => out << "_mm_set1_p" << prec
      case "AVX" | "AVX2" => out << "_mm256_set1_p" << prec
      case "AVX512"       => out << "_mm512_set1_p" << prec
      case "QPX"          => out << "vec_splats"
      case "NEON"         => out << "vdupq_n_f32"
      case "IMCI"         => out << "_mm512_set_1to" << Platform.simd_vectorSize << "_p" << prec
    }
    out << '(' << value << ')' // this uses value.toString(), which is Locale-independent and the string can be parsed without a loss of precision later
  }
}

case class SIMD_Scalar2VectorExpression(var scalar : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Platform.simd_instructionSet match {
      case "SSE3"         => out << "_mm_set1_p" << prec
      case "AVX" | "AVX2" => out << "_mm256_set1_p" << prec
      case "AVX512"       => out << "_mm512_set1_p" << prec
      case "QPX"          => out << "vec_splats"
      case "NEON"         => out << "vdupq_n_f32"
      case "IMCI"         => out << "_mm512_set_1to" << Platform.simd_vectorSize << "_p" << prec
    }
    out << '(' << scalar << ')'
  }
}
