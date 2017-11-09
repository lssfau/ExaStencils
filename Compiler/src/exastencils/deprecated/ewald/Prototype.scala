package exastencils.deprecated.ewald

import scala.reflect.ClassTag

object BoundaryCondition {
  def apply(eq : Equation) = {
    val left = eq.left
    val right = eq.right
    left match {
      case u : Solution         => Dirichlet(right)
      case n : NormalDerivation => Neumann(right)
      case _                    => throw new IllegalArgumentException(
        s"""|Allowed boundary conditions are:
            |\t- Dirichlets\n\t- Neumann.""".stripMargin)
    }
  }
}

sealed abstract class BoundaryCondition {
  val func : MathTree
}

case class Dirichlet(val func : MathTree) extends BoundaryCondition

case class Neumann(val func : MathTree) extends BoundaryCondition

case class Boundary(
    val condition : BoundaryCondition,
    val area : BoundaryTree) {
  override def toString = s"Boundary: $condition for $area"
}

sealed abstract class BoundaryTree

case class Whole() extends BoundaryTree

case class And(
    left : BoundaryTree,
    right : BoundaryTree
) extends BoundaryTree

case class Or(
    left : BoundaryTree,
    right : BoundaryTree
) extends BoundaryTree

case class IntervalAtDim(
    dim : Int,
    int : IntervalLike,
    including : Boolean
) extends BoundaryTree

case class Inequality(
    left : MathTree,
    right : MathTree,
    relation : Relation
) extends BoundaryTree

sealed abstract class Relation

case object EQ extends Relation

case object LESS extends Relation

case object LESSEQ extends Relation

case object GREATER extends Relation

case object GREATEREQ extends Relation

object BoundaryTree {

}

class Domain(val intervals : Seq[Interval]) {
  def this(interval : Interval) = this(List(interval))
  def this() = this(List())

  require(intervals forall { _.isOpen }, "Domain must be a product of OPEN intervals!")
  require(intervals forall { !_.isPoint }, "Domain must not contain single points!")

  def +(other : Domain) = new Domain(this.intervals ++ other.intervals)
  def +(interval : Interval) = new Domain(this.intervals :+ interval)

  def dimCount = intervals.length

  def interval(index : Int) = intervals(index)

  override def toString = "\\Omega = " + intervals.mkString(" \\times ")
}

trait ExprStencilParam {
  def dimCount = 0
  def len = 1
  def *(e : MathTree) : ExprStencilParam
  def /(e : MathTree) : ExprStencilParam
}

final class ExprStencil(val entries : Seq[ExprStencilParam]) extends ExprStencilParam {
  override def len = entries.length
  assert(len > 0)
  override def dimCount = entries(0).dimCount + 1

  def +(other : ExprStencil) : ExprStencil = {
    val newEntries = if (dimCount == 1) {
      for (i <- 0 until this.len) yield entries(i).asInstanceOf[MathTree] + other.entries(i).asInstanceOf[MathTree]
    } else {
      for (i <- 0 until this.len) yield entries(i).asInstanceOf[ExprStencil] + other.entries(i).asInstanceOf[ExprStencil]
    }
    new ExprStencil(newEntries)

  }

  def -(other : ExprStencil) : ExprStencil = this + (other * Value(-1))

  def /(e : MathTree) = new ExprStencil(for (entry <- entries) yield entry / e)

  def *(e : MathTree) = new ExprStencil(for (entry <- entries) yield entry * e)

  override def toString = dimCount match {
    case 1 => "[" + entries.map { _.toString }.reduceLeft { (x, y) => x + ", " + y } + "]\n"
    case 2 => entries.foldLeft("") { (x, y) => x + y.toString }
    //case 3 => (entries map { _.toString }) mkString "\n"
    case _ => {
      val stringList = for (i <- 0 until len) yield "[" + (i) + "]:\n" + entries(i).toString
      stringList.mkString("\n")
    }
  }

}

sealed abstract class FddParameter {
  val dim : Int
  val value : Any
  // copy constructor of the case classes
  // needs to be called explicitly
  def useAtDim(dim : Int) : FddParameter = this match {
    case x : Direction  => x.copy(dim = dim)
    case x : ErrorOrder => x.copy(dim = dim)
    case x : GridPoints => x.copy(dim = dim)
    case x : StepSize   => x.copy(dim = dim)
  }
}

// dim = -1 represents default value for dimensions without given parameter value
case class Direction(val dim : Int, val value : FDDirection) extends FddParameter

case class ErrorOrder(val dim : Int, val value : Int) extends FddParameter

case class GridPoints(val dim : Int, val value : Int) extends FddParameter

case class StepSize(val dim : Int, val value : Double) extends FddParameter

// enumeration of finite difference directions
sealed abstract class FDDirection

case object FORWARD extends FDDirection

case object BACKWARD extends FDDirection

case object CENTRAL extends FDDirection

class FddParameters(val domain : Domain, parsedParameters : List[FddParameter]) {
  implicit val defaultDirection = Direction(0, CENTRAL)
  implicit val defaultErrorOrder = ErrorOrder(0, 2)
  implicit val defaultGridPoints = GridPoints(0, 1025)

  val directions = getParameter[Direction]
  val errorOrders = getParameter[ErrorOrder]
  val gridPoints = getParameter[GridPoints]
  validate()
  val stepSize = gridPoints map {
    n => val range = domain.interval(n.dim); StepSize(n.dim, (range.x1 - range.x0) / (n.value - 1))
  }

  private[this] def getParameter[T <: FddParameter](implicit ct : ClassTag[T], default : T) : List[T] = {
    // extract parameters of type T
    val parsedParameterType = parsedParameters.flatMap {
      _ match {
        case _ : StepSize => throw new RuntimeException(" method not supported for StepSize")
        case value : T    => Some(value)
        case _            => None
      }
    }

    // extract information
    val userGivenDefault = parsedParameterType find { _.dim == -1 }

    val parameterValues = for {i <- 0 until domain.dimCount} yield {
      parsedParameterType.find { _.dim == i } match {
        case Some(parameter) => parameter
        case None            => userGivenDefault match {
          case Some(parameter) => parameter.useAtDim(i).asInstanceOf[T]
          case None            => default match {
            case default : Direction  => default.useAtDim(i).asInstanceOf[T]
            case default : GridPoints => default.useAtDim(i).asInstanceOf[T]
            case default : ErrorOrder => default.useAtDim(i).asInstanceOf[T]
            case _                    => throw new RuntimeException(s"Unexpected type $ct")
          }
        }
      }
    }
    return parameterValues.toList
  }

  override def toString = {
    "FddParameters(" + directions.mkString(", ") + ")" +
      ", " + errorOrders.mkString(", ") +
      ", " + gridPoints.mkString(", ") +
      ", " + stepSize.mkString(", ") + ")"
  }

  def validate() {
    val faulty = gridPoints.find(_.value < 3)
    if (faulty.isDefined)
      throw new RuntimeException(s"number of grid points less than 3 at dimension ${ faulty.get.value }")
  }
}

/** First stage of data-structure received from the main token parser (parsing.L1Parser.scala)
  * Includes the Domain, left and right hand side of the pde
  */
class FirstStageData(
    val domain : Domain,
    eq : Equation,
    val boundaries : List[Boundary],
    val funcs : Map[String, UserFunction],
    val options : FddParameters) {

  val lhs = MathTree.simplify(eq.left)
  val rhs = MathTree.simplify(eq.right)

  def dimCount = domain.dimCount

  override def toString : String = (
    "FirstStageData:"
      + s"\n\tDomain: $domain"
      + s"\n\tLhs: $lhs"
      + s"\n\tRhs: $rhs"
      + s"\n\tFunctions:" + "\n\t\t" + funcs.mkString("\n\t\t")
      + s"\n\tBoundaries:" + "\n\t\t" + boundaries.mkString("\n\t\t")
      + s"\n\tParameters: $options"
    )

}

object IntervalLike {
  def includes(int : IntervalLike, t : Double) : Boolean = int match {
    case Union(left, right)        => {
      IntervalLike.includes(left, t) || IntervalLike.includes(right, t)
    }
    case Intersection(left, right) => {
      IntervalLike.includes(left, t) && IntervalLike.includes(right, t)
    }
    case SetMinus(left, right)     => {
      IntervalLike.includes(left, t) && !IntervalLike.includes(right, t)
    }
    case x : Interval              => Interval.includes(x, t)
  }
}

abstract class IntervalLike

case class Union(
    left : IntervalLike,
    right : IntervalLike
) extends IntervalLike

case class Intersection(
    left : IntervalLike,
    right : IntervalLike
) extends IntervalLike

case class SetMinus(
    left : IntervalLike,
    right : IntervalLike
) extends IntervalLike

object Interval {
  // simple point initialization for intervals
  def apply(p : Double) = new Interval(p, true, p, true)

  def includes(int : Interval, t : Double) = {
    (((int.x0Included && int.x0 <= t) || (int.x0 < t))
      && ((int.x1Included && int.x1 >= t) || (int.x1 > t))
      )
  }
}

// one dimensional range
case class Interval(
    val x0 : Double,
    val x0Included : Boolean,
    val x1 : Double,
    val x1Included : Boolean
) extends IntervalLike {
  val isPoint = (x0 == x1)
  lazy val isOpen = !(x0Included || x1Included)
  lazy val isClosed = x0Included && x1Included

  require(x0 <= x1, "Left interval boundary cant be greater than right! " + this.toString)
  require(!(isPoint && isOpen), "A single point can not be open!")

  override def toString = (if (x0Included) "[" else "(") + s"$x0,$x1" +
    (if (x1Included) "]" else ")")
}

/** Abstract base-class for a binary tree structure of mathematical expressions,
  * where leaves are either actual values or derivations and inner nodes represent
  * the operations.
  * */
sealed abstract class MathTree extends ExprStencilParam {
  def +(expr : MathTree) = Add(this, expr)
  def -(expr : MathTree) = Sub(this, expr)
  def *(expr : MathTree) = Mul(this, expr)
  def /(expr : MathTree) = Div(this, expr)
}

/** This companion object assures the existence of the copy method
  * so that the copy function of every case class can direclty be called
  * from their base class NaryNode
  * */
object NaryNode {
  type Copyable = {
    def copy(children : Seq[MathTree]) : NaryNode
  }
  implicit def toCopyable(x : NaryNode) = x.asInstanceOf[NaryNode with Copyable]
  def unapply(x : NaryNode) = Some(x.children)
}

/** Abstract base-class for n-ary nodes */
sealed abstract class NaryNode extends MathTree {
  val children : Seq[MathTree]
}

/** This companion object assures the existence of the copy method
  * so that the copy function of every case class can direclty be called
  * from their base class BinaryNode
  * */
object BinaryNode {
  type Copyable = {
    def copy(left : MathTree, right : MathTree) : BinaryNode
  }
  implicit def toCopyable(x : BinaryNode) = x.asInstanceOf[BinaryNode with Copyable]
  def unapply(x : BinaryNode) = Some(x.left, x.right)
}

/** Abstract base-class for binary nodes */
sealed abstract class BinaryNode extends MathTree {
  val left : MathTree
  val right : MathTree
}

/** This companion object assures the existence of the copy method
  * so that the copy function of every case class can directly be called
  * from their base class UnaryNode
  * */
object UnaryNode {
  type Copyable = {
    def copy(leaf : MathTree) : UnaryNode
  }
  implicit def toCopyable(x : UnaryNode) = x.asInstanceOf[UnaryNode with Copyable]
  def unapply(x : UnaryNode) = Some(x.child)
}

/** Abstract base-class for unary nodes */
sealed abstract class UnaryNode extends MathTree {
  val child : MathTree
}

/** Abstract base-class for leafs */
sealed abstract class Leaf extends MathTree

/** trait for differentiable expressions */
sealed trait Differentiable extends MathTree

case class FunctionCall(name : String, children : Seq[MathTree]) extends NaryNode {
  def args = children
  def copy(children : Seq[MathTree]) : FunctionCall = FunctionCall(name, children)
}

case class Add(left : MathTree, right : MathTree) extends BinaryNode

case class Sub(left : MathTree, right : MathTree) extends BinaryNode

case class Mul(left : MathTree, right : MathTree) extends BinaryNode

case class Div(left : MathTree, right : MathTree) extends BinaryNode

case class Exp(left : MathTree, right : MathTree) extends BinaryNode {
  def base = left;
  def power = right
}

case class Value(t : Double) extends Leaf

case class Variable(name : String) extends Leaf

case class VecEntry(index : Int) extends Leaf

case class Vec() /*'Vector' is reserved in Scala*/ extends Leaf

case class Solution() extends Leaf with Differentiable // Represents the 0th derivative
case class Derivation(direction : Int, number : Int) extends Leaf with Differentiable // Single Derivation
case class NestedDerivation(devs : Seq[Derivation]) extends Leaf with Differentiable // Nested Derivations
case class Laplace() extends Leaf with Differentiable // Laplace operator
case class NormalDerivation() extends Leaf // no 'Differentiable' in terms of the stencil geneartion
// only used in Neumann boundaries

case class Equation(left : MathTree, right : MathTree) // Root for the PDE

object MathTree {

  /** simplifies an expression as far as possible
    *
    * @param  expr MathTree to be simplified
    * @throws  RuntimeException for unknown MathTree subclasses
    * @return The simplified MathTree
    */
  def simplify(
      expr : MathTree,
      variables : Map[String, Double] = Map(),
      xVector : Vector[Double] = Vector()) : MathTree = {
    def simplifyOnce(expr : MathTree) : MathTree = expr match {
      case x : NestedDerivation => simplifyNestedDerivative(x)
      case x : NaryNode         => x
      case x : BinaryNode       => x
      case x : UnaryNode        => x
      case x : Leaf             => x
    }

    def simplifyRec(expr : MathTree) : MathTree = expr match {
      case x @ NaryNode(children)      => simplifyOnce(x copy (children map simplifyRec))
      case x @ BinaryNode(left, right) => simplifyOnce(x copy(simplifyRec(left), simplifyRec(right)))
      case x @ UnaryNode(child)        => simplifyOnce(x copy (simplify(child)))
      case x : Leaf                    => simplifyOnce(x)
    }

    simplifyRec(expr)
  }

  /** Summarizes and orders nested Derivations with same direction
    *
    * @param  nDev NestedDerivation to be summarized
    * @return Either a 'NestedDerivation' or a single 'Derivation'
    */
  def simplifyNestedDerivative(nDev : NestedDerivation) : Differentiable = {
    assert(nDev.devs.size > 0)
    // group based on directions
    val directionMap = nDev.devs groupBy { _.direction }
    // aggregate number of derivatives in same direction
    val aggregatedMap = directionMap mapValues {
      devSet => devSet.foldLeft(0) { (number, dev) => number + dev.number }
    }
    // convert to Set of Derivations
    val devsUnsorted = aggregatedMap.toSeq.map { case (direction, number) => Derivation(direction, number) }
    // sorts based on direction
    val devs = devsUnsorted.sortWith(_.direction < _.direction)

    if (devs.length == 0)
      throw new Exception("Unforseen error occured")
    else if (devs.length == 1)
      devs.head
    else
      NestedDerivation(devs)
  }

}

sealed abstract class StencilParameter(val dim : Int, val len : Int) {
  def *(e : Entry) : StencilParameter
  def /(e : Entry) : StencilParameter
}

object StencilParameter {
  implicit def doubleToEntry(value : Double) : Entry = Entry(value)
  implicit def doubleSeqToEntrySeq(values : Double*) : Seq[Entry] = values map { x => Entry(x) }
}

case class Entry(val value : Double) extends StencilParameter(0, 1) {
  implicit def doubleToEntry(_value : Double) = Entry(_value)
  def *(stencil : Stencil) : Stencil = stencil * this
  def *(e : Entry) : Entry = value * e.value
  def /(e : Entry) : Entry = value / e.value
  def +(e : Entry) : Entry = value + e.value
  override def toString = value.toString
}

final class Stencil(val entries : Seq[StencilParameter]) extends StencilParameter(1 + entries(0).dim, entries.length) {
  assert(validate)

  def +(other : Stencil) : Stencil = {
    assert(dim == other.dim)
    if (this.len < other.len) {
      (this expandTo other.len) + other
    } else if (this.len > other.len) {
      this + (other expandTo this.len)
    } else {
      val newEntries = if (dim == 1) {
        for (i <- 0 until this.len) yield entries(i).asInstanceOf[Entry] + other.entries(i).asInstanceOf[Entry]
      } else {
        for (i <- 0 until this.len) yield entries(i).asInstanceOf[Stencil] + other.entries(i).asInstanceOf[Stencil]
      }
      new Stencil(newEntries)
    }
  }

  def -(other : Stencil) : Stencil = {
    this + (-1 * other)
  }

  def *(e : Entry) = new Stencil(for (entry <- entries) yield entry * e)

  def /(e : Entry) = new Stencil(for (entry <- entries) yield entry / e)

  def apply(i : Int) = {
    if (i >= len) {
      throw new IndexOutOfBoundsException()
    } else {
      entries(i)
    }
  }

  def buildEmptyEntry : StencilParameter = {
    if (this.dim == 1) {
      Entry(0)
    } else {
      val subEntry = entries(0).asInstanceOf[Stencil].buildEmptyEntry
      new Stencil(Seq.fill(entries(0).len)(subEntry))
    }
  }

  def expandTo(length : Int) : Stencil = {
    val lenDif = length - this.len
    assert(lenDif > 0)
    if (lenDif % 2 != 0) {
      throw new IllegalArgumentException("Cannot expand from odd to even (or even to odd) sized stencil")
    }

    def buildEmpty(dimensions : Int) : StencilParameter = {
      if (dimensions == 1) Entry(0) else new Stencil(Seq.fill(entries(0).len)(buildEmpty(dimensions - 1)))
    }

    val empty = buildEmpty(this.dim)
    val filler = Seq.fill(lenDif / 2)(empty)
    new Stencil(filler ++ this.entries ++ filler)
  }

  def tensorProduct(other : Stencil) : Stencil = {
    assert(other.entries.forall { x => x.isInstanceOf[Entry] })
    val newEntries = for (oEntry <- other.entries.asInstanceOf[Seq[Entry]]) yield {
      new Stencil(for (entry <- entries) yield entry * oEntry)
    }
    new Stencil(newEntries)
  }

  override def toString = {
    dim match {
      case 1 => "[" + entries.map { _.toString }.reduceLeft { (x, y) => x + ", " + y } + "]\n"
      case 2 => entries.foldLeft("") { (x, y) => x + y.toString }
      //case 3 => (entries map { _.toString }) mkString "\n"
      case _ => {
        val stringList = for (i <- 0 until len) yield "[" + (i) + "]:\n" + entries(i).toString
        stringList.mkString("\n")
      }
    }
  }

  lazy val validate : Boolean = {
    if (dim == 1)
      entries forall { x => x.isInstanceOf[Entry] }
    else {
      val dim = entries(0).dim
      val len = entries(0).len
      val bool1 = entries.forall { x => x.dim == dim && x.len == len && x.isInstanceOf[Stencil] }
      val bool2 = entries.forall { _.asInstanceOf[Stencil].validate }
      bool1 && bool2
    }
  }
}

class UserFunction(val name : String, val arguments : Seq[String], _expr : MathTree) {

  val expr = MathTree.simplify(_expr)
  val (valid, optError) = verifyFunction
  require(valid, optError.getOrElse("unknown requirement"))

  def verifyFunction() : (Boolean, Option[String]) = {
    def getUsedVars(
        _expr : MathTree,
        usedVars : Set[String] = Set(),
        usedVec : Boolean = false,
        usedSol : Boolean = false
    ) : (Set[String], Boolean, Boolean) = _expr match {
      case x : NaryNode       => {
        x.children.map {
          y => getUsedVars(y, usedVars, usedVec, usedSol)
        }.reduce {
          (used1, used2) => ((used1._1 ++ used2._1), (used1._2 || used2._2), (used1._3 || used2._3))
        }
      }
      case x : BinaryNode     => {
        val used = getUsedVars(x.right, usedVars, usedVec, usedSol)
        getUsedVars(x.left, used._1, used._2, used._3)
      }
      case x : UnaryNode      => getUsedVars(x.child, usedVars, usedVec, usedSol)
      case Variable(name)     => (usedVars + name, usedVec, usedSol)
      case _ : Vec            => (usedVars, true, usedSol)
      case _ : VecEntry       => (usedVars, true, usedSol)
      case _ : Differentiable => (usedVars, usedVec, true)
      case _ : Leaf           => (usedVars, usedVec, usedSol)
    }

    val (neededVars, neededVec, neededSol) = getUsedVars(expr)
    val reserved = neededVars & ReservedNames.fieldsToStringSet
    val missing = neededVars -- arguments.toSet
    if (!reserved.isEmpty)
      return (false, Some(s"Function '$name': following used variables are reserved: " + reserved.mkString(",")))
    else if (!missing.isEmpty)
      return (false, Some(s"Function '$name': following arguments are used but not given: " + missing.mkString(",")))
    else if (neededVec && !(arguments contains ReservedNames.vector))
      return (false, Some(s"Function '$name': ${ ReservedNames.vector } is used but not specified as function argument"))
    else if (neededSol && !(arguments contains ReservedNames.solution))
      return (false, Some(s"Function '$name': ${ ReservedNames.solution } is used but not specified as function argument"))
    else
      return (true, None)
  }

  def verifyInput(values : Seq[MathTree]) : (Boolean, Option[String]) = {
    if (arguments.length != values.length) {
      val message = s"Function '$name' takes ${ arguments.length } arguments, but ${ values.length } were given"
      return (false, Some(message))
    }

    def verifySingle(pair : (String, MathTree)) : (Boolean, Option[String]) = {
      if (pair._1 == ReservedNames.solution) pair._2 match {
        case Solution()     => (true, None)
        case Variable(name) => (false, Some(s"Function '$name': cannot replace solution name '${ ReservedNames.solution }' with '$name'"))
        case _              => (false, Some(s"Function '$name': cannot insert expression or value for solution argument '${ ReservedNames.solution }'"))
      } else if (pair._1 == ReservedNames.vector) pair._2 match {
        case Vec()          => (true, None)
        case Variable(name) => (false, Some(s"Function '$name': cannot replace vector name '${ ReservedNames.vector }' with '$name'"))
        case _              => (false, Some(s"Function '$name': cannot insert expression or value for solution argument '${ ReservedNames.vector }'"))
      } else {
        (true, None)
      }
    }

    val pairs = arguments zip values
    pairs.map { verifySingle _ }.find { _._1 == false } match {
      case Some(error) => error
      case None        => (true, None)
    }
  }

  override def toString = "(" + arguments.mkString(",") + ") => " + expr
}

/** Reserved variable names */
final object ReservedNames {

  val boundaryKey = "for"

  /**
    * Name of the domain in (unicode, latex)
    */
  val domain = ("\u03a9", "\\Omega")

  /**
    * Variable name for the error order
    */
  val errorOrder = "e"

  /**
    * Gridpoints
    */
  val gridPoints = "N"

  /**
    * Normal vector of the domain boundary
    */
  val normal = "n"

  /**
    * Name of the solution of the pde
    */
  val solution = "U"

  /**
    * Variable name for the stepsize
    */
  val stepSize = "h"

  /**
    * Name of the vector used as argument for the solution
    */
  val vector = "x"

  def fieldsToStringSet() = (this.getClass.getDeclaredFields :\ Set[String]()) {
    (field, set) =>
      field.setAccessible(true);
      field.get(this) match {
        case x : (Any, Any) => (set
          + x._1.asInstanceOf[String]
          + x._2.asInstanceOf[String])
        case x : String     => set + x
        case _              => set
      }
  }

  def _fieldStringsToMap = (this.getClass.getDeclaredFields :\ Map[String, String]()) {
    (field, map) =>
      field.setAccessible(true);
      field.get(this) match {
        //case x: (Any,Any) => map +
        case x : String => map + (field.getName -> x)
        case _          => map
      }
  }
}

/** Latex and unicode signs */
final object ReservedSigns {
  val capitalDelta = ("\u2206", "\\Delta")

  val lessOrEqual = ("\u2264", "\\leq")
  val greaterOrEqual = ("\u2265", "\\geq")

  val elemOf = ("\u2208", "\\in")
  val notElemOf = ("\u2209", "\\notin")

  val logicalAnd = ("\u2227", "\\land")
  val logicalOr = ("\u2228", "\\lor")

  val intersection = ("\u2229", "\\cap")
  val partial = ("\u2202", "\\partial")
  val setMinus = ("\u2216", "\\setminus")
  val times = ("\u00D7", "\\times") // cartesian product or cross product
  val dotOperator = ("\u22C5", "\\cdot")
  val union = ("\u222A", "\\cup")

  def fieldsToStringSet() = (this.getClass.getDeclaredFields :\ Set[String]()) {
    (field, set) =>
      field.setAccessible(true);
      field.get(this) match {
        case x : (Any, Any) => (set
          + x._1.asInstanceOf[String]
          + x._2.asInstanceOf[String])
        case _              => set
      }
  }
}

