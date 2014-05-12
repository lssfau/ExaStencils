package harald_dep.Parser

import scala.util.parsing.combinator._

class ExaParser extends JavaTokenParsers // StandardTokenParsers
{

  // set members of given object obj via reflection
  def set[T](obj : AnyRef, ident : String, value : T) = {
    if (!value.equals(None))
      println("setting " + ident + " to " + value)
    else
      println("variable " + ident)
    val field = obj.getClass.getDeclaredField(ident)
    val accessible = field.isAccessible
    field.setAccessible(true)

    if (field.get(obj).getClass.equals(None.getClass())) {
      // Field is Option[T]
      obj.getClass.getMethods.find(_.getName == ident + "_$eq").get.invoke(obj, Option[T](value))
    } else {
      // field is pojo - set directly
      obj.getClass.getMethods.find(_.getName == ident + "_$eq").get.invoke(obj, value.asInstanceOf[Object])
    }

    field.setAccessible(accessible)
  }

  def newline : Parser[Any] = "\n" | "\r\n"

  // common part for expressions 
  def expr : Parser[Expr] =
    (term ~ "+" ~ term) ^^ { case lhs ~ op ~ rhs => BinaryOp("+", lhs, rhs) } |
      (term ~ "-" ~ term) ^^ { case lhs ~ op ~ rhs => BinaryOp("-", lhs, rhs) } |
      term

  def term : Parser[Expr] =
    (factor ~ "*" ~ factor) ^^ { case lhs ~ op ~ rhs => BinaryOp("*", lhs, rhs) } |
      (factor ~ "/" ~ factor) ^^ { case lhs ~ op ~ rhs => BinaryOp("/", lhs, rhs) } |
      factor

  def factor : Parser[Expr] =
    "(" ~> expr <~ ")" |
      floatingPointNumber ^^ { x => NumberDouble(x.toFloat) } |
      wholeNumber ^^ { x => Number(x.toInt) }

  // case classes to hold expressions
  //private[ExaParser] 
  abstract class Expr
  private case class Variable(name : String) extends Expr
  private case class Number(value : Int) extends Expr
  private case class NumberDouble(value : Double) extends Expr
  private case class UnaryOp(operator : String, arg : Expr) extends Expr
  private case class BinaryOp(operator : String, left : Expr, right : Expr) extends Expr

  def simplify(e : Expr) : Expr = {
    val simpSubs = e match {
      case BinaryOp(op, left, right) => BinaryOp(op, simplify(left), simplify(right))
      case UnaryOp(op, operand)      => UnaryOp(op, simplify(operand))
      case _                         => e
    }

    // simplify expressions by pattern matching
    def simplifyTop(x : Expr) = x match {
      case UnaryOp("-", UnaryOp("-", x))     => x
      case UnaryOp("+", x)                   => x
      case BinaryOp("*", x, Number(1))       => x
      case BinaryOp("*", Number(1), x)       => x
      case BinaryOp("*", x, Number(0))       => Number(0)
      case BinaryOp("*", Number(0), x)       => Number(0)
      case BinaryOp("/", x, Number(1))       => x
      case BinaryOp("/", x1, x2) if x1 == x2 => Number(1)
      case BinaryOp("+", x, Number(0))       => x
      case BinaryOp("+", Number(0), x)       => x
      case e                                 => e
    }
    simplifyTop(simpSubs)
  }

  // evaluate expressions
  def evaluate(e : Expr) : Int =
    {
      simplify(e) match {
        case Number(x)             => x
        case NumberDouble(x)       => x.toInt
        case UnaryOp("-", x)       => -(evaluate(x))
        case BinaryOp("+", x1, x2) => (evaluate(x1) + evaluate(x2))
        case BinaryOp("-", x1, x2) => (evaluate(x1) - evaluate(x2))
        case BinaryOp("*", x1, x2) => (evaluate(x1) * evaluate(x2))
        case BinaryOp("/", x1, x2) => (evaluate(x1) / evaluate(x2))
      }
    }

}
