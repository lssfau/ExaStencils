package exastencils.matlang

import exastencils.datastructures._
import exastencils.core._

class Assignment(val dest : Variable, val rhs : Expr) extends Node {
  
}

/** Expressions are entities that can be reduced to a value. */
class Expr(val etype : Type) extends Node {
  
}

class NilExpr(etype : Type) extends Expr(etype)

/** Expression that yields a matrix */
case class FunctionCall(
		val fun : Function,
		val args : List[Expr]) 
    extends Expr(fun.returnType)
{

}

class Function(val returnType : Type, val name : String)
{
}

class Variable(etype : Type) extends Expr(etype) {
  
}

abstract sealed class Type;
case class MatrixT() extends Type;
case class FieldT() extends Type;

