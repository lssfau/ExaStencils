package exastencils.matlang

import exastencils.datastructures._
import exastencils.core._

/** Expressions are entities that can be reduced to a value. */
class Expr(val etype : Type) extends Node {
  
}

/** Expression that yields a matrix */
class FunctionCall(
		val returnType : Type,
		val fun : Function,
		val args : List[Expr]) 
    extends Expr(returnType)
{

}

class Function(val name : String)
{
}

abstract sealed class Type;
case class MatrixT() extends Type;
case class FieldT() extends Type;

