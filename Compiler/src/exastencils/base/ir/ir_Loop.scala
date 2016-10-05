package exastencils.base.ir

import scala.collection.mutable.ListBuffer

import exastencils.prettyprinting.PpStream

/// IR_ForLoop

object IR_ForLoop {
  def apply(begin : IR_Statement, end : IR_Expression, inc : IR_Statement, reduction : IR_Reduction, body : IR_Statement*) =
    new IR_ForLoop(begin, end, inc, body.to[ListBuffer], Option(reduction))
  def apply(begin : IR_Statement, end : IR_Expression, inc : IR_Statement, body : IR_Statement*) =
    new IR_ForLoop(begin, end, inc, body.to[ListBuffer])

}

case class IR_ForLoop(
    var begin : IR_Statement,
    var end : IR_Expression,
    var inc : IR_Statement,
    var body : ListBuffer[IR_Statement],
    var reduction : Option[IR_Reduction] = None) extends IR_Statement {
  // TODO: extract reduction, eg IR_ReductionLoop(IR_ForLoop)

  def maxIterationCount() = {
    if (hasAnnotation("numLoopIterations"))
      getAnnotation("numLoopIterations").get.asInstanceOf[Int]
    else
      0 // TODO: warning?
  }

  override def prettyprint(out : PpStream) : Unit = {
    // BEGIN AMAZING HACK as workaround for IBM XL compiler
    var realEnd = end.prettyprint(out.env)
    if (realEnd.length > 2 && realEnd(0) == '(')
      realEnd = realEnd.substring(1, realEnd.length - 1)
    var realInc = inc.prettyprint(out.env)
    if (realInc.length > 2 && realInc(0) == '(')
      realInc = realInc.substring(1, realInc.length - 1)
    out << "for (" << begin << ' ' << realEnd << "; " << realInc
    // END HACK
    //out << "for (" << begin << ' ' << end << "; " << inc
    val last = out.last()
    if (last == ';' || last == ')') // ')' in case of upper hack removed the ';' instead of the closing bracket
      out.removeLast()
    out << ") {\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }
}

/// IR_WhileLoop

object IR_WhileLoop {
  def apply(comparison : IR_Expression, body : IR_Statement*) = new IR_WhileLoop(comparison, body.to[ListBuffer])
}

case class IR_WhileLoop(var comparison : IR_Expression, var body : ListBuffer[IR_Statement]) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "while (" << comparison << ") {\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }
}

/// IR_BreakStatement

case class IR_Break() extends IR_Statement {
  override def prettyprint(out : PpStream) = out << "break;"
}
