package exastencils.baseExt.l4

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_ColorLoops

case class L4_ColorLoops(var colorExps : ListBuffer[L4_Modulo], var stmts : ListBuffer[L4_Statement]) extends L4_Statement {
  override def prettyprint(out : PpStream) = out << "color with {\n" <<< (colorExps, ",\n") << ",\n" <<< (stmts, "\n") << "\n}"
  override def progress = Logger.error("Trying to progress " + this.getClass.getName + " which is unsupported")

  def toRepeatLoops() : L4_RepeatLoops = {
    val conjNF = ArrayBuffer[ListBuffer[L4_Expression]]()
    for (colorExp @ L4_Modulo(_, L4_IntegerConstant(nrCols)) <- colorExps) {
      val disjTerm = ListBuffer[L4_Expression]()
      for (c <- 0L until nrCols)
        disjTerm += L4_EqEq(colorExp, L4_IntegerConstant(c))
      conjNF += disjTerm
    }

    // can be seen as a transformation from conjunctive normal form to disjunctive normal form:
    //   perform cross product from all intermediate results with next list of possibilities
    var disjNF = ListBuffer[L4_Expression]() // only boolean expressions
    val disjTerms = conjNF.iterator
    disjNF = disjTerms.next()
    while (disjTerms.hasNext) {
      val disjTerm = disjTerms.next()
      disjNF = for (left <- disjNF; right <- disjTerm) yield L4_AndAnd(left, right) // cross product using L4_AndAnd as combinator
    }

    L4_RepeatLoops(disjNF, stmts)
  }
}

/// L4_ResolveColorLoops

object L4_ResolveColorLoops extends DefaultStrategy("Resolve color with loops") {
  this += new Transformation("Resolve", {
    case cl : L4_ColorLoops => cl.toRepeatLoops()
  })
}
