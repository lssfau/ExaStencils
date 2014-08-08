import scala.collection.mutable.ListBuffer

import exastencils.datastructures._
import exastencils.datastructures.ir._

object Bug {
  private final object Add2 extends DefaultStrategy("replaces x -> x+2") {

    private final val SKIP_ANNOT = "skip"

    this += new Transformation("apply", {
      case vAcc @ VariableAccess(v, Some(IntegerDatatype())) =>
        if (!vAcc.removeAnnotation(SKIP_ANNOT).isDefined) {
          vAcc.annotate(SKIP_ANNOT) // already done
          AdditionExpression(vAcc, IntegerConstant(2))
        } else
          vAcc
    })
  }

  def main(args : Array[String]) : Unit = {
    println("fail")
    val failInput = ArrayAccess(
      VariableAccess("A", Some(PointerDatatype(RealDatatype()))),
      AdditionExpression(VariableAccess("x", Some(IntegerDatatype())), IntegerConstant(2)))
    println(failInput.cpp)
    println()
    Add2.applyStandalone(failInput) // it matches, but it did not replace anything
    println(failInput.cpp)

    println()
    println()
    println()

    println("ok")
    val goodInput = ArrayAccess(
      VariableAccess("A", Some(PointerDatatype(RealDatatype()))),
      AdditionExpression(VariableAccess("x", Some(IntegerDatatype())), IntegerConstant(1)))
    println(goodInput.cpp)
    println()
    Add2.applyStandalone(goodInput)
    println(goodInput.cpp)
  }
}
