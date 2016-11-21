package test.bugs

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Comment
import exastencils.datastructures._

object InDefaultStrategy {
  def main(args : Array[String]) : Unit = {

    val input = new ListBuffer[IR_Comment]()
    input += IR_Comment("change me")

    val newComment = "neu"
    val removeComments = new DefaultStrategy("change comments")
    removeComments += new Transformation("now!", {
      case c : IR_Comment => IR_Comment(newComment)
    })
    removeComments.applyStandalone(input)

    println(input.head)
    assert(input.head.comment == newComment)
  }
}
