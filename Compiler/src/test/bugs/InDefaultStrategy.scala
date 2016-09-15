package test.bugs

import scala.collection.mutable.ListBuffer

import exastencils.datastructures._
import exastencils.datastructures.ir.CommentStatement

object InDefaultStrategy {
  def main(args : Array[String]) : Unit = {

    val input = new ListBuffer[CommentStatement]()
    input += new CommentStatement("change me")

    val newComment = "neu"
    val removeComments = new DefaultStrategy("change comments")
    removeComments += new Transformation("now!", {
      case c : CommentStatement => CommentStatement(newComment)
    })
    removeComments.applyStandalone(input)

    println(input.head)
    assert(input.head.comment == newComment)
  }
}
