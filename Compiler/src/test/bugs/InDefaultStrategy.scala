package test.bugs

import scala.collection.mutable.ListBuffer

import exastencils.datastructures.ir.CommentStatement
import exastencils.datastructures.ir.Statement
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation

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
