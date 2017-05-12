package exastencils.deprecated.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.datastructures._

/// IR_UnifyInnerTypes

object IR_UnifyInnerTypes extends DefaultStrategy("Unify inner types of (constant) vectors and matrices") {
  var vectors = ListBuffer[IR_VectorExpression]()
  var matrices = ListBuffer[IR_MatrixExpression]()

  override def apply(applyAtNode : Option[Node]) = {
    this.execute(new Transformation("Find vectors and matrices", {
      case x : IR_VectorExpression =>
        vectors.+=(x); x
      case x : IR_MatrixExpression => matrices.+=(x); x
    }))

    vectors.foreach(vector => {
      if (vector.isConstant) {
        val reals = vector.expressions.count(_.isInstanceOf[IR_RealConstant])
        val ints = vector.expressions.count(_.isInstanceOf[IR_IntegerConstant])
        if (ints > 0 && reals > 0) {
          vector.expressions = vector.expressions.map(e => if (e.isInstanceOf[IR_RealConstant]) e; else IR_RealConstant(e.asInstanceOf[IR_IntegerConstant].v))
        }
      }
    })

    matrices.foreach(matrix => {
      if (matrix.isConstant) {
        val reals = matrix.expressions.count(_.isInstanceOf[IR_RealConstant])
        val ints = matrix.expressions.count(_.isInstanceOf[IR_IntegerConstant])
        if (ints > 0 && reals > 0) {
          for(i <- 0 until matrix.expressions.length) {
            matrix.expressions(i) match {
              case c : IR_IntegerConstant => matrix.expressions(i) = IR_RealConstant(c.v)
              case _ =>
            }
          }
        }
      }
    })
  }
}
