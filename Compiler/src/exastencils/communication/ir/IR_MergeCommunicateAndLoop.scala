package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._

/// IR_MergeCommunicateAndLoop

object IR_MergeCommunicateAndLoop extends DefaultStrategy("Merge communicate statements with loop nodes") {
  def processFctBody(body : ListBuffer[IR_Statement]) : ListBuffer[IR_Statement] = {
    if (body.length < 2) return body

    var newBody = ListBuffer[IR_Statement]()

    for (i <- 1 until body.length) { // check for pre communications steps
      (body(i - 1), body(i)) match {
        case (cs : IR_Communicate, loop : IR_LoopOverPoints) if cs.field.field.level == loop.field.level => // skip intergrid ops for now
          loop.preComms += cs // already merged: newBody += cs
        case (first, second)                                                                             => newBody += first
      }
    }
    newBody += body.last

    if (newBody.length == body.length) { // nothing changed -> look for post communications steps
      newBody.clear
      for (i <- body.length - 1 until 0 by -1) {
        (body(i - 1), body(i)) match {
          case (loop : IR_LoopOverPoints, cs : IR_Communicate) if cs.field.field.level == loop.field.level => // skip intergrid ops for now
            loop.postComms += cs // already merged: newBody += cs
          case (first, second)                                                                             => newBody.prepend(second)
        }
      }
      newBody.prepend(body.head)
    }

    if (newBody.length != body.length)
      processFctBody(newBody) // sth changed -> apply recursively to support multiple communicate statements
    else
      newBody // nothing changed -> work is done
  }

  this += new Transformation("Resolving", {
    case fct : IR_Function =>
      fct.body = processFctBody(fct.body)
      fct
  })
}
