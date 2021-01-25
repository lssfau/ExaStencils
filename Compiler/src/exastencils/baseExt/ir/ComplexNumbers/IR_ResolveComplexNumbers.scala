package exastencils.baseExt.ir.ComplexNumbers

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_ComplexDatatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_Function
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_UserFunctions
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.logger.Logger
import exastencils.util.ir.IR_UtilFunctions

object IR_ResolveComplexNumbers extends DefaultStrategy("Resolve operations with complex expressions") {

  def isComplexExpr(expr : IR_Expression) : Boolean = expr.isInstanceOf[IR_ComplexExpression]
  def isComplexAcc(va : IR_Expression) : Boolean = va.datatype.isInstanceOf[IR_ComplexDatatype]
  def isComplexNumber(expr : IR_Expression) : Boolean = isComplexAcc(expr) || isComplexExpr(expr)
  def getAccName(expr : IR_Expression) : String = expr match {
    case va : IR_VariableAccess => va.name
    case _                      => Logger.error("unexpected argument")
  }

  // label
  val potentialComplexNumberOps = "potentialComplexNumberOps"

  // map strings to corresponding function node
  val callMap = mutable.HashMap[String, ListBuffer[IR_Expression] => IR_Expression](
    ("Im", IR_ComplexNumberAccessImag.apply),
    ("Re", IR_ComplexNumberAccessReal.apply),
    ("polar", IR_ComplexNumberPolar.apply),
    ("equal", IR_ComplexNumberEqual.apply),
    ("notEqual", IR_ComplexNumberNotEqual.apply)
  )



  // mark calls potentially related to complex numbers but not immediately distinguishable
  // => look at surrounding node (assignment, declaration, function call)
  this += new Transformation("mark", {
    case fcall : IR_FunctionCall if (fcall.name == "polar") =>
      fcall.annotate(potentialComplexNumberOps)
      fcall
    // others ...
  })

  this += new Transformation("check for types", {
    // check marked nodes
    case fcall : IR_FunctionCall if (fcall.arguments.exists(arg => arg.hasAnnotation(potentialComplexNumberOps))) =>
      val arg = fcall.arguments.find(arg => arg.hasAnnotation(potentialComplexNumberOps)).get
      val argidx : Int = fcall.arguments.indexOf(arg)

      // chained by complex function
      if(callMap.contains(fcall.name)) {
        fcall.arguments(argidx) = callMap(arg.asInstanceOf[IR_FunctionCall].name)(arg.asInstanceOf[IR_FunctionCall].arguments)
        fcall
      } else {
        // search function collections to determine argument type
        val surroundingFunction : IR_Function =
          if (IR_UserFunctions.get.functions.exists(func => func.name == fcall.name)) {
            IR_UserFunctions.get.functions.find(func => func.name == fcall.name).get.asInstanceOf[IR_Function]
          } else if (IR_UtilFunctions.get.functions.exists(func => func.name == fcall.name)) {
            IR_UtilFunctions.get.functions.find(func => func.name == fcall.name).get.asInstanceOf[IR_Function]
          }
          else Logger.error("surrounding function not found")

        // is the marked node an argument of type complex
        if (surroundingFunction.parameters(argidx).datatype.isInstanceOf[IR_ComplexDatatype]) {
          // resolve to complex number node
          fcall.arguments(argidx) = callMap(arg.asInstanceOf[IR_FunctionCall].name)(arg.asInstanceOf[IR_FunctionCall].arguments)
          fcall
        } else {
          // leave alone
          arg.removeAnnotation(potentialComplexNumberOps)
          fcall
        }
      }

    case assign @ IR_Assignment(_, src, _) if (src.hasAnnotation(potentialComplexNumberOps))                                    =>
      // is the marked node a assignment for a complex variable
      if (assign.dest.datatype.isInstanceOf[IR_ComplexDatatype]) {
        callMap(src.asInstanceOf[IR_FunctionCall].name)(src.asInstanceOf[IR_FunctionCall].arguments)
      } else {
        src.removeAnnotation(potentialComplexNumberOps)
        assign
      }
    case decl @ IR_VariableDeclaration(dt, _, init, _) if (init.isDefined && init.get.hasAnnotation(potentialComplexNumberOps)) =>
      // initialization of a complex variable declaration
      if (dt.isInstanceOf[IR_ComplexDatatype]) {
        callMap(init.get.asInstanceOf[IR_FunctionCall].name)(init.get.asInstanceOf[IR_FunctionCall].arguments)
      } else {
        init.get.removeAnnotation(potentialComplexNumberOps)
        decl
      }
  })
 this += new Transformation("resolve directly distinguishables", {
    // immediately as complex number function identifiable from arguments
    case fcall : IR_FunctionCall if (callMap.contains(fcall.name) && fcall.arguments.exists(arg => arg.datatype.isInstanceOf[IR_ComplexDatatype])) =>
      callMap(fcall.name)(fcall.arguments)
  })

}
