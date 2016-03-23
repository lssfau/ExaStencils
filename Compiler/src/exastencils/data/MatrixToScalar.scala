package exastencils.data

import exastencils.core._
import exastencils.logger._;
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.Transformation._

object MatrixToScalar extends DefaultStrategy("Setting up fragment") {
  val componentAnnotation = "ASSIGNMENT_COMPONENT"

  this += new Transformation("Apply transpose for inline expressions", {
    case x : FunctionCallExpression if (x.name == "transpose") => {
      x.arguments(0) match {
        case x : VectorExpression =>
          x.rowVector = !x.rowVector; x
        case x : MatrixExpression =>
          x.expressions.transpose; x
      }
      x
    }
  })

  def getExpressionLength(node : Node) : Array[Int] = {
    node match {
      case x : FieldAccessLike               => getExpressionLength(x.fieldSelection.field.fieldLayout.datatype)
      case VectorDatatype(_, size, true)     => Array(1, size)
      case VectorDatatype(_, size, false)    => Array(size, 1)
      case MatrixDatatype(_, m, n)           => Array(m, n)
      case x @ VectorExpression(_, _, true)  => Array(1, x.length)
      case x @ VectorExpression(_, _, false) => Array(1, x.length)
      case x : MatrixExpression              => Array(x.rows, x.columns)
      case _                                 => Logger.warn(s"""getExpressionLength for '$node'"""); Array(0, 0)
    }
  }

  this += new Transformation("Duplicate and annotate assignments", {
    case x : AssignmentStatement => {
      var length = getExpressionLength(x.dest)
      if (length(0) * length(1) > 1) {
        var assignments = Duplicate(x, length(0) * length(1))
        var crow = 0
        var ccol = 0
        assignments.foreach(a => {
          a.annotateRecursively(componentAnnotation, new ConstIndex(ccol, crow))
          ccol += (ccol + 1) % length(0)
          crow += 1
        })
        assignments
      } else {
        x
      }
    }
  })

  this += new Transformation("Resolve multiplications", {
    //    case MultiplicationExpression() =>
    case x => x
  })

  this += new Transformation("Resolve scaling", {
    //    case MultiplicationExpression(left

    case x => x
  })

  this += new Transformation("Resolve matrix vector multiplications", { // transforms matrix*matrix into matrix
    case x => x
  }) // macht aus Matrix*Vektor einen Vektor mit einzelnen Multiplikationen
  this += new Transformation("Resolve matrix matrix multiplications", { case x => x }) // macht aus Matrix*Matrix eine Matrix mit einzelnen Multiplikationen
  this += new Transformation("Resolve additions and subtractions", { case x => x }) // macht aus Matrix+-Matrix bzw Vektor+-Vektor eine Matrix/Vektor mit einzelnen Operationen

  this += new Transformation("Set component indices", {
    case x : DirectFieldAccess if (x.hasAnnotation(componentAnnotation)) => x.componentIndex = x.getAnnotation(componentAnnotation).get.asInstanceOf[ConstIndex]; x
  })

  //  def getExpressionLength(node : Node) : (Int, Int) = {
  //    var length = (1, 1)
  //    var matrices = StateManager.findAll[MatrixExpression](node)
  //    for (matrix <- matrices) {
  //      length = math.max(length, matrix.rows * matrix.columns)
  //    }
  //
  //    if (length <= 1) {
  //      var vectors = StateManager.findAll[VectorExpression](node)
  //      for (vector <- vectors) {
  //        length = math.max(length, vector.length)
  //      }
  //    }
  //
  //    return length
  //  }
  //
  //  this += new Transformation("Split assignments", {
  //    case x : AssignmentStatement => {
  //      var dest = x.dest
  //      var src = x.src
  //      var dLength = getExpressionLength(dest)
  //      var sLength = getExpressionLength(src)
  //      if (dLength != sLength || (dLength > 1 && sLength != 1)) { // Entweder gleiche Dimensionalität (Skalar=Skalar bzw HigherDim=HigherDim) oder (HigherDim = Skalar) 
  //        Logger.warn("Dimensionality mismatch in expression " + x.prettyprint())
  //      }
  //      var length = math.max(dLength, sLength)
  //      if (length == 1) {
  //        x
  //      } else {
  //        var assignments = Duplicate(x, length)
  //        var component = 0
  //        assignments.foreach(a => {
  //          a.annotate("ASSIGNMENT_COMPONENT", component)
  //          component += 1
  //        })
  //        assignments
  //      }
  //    }
  //  })
}
