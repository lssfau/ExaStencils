package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.core.StateManager
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_ResultingDatatype

/// IR_HackMatComponentAccess

// FIXME: update with actual accessors
case class IR_HackMatComponentAccess(var mat: IR_VariableAccess, var i: IR_Expression, var j: IR_Expression) extends IR_Expression {
  override def datatype = mat.datatype
  override def prettyprint(out: PpStream): Unit = out << mat << "(" << i << ", " << j << ")"
}

/// IR_MatrixExpression
object IR_MatrixExpression {
  //def apply(innerDatatype : Option[IR_Datatype], rows : Integer, columns : Integer) : IR_MatrixExpression = new IR_MatrixExpression(innerDatatype, rows, columns)
  def apply(innerDatatype : IR_Datatype, rows : Integer, columns : Integer) : IR_MatrixExpression = new IR_MatrixExpression(Some(innerDatatype), rows, columns)
  def apply(innerDatatype: Option[IR_Datatype], rows: Integer, columns: Integer, expressions: Array[IR_Expression]): IR_MatrixExpression = {
    val tmp = new IR_MatrixExpression(innerDatatype, rows, columns)
    tmp.expressions = expressions
    tmp
  }
  def apply(innerDatatype: Option[IR_Datatype], expressions: ListBuffer[ListBuffer[IR_Expression]]): IR_MatrixExpression = {
    val rows = expressions.size
    val columns = expressions(0).size
    val tmp = new IR_MatrixExpression(innerDatatype, rows, columns)
    for (row <- 0 until rows) {
      for (col <- 0 until columns) {
        tmp.set(row, col, expressions(row)(col))
      }
    }
    tmp
  }

//  implicit def +(a : IR_MatrixExpression, b : IR_MatrixExpression) = {
//    val ret = new IR_MatrixExpression(Some(IR_ResultingDatatype(a.innerDatatype.get, b.innerDatatype.get)), a.rows, a.columns)
//    var xx = a.expressions.zip(b.expressions).map(x => x._1 + x._2).asInstanceOf[Array[IR_Expression]]
//    ret.expressions = xx
//    ret
//  }
}

// FIXME: to be replaced/ updated
case class IR_MatrixExpression(var innerDatatype: Option[IR_Datatype], var rows: Integer, var columns: Integer) extends IR_Expression {
  var expressions: Array[IR_Expression] = Array.ofDim[IR_Expression](rows * columns)

  override def datatype = {
    if (innerDatatype.isEmpty) {
      var ret = expressions(0).datatype
      expressions.foreach(s => ret = IR_ResultingDatatype(ret, s.datatype))
      innerDatatype = Some(ret)
    }
    IR_MatrixDatatype(innerDatatype.getOrElse(IR_RealDatatype), this.rows, this.columns)
  }

  def prettyprintInner(out: PpStream): Unit = {
    out << (if (Platform.targetCompiler == "GCC") "std::move((" else "((")
    innerDatatype.getOrElse(IR_RealDatatype).prettyprint(out)
    out << "[]){" << (expressions.map(_.prettyprint).mkString(",")) << "})"
  }
  override def prettyprint(out: PpStream): Unit = {
    val prec = if (Knowledge.useDblPrecision) "double" else "float"

    out << "Matrix<" << (if (isInteger) "int" else prec) << ", " << rows << ", " << columns << "> ("
    prettyprintInner(out)
    out << ")"
  }

  def isConstant = expressions.forall(e => e.isInstanceOf[IR_Number])
  def isInteger = expressions.forall(e => e.isInstanceOf[IR_IntegerConstant])
  def isReal = expressions.forall(e => e.isInstanceOf[IR_RealConstant])
  def get(row: Integer, column: Integer) = expressions(row * columns + column)
  def set(row: Integer, column: Integer, exp: IR_Expression) = expressions(row * columns + column) = exp
  override def toString: String = {"IR_MatrixExpression(" + innerDatatype + ", " + rows + ", " + columns + "); Items: " + expressions.mkString(", ")}
}

case object IR_ResolveMatrices extends DefaultStrategy("Resolve matrices into scalars") {
  val annotationFctCallCounter = "IR_ResolveMatrices.fctCallCounter"
  var fctCallCounter = 0 // temporary variable used to replace function calls in expressions

  this += new Transformation("declarations", {
    case decl @ IR_VariableDeclaration(matrix : IR_MatrixDatatype, _, Some(exp : IR_Expression)) => {
      var newDecls = ListBuffer[IR_Statement]()
      // split declaration and definition so each part can be handled by subsequent transformations
      newDecls += IR_VariableDeclaration(matrix, decl.name, None)
      newDecls += IR_Assignment(IR_VariableAccess(decl), exp)
      newDecls
    }
      // FIXME valueDeclarations
  })

  this += new Transformation("modify function calls 1/2", { // prepare function calls returning IR_MatrixDatatype to replaced by separate accesses
    case stmt @ IR_Assignment(_, src, _) => {
      val calls = StateManager.findAll[IR_FunctionCall](src).filter(_.datatype.isInstanceOf[IR_MatrixDatatype])
      var newStmts = ListBuffer[IR_Statement]()
      calls.foreach(c => {
        newStmts += IR_VariableDeclaration(c.function.datatype, "_fct" + fctCallCounter + "_" + c.function.name, Some(Duplicate(c)))
        c.annotate(annotationFctCallCounter, fctCallCounter)
      })
      newStmts += stmt
      newStmts
    }
    case stmt @ IR_VariableDeclaration(_, _, Some(src)) => {
      val calls = StateManager.findAll[IR_FunctionCall](src).filter(_.datatype.isInstanceOf[IR_MatrixDatatype])
      var newStmts = ListBuffer[IR_Statement]()
      calls.foreach(c => {
        newStmts += IR_VariableDeclaration(c.function.datatype, "_fct" + fctCallCounter + "_" + c.function.name, Some(Duplicate(c)))
        c.annotate(annotationFctCallCounter, fctCallCounter)
      })
      newStmts += stmt
      newStmts
    }
  })

  this += new Transformation("modify function calls 2/2", {
    case call : IR_FunctionCall if(call.hasAnnotation(annotationFctCallCounter)) => {
      IR_VariableAccess("_fct" + fctCallCounter + "_" + call.function.name, call.function.datatype.asInstanceOf[IR_MatrixDatatype].datatype)
    }
  })


  this += new Transformation("return types", {
    case func : IR_Function if(func.returntype.isInstanceOf[IR_MatrixDatatype]) => {
      var matrix = func.returntype.asInstanceOf[IR_MatrixDatatype]
      func.parameters += IR_FunctionArgument("_matrix_return", matrix)
      func.returntype = IR_UnitDatatype

      func.body = func.body.flatMap(stmt => stmt match {
        case IR_Return(Some(exp : IR_MatrixExpression)) => {
          if(exp.rows != matrix.sizeM || exp.columns != matrix.sizeN) {
            Logger.error("Matrix dimension mismatch in function " + func.name)
          }
          var stmts = ListBuffer[IR_Statement]()
          for(row <- 0 until matrix.sizeM) {
            for (col <- 0 until matrix.sizeM) {
              stmts += IR_Assignment(IR_VariableAccess("_matrix_return_" + row + "_" + col, exp.innerDatatype.getOrElse(matrix.datatype)), exp.get(row, col))
            }
          }
          stmts
        }
        case IR_Return(Some(access : IR_VariableAccess)) if(access.datatype.isInstanceOf[IR_MatrixDatatype]) => {
          var stmts = ListBuffer[IR_Statement]()
          for(row <- 0 until matrix.sizeM) {
            for (col <- 0 until matrix.sizeM) {
              stmts += IR_Assignment(IR_HighDimAccess(IR_VariableAccess("_matrix_return", matrix.datatype), IR_ConstIndex(Array(row, col))), IR_HighDimAccess(access, IR_ConstIndex(Array(row, col))))
            }
          }
          stmts
        }
        case _ => List(stmt)
      })
      func
    }
  })

  this += new Transformation("expressions 1/X", {
    // Expression results in MatrixDatatype => Upgrade everything into MatrixExp to separate it later

    case exp : IR_Addition if(exp.datatype.isInstanceOf[IR_MatrixDatatype]) => {
      var matrix = exp.datatype.asInstanceOf[IR_MatrixDatatype]
      exp.summands = exp.summands.map(summand => summand match {
        case varaccess : IR_VariableAccess if(varaccess.datatype.isInstanceOf[IR_MatrixDatatype]) => {
          var mymatrixexp = IR_MatrixExpression(matrix.datatype, matrix.sizeM, matrix.sizeN)
          for(row <- 0 until matrix.sizeM) {
            for (col <- 0 until matrix.sizeM) {
              mymatrixexp.set(row, col, IR_HighDimAccess(varaccess, IR_ConstIndex(Array(row, col))))
            }
          }
          mymatrixexp
        }
        case varaccess : IR_VariableAccess if(!varaccess.datatype.isInstanceOf[IR_MatrixDatatype]) => {
          var mymatrixexp = IR_MatrixExpression(matrix.datatype, matrix.sizeM, matrix.sizeN)
          for(row <- 0 until matrix.sizeM) {
            for (col <- 0 until matrix.sizeM) {
              mymatrixexp.set(row, col, Duplicate(varaccess))
            }
          }
          mymatrixexp
        }
        case const : IR_RealConstant => {
          var mymatrixexp = IR_MatrixExpression(matrix.datatype, matrix.sizeM, matrix.sizeN)
          for(row <- 0 until matrix.sizeM) {
            for (col <- 0 until matrix.sizeM) {
              mymatrixexp.set(row, col, Duplicate(const))
            }
          }
          mymatrixexp
        }
        case _ => summand
      }
      )
      exp
    }
  })

  this += new Transformation("expressions 2/X", {
    case exp : IR_Addition if(exp.datatype.isInstanceOf[IR_MatrixDatatype]) => {
      var matrix = exp.datatype.asInstanceOf[IR_MatrixDatatype]
      var matrixexp = IR_MatrixExpression(matrix.datatype, matrix.sizeM, matrix.sizeN)
      // matrixexp.expressions = exp.summands.reduce((a : IR_MatrixExpression, b : IR_MatrixExpression) => { a.expressions.zip(b.expressions).map(lll => lll._1 + lll._2) })
      for(row <- 0 until matrix.sizeM) {
        for(col <- 0 until matrix.sizeN) {
          matrixexp.set(row, col, exp.summands(0))
        }
      }
      for(row <- 0 until matrix.sizeM) {
        for (col <- 0 until matrix.sizeN) {
          var myexp = exp.summands(0).asInstanceOf[IR_MatrixExpression].get(row, col)
          for (i <- 1 until exp.summands.length) {
            myexp = myexp + exp.summands(i).asInstanceOf[IR_MatrixExpression].get(row, col)
          }
          matrixexp.set(row, col, myexp)
        }
      }
      matrixexp
    }
  })

/*
  this += new Transformation("declarations 2/2", { // This code might look duplicated, but should be faster for different types of initial values
    /*
    case decl @ IR_VariableDeclaration(matrix : IR_MatrixDatatype, _, None) => {
      var newDecls = ListBuffer[IR_VariableDeclaration]()
      for(row <- 0 until matrix.sizeM) {
        for(col <- 0 until matrix.sizeN) {
          newDecls += new IR_VariableDeclaration(matrix.datatype, "_matrix_" + decl.name + "_" + row + "_" + col, None)
        }
      }
      newDecls
    }
    case decl @ IR_VariableDeclaration(matrix : IR_MatrixDatatype, _, Some(x : IR_MatrixExpression)) => {
      var newDecls = ListBuffer[IR_VariableDeclaration]()
      for(row <- 0 until matrix.sizeM) {
        for(col <- 0 until matrix.sizeN) {
          var value : Option[IR_Expression] = None
          if(decl.initialValue.isDefined) {
            value = Some(x.get(row, col))
          }
          newDecls += new IR_VariableDeclaration(matrix.datatype, "_matrix_" + decl.name + "_" + row + "_" + col, value)
        }
      }
      newDecls
    }
    */
    case decl @ IR_VariableDeclaration(matrix : IR_MatrixDatatype, _, Some(x : IR_MatrixExpression)) => {
      var newStmts = ListBuffer[IR_Statement]()
      decl.initialValue = None
      newStmts += decl
      for (row <- 0 until matrix.sizeM) {
        for (col <- 0 until matrix.sizeN) {
          newStmts += IR_Assignment(IR_VariableAccess(decl), IR_HighDimAccess(IR_VariableAccess(decl), IR_ConstIndex(Array(row, col))))
        }
      }
      newStmts
    }
  })
  */

  /*
  this += new Transformation("function argument types", {
    case func :  IR_Function => { // Resolve matrix types for function declarations
      func.parameters = func.parameters.flatMap(param => {
        param.datatype match {
          case matrix : IR_MatrixDatatype => {
            var resolvedParams = ListBuffer[IR_FunctionArgument]()
            for(row <- 0 until matrix.sizeM) {
              for(col <- 0 until matrix.sizeN) {
                resolvedParams += IR_FunctionArgument("_matrix_" + param.name + "_" + row + "_" + col, matrix.datatype)
              }
            }
            resolvedParams
          }
          case _ => List(param)
        }
      })
      func
    }
    case expstmt @ IR_ExpressionStatement(func : IR_FunctionCall) => { // Resolve matrix types for standalone function calls
      if(func.function.datatype.isInstanceOf[IR_MatrixDatatype]) { // change function access type
        func.function = IR_UserFunctionAccess(func.function.name, func.function.datatype.asInstanceOf[IR_MatrixDatatype].datatype)
      }
      func.arguments = func.arguments.flatMap(param => {
        var resolvedParams = ListBuffer[IR_Expression]()
        param match {
          case matrix : IR_MatrixExpression => {
            for(row <- 0 until matrix.rows) {
              for(col <- 0 until matrix.columns) {
                resolvedParams += matrix.get(row, col)
              }
            }
          }
          case access : IR_VariableAccess if(access.datatype.isInstanceOf[IR_MatrixDatatype]) => {
            val matrix = access.datatype.asInstanceOf[IR_MatrixDatatype]
            for (row <- 0 until matrix.sizeM) {
              for (col <- 0 until matrix.sizeN) {
                resolvedParams += IR_VariableAccess("_matrix_" + access.name + "_" + row + "_" + col, matrix.datatype)
              }
            }
          }
          // No other IR_Expression with Datatype == IR_MatrixDatatype should exist here
          case _ => resolvedParams += param
        }
        resolvedParams
      })
      expstmt
    }
  })

  this += new Transformation("return types", {
    case func : IR_Function if(func.returntype.isInstanceOf[IR_MatrixDatatype]) => {
      var matrix = func.returntype.asInstanceOf[IR_MatrixDatatype]
      for(row <- 0 until matrix.sizeM) {
        for(col <- 0 until matrix.sizeN) {
          var arg = IR_FunctionArgument("_matrix_return_" + row + "_" + col, IR_ReferenceDatatype(matrix.datatype))
          func.parameters += arg
        }
      }
      func.returntype = IR_UnitDatatype
      func.body = func.body.flatMap(stmt => stmt match {
        case IR_Return(Some(exp : IR_MatrixExpression)) => {
          if(exp.rows != matrix.sizeM || exp.columns != matrix.sizeN) {
            Logger.error("Matrix dimension mismatch in function " + func.name)
          }
          var stmts = ListBuffer[IR_Statement]()
          for(row <- 0 until matrix.sizeM) {
            for (col <- 0 until matrix.sizeM) {
              stmts += IR_Assignment(IR_VariableAccess("_matrix_return_" + row + "_" + col, exp.innerDatatype.getOrElse(matrix.datatype)), exp.get(row, col))
            }
          }
          stmts
        }
        case IR_Return(Some(access : IR_VariableAccess)) if(access.datatype.isInstanceOf[IR_MatrixDatatype]) => {
          var stmts = ListBuffer[IR_Statement]()
          for(row <- 0 until matrix.sizeM) {
            for (col <- 0 until matrix.sizeM) {
              stmts += IR_Assignment(IR_VariableAccess("_matrix_return_" + row + "_" + col, matrix.datatype), IR_VariableAccess("_matrix_" + access.name + "_" + row + "_" + col, matrix.datatype))
            }
          }
          stmts
        }
        case _ => List(stmt)
      })
      func
    }
  })*/

  this += new Transformation("assignments", {
    case IR_Assignment(dst : IR_VariableAccess, src : IR_VariableAccess, _) if (dst.datatype.isInstanceOf[IR_MatrixDatatype] && src.datatype.isInstanceOf[IR_MatrixDatatype]) => {
      var dstdt = dst.datatype.asInstanceOf[IR_MatrixDatatype]
      var srcdt = src.datatype.asInstanceOf[IR_MatrixDatatype]
      if (dstdt.sizeM != srcdt.sizeM || dstdt.sizeN != srcdt.sizeN) {
        Logger.error("Matrix dimension mismatch in assignment")
      }
      var newStmts = ListBuffer[IR_Assignment]()
      for (row <- 0 until dstdt.sizeM) {
        for (col <- 0 until dstdt.sizeN) {
          newStmts += IR_Assignment(IR_HighDimAccess(dst, IR_ConstIndex(Array(row, col))), IR_HighDimAccess(src, IR_ConstIndex(Array(row, col))))
        }
      }
      newStmts
    }

    case IR_Assignment(dst : IR_VariableAccess, src : IR_FunctionCall, _) if(dst.datatype.isInstanceOf[IR_MatrixDatatype] && src.datatype.isInstanceOf[IR_MatrixDatatype]) => {
      var dstdt = dst.datatype.asInstanceOf[IR_MatrixDatatype]
      var srcdt = src.datatype.asInstanceOf[IR_MatrixDatatype]
      if(dstdt.sizeM != srcdt.sizeM || dstdt.sizeN != srcdt.sizeN) {
        Logger.error("Matrix dimension mismatch in assignment")
      }

      /*
      src.function = IR_UserFunctionAccess(src.function.name, srcdt.datatype)
      src.arguments = src.arguments.flatMap(param => {
        var resolvedParams = ListBuffer[IR_Expression]()
        param match {
          case matrix : IR_MatrixExpression => {
            for(row <- 0 until matrix.rows) {
              for(col <- 0 until matrix.columns) {
                resolvedParams += matrix.get(row, col)
              }
            }
          }
          // Leave other arguments alone
          case _ => resolvedParams += param
        }
        resolvedParams
      })
      */

      // add return variable to function call
      src.arguments += dst

      // FIXME this will result in invalid code if Function returns something, but is called without assignment!
      // e.g.:  Function foo(bar : Matrix<Real,2,2>) : Matrix<Real,2,2> { ... }
      //        Function Application() { foo(...); }

      IR_ExpressionStatement(src)
    }

  /*
    this += new Transformation("assignments", {
      case IR_Assignment(dst : IR_VariableAccess, src : IR_VariableAccess, _) if(dst.datatype.isInstanceOf[IR_MatrixDatatype] && src.datatype.isInstanceOf[IR_MatrixDatatype]) => {
        var dstdt = dst.datatype.asInstanceOf[IR_MatrixDatatype]
        var srcdt = src.datatype.asInstanceOf[IR_MatrixDatatype]
        if(dstdt.sizeM != srcdt.sizeM || dstdt.sizeN != srcdt.sizeN) {
          Logger.error("Matrix dimension mismatch in assignment")
        }
        var newStmts = ListBuffer[IR_Assignment]()
        for(row <- 0 until dstdt.sizeM) {
          for(col <- 0 until dstdt.sizeN) {
            newStmts += IR_Assignment(IR_VariableAccess("_matrix_" + dst.name + "_" + row + "_" + col, dstdt.datatype), IR_VariableAccess("_matrix_" + src.name + "_" + row + "_" + col, srcdt.datatype))
          }
        }
        newStmts
      }
      case IR_Assignment(dst : IR_VariableAccess, src : IR_FunctionCall, _) if(dst.datatype.isInstanceOf[IR_MatrixDatatype] && src.datatype.isInstanceOf[IR_MatrixDatatype]) => {
        var dstdt = dst.datatype.asInstanceOf[IR_MatrixDatatype]
        var srcdt = src.datatype.asInstanceOf[IR_MatrixDatatype]
        if(dstdt.sizeM != srcdt.sizeM || dstdt.sizeN != srcdt.sizeN) {
          Logger.error("Matrix dimension mismatch in assignment")
        }

        src.function = IR_UserFunctionAccess(src.function.name, srcdt.datatype)
        src.arguments = src.arguments.flatMap(param => {
          var resolvedParams = ListBuffer[IR_Expression]()
          param match {
            case matrix : IR_MatrixExpression => {
              for(row <- 0 until matrix.rows) {
                for(col <- 0 until matrix.columns) {
                  resolvedParams += matrix.get(row, col)
                }
              }
            }
            case access : IR_VariableAccess if(access.datatype.isInstanceOf[IR_MatrixDatatype]) => {
              val matrix = access.datatype.asInstanceOf[IR_MatrixDatatype]
              for (row <- 0 until matrix.sizeM) {
                for (col <- 0 until matrix.sizeN) {
                  resolvedParams += IR_VariableAccess("_matrix_" + access.name + "_" + row + "_" + col, matrix.datatype)
                }
              }
            }
            // Leave other arguments alone
            case _ => resolvedParams += param
          }
          resolvedParams
        })

        // add return variable to function call
        for(row <- 0 until dstdt.sizeM) {
          for(col <- 0 until dstdt.sizeN) {
            var arg = IR_VariableAccess("_matrix_" + dst.name + "_" + row + "_" + col, dstdt.datatype)
            src.arguments += arg
          }
        }

        // FIXME this will result in invalid code if Function returns something, but is called without assignment!
        // e.g.:  Function foo(bar : Matrix<Real,2,2>) : Matrix<Real,2,2> { ... }
        //        Function Application() { foo(...); }

        IR_ExpressionStatement(src)
      }*/
    case IR_Assignment(dst : IR_VariableAccess, src : IR_MatrixExpression, _) if(dst.datatype.isInstanceOf[IR_MatrixDatatype]) => {
      var dstdt = dst.datatype.asInstanceOf[IR_MatrixDatatype]
      var srcdt = src.datatype.asInstanceOf[IR_MatrixDatatype]
      if(dstdt.sizeM != srcdt.sizeM || dstdt.sizeN != srcdt.sizeN) {
        Logger.error("Matrix expression dimensions must match for assignment")
      }
      var stmts = ListBuffer[IR_Assignment]()
      for(row <- 0 until dstdt.sizeM) {
        for (col <- 0 until dstdt.sizeN) {
          stmts += IR_Assignment(IR_HighDimAccess(dst, IR_ConstIndex(Array(row, col))), src.get(row, col))
        }
      }
      stmts.toList
    }
  })
/*
  this += new Transformation("VariableAccesses", {
    case IR_VariableAccess(name, matrix : IR_MatrixDatatype) => {
      var exps = ListBuffer[IR_Expression]()
      for(row <- 0 until matrix.sizeM) {
        for (col <- 0 until matrix.sizeN) {
          exps += IR_VariableAccess("_matrix_" + name + "_" + row + "_" + col, matrix.datatype)
        }
      }
      IR_MatrixExpression(Some(matrix.datatype), matrix.sizeM, matrix.sizeN, exps.toArray)
    }
  })
  */
}