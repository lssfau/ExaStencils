//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.logger.Logger
import exastencils.prettyprinting._
import exastencils.util.ir._
import exastencils.util.l4.L4_VariableDeclarationCollector

/** Factory for IR_ComplexAccess objects */
object IR_ComplexAccess {
  def apply(name : String, decl : IR_VariableDeclaration) = new IR_ComplexAccess(name, decl, None, None)
}


case class IR_ComplexAccess(var name : String, var decl : IR_VariableDeclaration, var arrayIndex : Option[String], var mulDimIndex : Option[List[String]]) extends IR_Access {

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " <<
    this.getClass.getName << "\n"
  override def datatype : IR_Datatype = decl.datatype

  def isNUmeric(x : Char) : Boolean = {
    if ((x < 48) || (x > 57)) return false
    true
  }

  def resolve() : IR_Expression = {
    val acc = IR_VariableAccess(name, decl.datatype)
    if (arrayIndex.isDefined) {
      IR_StringConstant("Complex Array Access not yet implemented")
    } else if (mulDimIndex.isDefined) {
      mulDimIndex match {
        case Some(i) if !(i.exists(k => k.exists(l => !isNUmeric(l)))) => // case classic access
          val l = i.asInstanceOf[List[Int]]
          acc.datatype match {
            case _ : IR_MatrixDatatype =>
              if (l.length > 2) Logger.error("Matrix Access with more than 2 axes not yet implemented")
              val tmp = acc.datatype.asInstanceOf[IR_MatrixDatatype]
              if (l(0) > tmp.sizeM) Logger.error("Matrix Access x index out of bounds")
              if (l(1) > tmp.sizeN) Logger.error("Matrix Access y index out of bounds")
              IR_StringConstant(Array(name, "[", l(0) + l(1) * tmp.sizeN , "]").mkString(""))

            case _ : IR_TensorDatatype2 =>
              if (l.length > 2) Logger.error("Order 2 Tensor can not be accessed with more than 2 dimensions")
              IR_HighDimAccess(Duplicate(acc), IR_ExpressionIndex(l.toArray))
              val tmp = acc.datatype.asInstanceOf[IR_TensorDatatype2]
              if (l(0) > tmp.dims) Logger.error("Matrix Access x index out of bounds")
              if (l(1) > tmp.dims) Logger.error("Matrix Access y index out of bounds")
              IR_StringConstant(Array(name, "[", l(0) + l(1) * tmp.dims , "]").mkString(""))

            case _ : IR_TensorDatatypeN =>
              val tmp = acc.datatype.asInstanceOf[IR_TensorDatatypeN]
              if (tmp.order != i.length) Logger.error("Access to Tensor has wrong dimensionality")
              if (l.exists(a => a > tmp.order)) Logger.error("Tensor Acces index out of bounds")
              // Later it is possible to accept tmp.dims >= i.length
              // f.e.to access like [0, 1, :] = [0, 1] for a 3 dimensional tensor
              var index : Double = 0.0
              for (k <- i.indices.reverse) {
                index = index + l(k) * scala.math.pow(tmp.dims, (l.length - k - 1).toDouble)
              }
              IR_StringConstant(Array(name, "[", index , "]").mkString(""))
            case _ => Logger.error("Complex Access got not supported data type")
          }
        case Some(i) if i.isInstanceOf[List[String]] =>
          if (i.asInstanceOf[List[String]].exists(k => k.length > 1)) Logger.error("Complex Access got strange indeces")
          var l = ListBuffer[Char]()
          i.foreach(a => (l += a(0)))
          var myind : List[Any] = Nil
          for (k <- l) {
            if (!isNUmeric(k)) {
              val index : Int = myind.indexOf(k)
              if (index != -1) {
                myind = myind.updated(index, myind.size) // a number points to the index of the next similar element
                // [a , 4, c, d, e] means ind 1 == ind 4 == e
                // TODO: Zeus der Algorithmus muss dringend in die Thesis!
              }
              myind = myind :+ k
            }
          }
          acc.datatype match {
            case _ : IR_MatrixDatatype =>
              if (l.length > 2) Logger.error("Matrix Access with more than 2 axes not yet implemented")
              val tmp = acc.datatype.asInstanceOf[IR_MatrixDatatype]
              if (isNUmeric(l(0)) && !isNUmeric(l(1))) { // Case
                if (l(0).asInstanceOf[Int] > tmp.sizeM) Logger.error("Matrix Access x index out of bounds")
                val res = IR_MatrixExpression(acc.datatype, tmp.sizeM, tmp.sizeN)
                for (i <- 0 until tmp.sizeM) res.set(l(0).asInstanceOf[Int], i, IR_HighDimAccess(acc, IR_ExpressionIndex(Array(l(0).asInstanceOf[Int], i))))
                res
              } else if (!isNUmeric(l(0)) && isNUmeric(l(1))) {
                if (l(1).asInstanceOf[Int] > tmp.sizeN) Logger.error("Matrix Access y index out of bounds")
                val res = IR_MatrixExpression(acc.datatype, tmp.sizeM, tmp.sizeN)
                for (i <- 0 until tmp.sizeM) res.set(i, l(1).asInstanceOf[Int], IR_HighDimAccess(acc, IR_ExpressionIndex(Array(i, l(1).asInstanceOf[Int]))))
                res
              } else {
                acc
              }
            case _ : IR_TensorDatatype2 =>
              if (l.length > 2) Logger.error("Order 2 Tensor can not be accessed with more than 2 dimensions")
              val tmp = acc.datatype.asInstanceOf[IR_TensorDatatype2]
              if (isNUmeric(l(0)) && !isNUmeric(l(1))) { // Case
                if (l(0).asInstanceOf[Int] > tmp.dims) Logger.error("Tensor Access x index out of bounds")
                val res = IR_TensorExpression2(acc.datatype, tmp.dims)
                for (i <- 0 until res.dims) res.set(l(0).asInstanceOf[Int], i, IR_HighDimAccess(acc, IR_ExpressionIndex(Array(l(0).asInstanceOf[Int], i))))
                res
              } else if (!isNUmeric(l(0)) && isNUmeric(l(1))) {
                if (l(1).asInstanceOf[Int] > tmp.dims) Logger.error("Tensor Access y index out of bounds")
                val res = IR_TensorExpression2(acc.datatype, tmp.dims)
                for (i <- 0 until res.dims) res.set(i, l(1).asInstanceOf[Int], IR_HighDimAccess(acc, IR_ExpressionIndex(Array(i, l(1).asInstanceOf[Int]))))
                res
              } else {
                acc
              }
            case _ : IR_TensorDatatypeN => Logger.error("Tensor N not yet implemented")

            case _ => Logger.error("Complex Access got not supported data type")
          }

        case _ => Logger.error("Complex Access got not supported data type")
      }
    } else {
      IR_StringConstant("Not fully implemented")
    }
  }
}

/** Declaration of tensor functions and transformation nodes */
object IR_ResolveComplexAccess extends DefaultStrategy("Resolve user defined functions") {
  var declCollector = new L4_VariableDeclarationCollector
  this.register(declCollector)

  def myresolve(access : IR_ComplexAccess) : IR_Expression = {
    access.resolve()
  }

  this += new Transformation("add assignments/decl to function returns to arguments", {
    //case access : IR_ComplexAccess if declCollector.existsPlain(access.name) => myresolve(access)
    case access : IR_ComplexAccess => myresolve(access)
  })
}