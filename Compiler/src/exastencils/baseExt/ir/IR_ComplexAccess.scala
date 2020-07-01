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
  def apply(name : String) = new IR_ComplexAccess(name, None, None)
}


case class IR_ComplexAccess(var name : String, var arrayIndex : Option[String], var mulDimIndex : Option[List[String]]) extends IR_Expression {
  val declCollector = new L4_VariableDeclarationCollector

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " <<
    this.getClass.getName << "\n"
  override def datatype : IR_Datatype = declCollector.getDeclaration(name).progress.datatype

  def resolve() : IR_Expression = {
    val decl : IR_VariableDeclaration = declCollector.getDeclaration(name).progress
    val acc = IR_VariableAccess(name, decl.datatype)
    if (arrayIndex.isDefined) {
      IR_StringConstant("Complex Array Access not yet implemented")
    } else if (mulDimIndex.isDefined) {
      mulDimIndex match {
        case Some(i) if !(i.exists(k => k.exists(l => l.isDigit))) => // case classic access
          val l = i.asInstanceOf[List[Int]]
          acc.datatype match {
            case dat : IR_MatrixDatatype =>
              if (l.length > 2) Logger.error("Matrix Access with more than 2 axes not yet implemented")
              val tmp = acc.datatype.asInstanceOf[IR_MatrixDatatype]
              if (l(0) > tmp.sizeM) Logger.error("Matrix Access x index out of bounds")
              if (l(1) > tmp.sizeN) Logger.error("Matrix Access y index out of bounds")
              IR_StringConstant(Array(name, "[", l(0) + l(1) * tmp.sizeN , "]").mkString(""))

            case dat : IR_TensorDatatype2 =>
              if (l.length > 2) Logger.error("Order 2 Tensor can not be accessed with more than 2 dimensions")
              IR_HighDimAccess(Duplicate(acc), IR_ExpressionIndex(l.toArray))
              val tmp = acc.datatype.asInstanceOf[IR_TensorDatatype2]
              if (l(0) > tmp.dims) Logger.error("Matrix Access x index out of bounds")
              if (l(1) > tmp.dims) Logger.error("Matrix Access y index out of bounds")
              IR_StringConstant(Array(name, "[", l(0) + l(1) * tmp.dims , "]").mkString(""))

            case dat : IR_TensorDatatypeN =>
              val tmp = acc.datatype.asInstanceOf[IR_TensorDatatypeN]
              if (tmp.order != i.length) Logger.error("Access to Tensor has wrong dimensionality")
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
          val l = i.asInstanceOf[List[String]]
          for (k <- l) {
            if (!(k.isInstanceOf[Int]) || !(k.isInstanceOf[Char])) Logger.error("Complex Acces got strange indeces")
          }
          var myind : List[Any] = Nil
          for (k <- l) {
            if (k.isInstanceOf[Char]) {
              val index : Int = myind.indexOf(k)
              if (index != -1) {
                myind = myind.updated(index, myind.size) // a number points to the index of the next similar element
                // [a , 4, c, d, e] means ind 1 == ind 4 == e
                // TODO: Zeus der Algorithmus muss dringend in die Thesis!
              }
              myind = myind :+ k
            }
          }
          /*acc.datatype match {
            case dat : IR_TensorDatatype2 =>
              if (l.length > 2) Logger.error("Order 2 Tensor can not be accessed with more than 2 dimensions")
              IR_HighDimAccess(Duplicate(acc), IR_ExpressionIndex(Array(i)))
              val tmp = acc.datatype.asInstanceOf[IR_TensorDatatype2]
              if (l(0).isInstanceOf[Int]) {
                if (l(0).asInstanceOf[Int] > tmp.dims) Logger.error("Matrix Access x index out of bounds")
              }
              if (l(1).isInstanceOf[Int]) {
                if (l(1).asInstanceOf[Int] > tmp.dims) Logger.error("Matrix Access y index out of bounds")
              }
              IR_StringConstant(Array(name, "[", l(0) + l(1) * tmp.dims, "]").mkString(""))
          }*/
          Logger.error("Not fully implemeneted")

        case _ => Logger.error("Complex Access got not supported data type")
      }

      IR_StringConstant("Not fully implemented")
    } else {
      IR_StringConstant("Not fully implemented")
    }
  }
}

/** Declaration of tensor functions and transformation nodes */
object IR_ResolveComplexAccess extends DefaultStrategy("Resolve user defined functions") {
  var declCollector = new L4_VariableDeclarationCollector
  this.register(declCollector)

  this += new Transformation("add assignments/decl to function returns to arguments", {
    case access : IR_ComplexAccess if declCollector.existsPlain(access.name) =>
        IR_ExpressionStatement(access.resolve())
  })
}