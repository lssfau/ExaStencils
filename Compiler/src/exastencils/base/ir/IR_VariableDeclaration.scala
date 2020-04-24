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

package exastencils.base.ir

//import java.lang.System.Logger
import exastencils.logger.Logger

import exastencils.baseExt.ir._
import exastencils.config.Platform
import exastencils.prettyprinting.PpStream

object IR_VariableDeclaration {
  def apply(datatype : IR_Datatype, name : String, initialValue : IR_Expression)
  = new IR_VariableDeclaration(datatype, name, Option(initialValue))
  def apply(variable : IR_VariableAccess)
  = new IR_VariableDeclaration(variable.datatype, variable.name, None)
  def apply(variable : IR_VariableAccess, initialValue : IR_Expression)
  = new IR_VariableDeclaration(variable.datatype, variable.name, Some(initialValue))
}

case class IR_VariableDeclaration(var datatype : IR_Datatype, var name : String, var initialValue : Option[IR_Expression] = None, var isConst : Boolean = false) extends IR_Statement {
  var alignment : Int = 1
  val acc = IR_HighDimAccess
  val index = IR_ConstIndex

  override def prettyprint(out : PpStream) : Unit = {
    // TODO: extract specialized behavior
    datatype match {
      case dt : IR_VectorDatatype =>
        out << dt << ' ' << name
        if (initialValue.isDefined) {
          out << "("
          initialValue.get match {
            case init : IR_VectorExpression => init.prettyprintInner(out)
            case sthElse                    => out << sthElse
          }
          out << ")"
        }

      case dt : IR_MatrixDatatype =>
        dt.prettyprint(out)
        out << ' ' << name
        initialValue match {
          case Some(e : IR_MatrixExpression)                           => out << ' '; e.prettyprintInner(out)
          case Some(e) if (e.datatype.isInstanceOf[IR_ScalarDatatype]) => out << ' ' << '{'; for (i <- 0 until dt.sizeM * dt.sizeN) { e.prettyprint(out); out << ',' }; out.removeLast(); out << '}'
          case Some(e)                                                 => out << " = " << e
          case _                                                       =>
        }

      case dt : IR_TensorDatatype1 =>
        dt.prettyprint(out)
        out << ' ' << name
        initialValue match {
          case Some(e : IR_TensorExpression1)                                             => out << ' ' << '{'; e.expressions(0).prettyprint(out); out << ","; e.expressions(1).prettyprint(out); out<< ","; e.expressions(2).prettyprint(out); out << '}'
          case Some(e : IR_VariableAccess) if e.datatype.isInstanceOf[IR_TensorDatatype1] => out << ' ' << '{'; acc(e, index(0)).prettyprint(out); out << ","; acc(e, index(1)).prettyprint(out); out  << ","; acc(e, index(2)).prettyprint(out); out << '}'
          case Some(e) if (e.datatype.isInstanceOf[IR_ScalarDatatype])                    => out << ' ' << '{'; for (i <- 0 until 3) { e.prettyprint(out); out << ',' }; out.removeLast(); out << '}'
          case Some(e)                                                                    => Logger.error(e.toString)
          case _                                                                          =>
        }

      case dt : IR_TensorDatatype2 =>
        dt.prettyprint(out)
        out << ' ' << name
        initialValue match {
          case Some(e : IR_TensorExpression2)                                             => out << ' ' << '{' << e.expressions.map(_.prettyprint).mkString(","); out << '}'
          case Some(e : IR_VariableAccess) if e.datatype.isInstanceOf[IR_TensorDatatype2] => out << ' ' << '{'; for (i <- 0 until 9) {  acc(e, index(i)).prettyprint(out); out << "," }; out.removeLast(); out << "}"
          case Some(e) if (e.datatype.isInstanceOf[IR_ScalarDatatype])                    => out << ' ' << '{'; for (i <- 0 until 9) { e.prettyprint(out); out << ',' }; out.removeLast(); out << '}'
          case Some(e)                                                                    => out << " = " << e
          case _                                                                          =>
        }

      case dt : IR_TensorDatatypeN =>
        dt.prettyprint(out)
        out << ' ' << name
        initialValue match {
          case Some(e : IR_TensorExpressionN)                          => out << ' ' << '{' << e.expressions.map(_.prettyprint).mkString(","); out << '}' //TODO: Zeus, hier fehlt noch die Zuweisung mit IR_VariableAccess
          case Some(e) if (e.datatype.isInstanceOf[IR_ScalarDatatype]) => out << ' ' << '{'; for (i <- 0 until scala.math.pow(3, dt.order.toDouble).toInt) { e.prettyprint(out); out << ',' }; out.removeLast(); out << '}'
          case Some(e)                                                 => out << " = " << e
          case _                                                       =>
        }
      case _                       =>
        if (alignment > 1 && "MSVC" == Platform.targetCompiler)
          out << "__declspec(align(" << alignment * 8 << ")) "
        out << datatype.resolveDeclType << ' ' << name << datatype.resolveDeclPostscript
        if (alignment > 1 && "MSVC" != Platform.targetCompiler)
          out << " __attribute__((aligned(" << alignment * 8 << ")))"
        if (initialValue.isDefined)
          out << " = " << initialValue.get

    }
    out << ';'
  }

  /// prints only the declaration, ie omits (potential) initialization
  def prettyprintDeclaration() : String = IR_VariableDeclaration(datatype, name, None).prettyprint()

}
