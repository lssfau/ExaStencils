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

package exastencils.baseExt.l4

import exastencils.base.ProgressLocation
import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.baseExt.ir.IR_ComplexAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.l4.L4_VariableDeclarationCollector

/// L4_ComplexAccess
object L4_ComplexAccess {
  def apply(name : String) = new L4_ComplexAccess(name, None, None, None)
  def apply(name : String, level : Option[L4_AccessLevelSpecification]) = new L4_ComplexAccess(name, level, None, None)
  def apply(name : String, level : Option[L4_AccessLevelSpecification], arrayIndex : Option[String]) =
    new L4_ComplexAccess(name, level, arrayIndex, None)
  def apply(name : String, level : Option[L4_AccessLevelSpecification], arrayIndex : Option[String], mulDimIndex : Option[List[String]]) =
    new L4_ComplexAccess(name, level, arrayIndex, mulDimIndex)

}

case class L4_ComplexAccess(
    var name : String,
    var level : Option[L4_AccessLevelSpecification],
    var arrayIndex : Option[String],
    var mulDimIndex : Option[List[String]]) extends L4_Access { //TODO: Hier soll Option raus, nicht notwendig da * Operator

  val declCollector = new L4_VariableDeclarationCollector

  def prettyprint(out : PpStream) = {
    out << name
    if (level.isDefined) out << '@' << level.get
    if (arrayIndex.isDefined) out << ({
      arrayIndex match {
        case Some(i)  => Array("[", i.toString , "]").mkString("")
        case None     => Logger.error("That didn't work.")
      }
    }).toString
    if (mulDimIndex.isDefined) out << ({
      mulDimIndex match {
        case Some(i) =>
          var string = "["
          for (k <- i.indices.reverse) {
            string += Array(i(k).toString , ",").mkString("")
          }
          string = string.dropRight(1) + "]"
          string
        case None => Logger.error("That didn't work.")
      }
    })
  }

  override def progress : IR_Expression = ProgressLocation {

    Logger.warn(s"Progressing unresolved access on L4: $name" + (if (level.isDefined) s"@${ level.get }" else ""))

    if (arrayIndex.isDefined) Logger.warn("Discarding meaningless array index access on basic or leveled access")
    if (mulDimIndex.isDefined) Logger.warn("Discarding meaningless array index access on basic or leveled access")

    declCollector.reset()

    //if (!declCollector.exists(name)) Logger.error("Access on not defined value")

    val decl = declCollector.getDeclaration(name).progress

    if (arrayIndex.isDefined)
      IR_ComplexAccess(name, decl, arrayIndex, None)
    else
      IR_ComplexAccess(name, decl, None, mulDimIndex)
  }
}
