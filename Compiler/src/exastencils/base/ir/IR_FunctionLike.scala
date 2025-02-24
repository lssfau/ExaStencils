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

trait IR_FunctionLike extends IR_Statement {
  def name : String
  def name_=(newName : String)

  def prettyprint_decl() : String

  var isHeaderOnly : Boolean = false
  var allowInlining : Boolean = true
  var allowFortranInterface : Boolean = true
  var functionQualifiers : String = "" // e.g. "__global__" etc
}

trait IR_PlainFunctionLike extends IR_Function

trait IR_LeveledFunctionLike extends IR_Function {
  def baseName : String
  def level : Int
}
