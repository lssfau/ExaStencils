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

package exastencils.simd

import exastencils.base.ir._
import exastencils.config._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_ResultingDatatype

/// (scalar) arithmetic operations

/// SIMD_Addition

case class SIMD_Addition(var left : IR_Expression, var right : IR_Expression) extends SIMD_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Platform.simd_instructionSet match {
      case "SSE3"            => out << "_mm_add_p" << prec
      case "AVX" | "AVX2"    => out << "_mm256_add_p" << prec
      case "AVX512" | "IMCI" => out << "_mm512_add_p" << prec
      case "QPX"             => out << "vec_add"
      case "NEON"            => out << "vaddq_f32"
    }
    out << '(' << left << ", " << right << ')'
  }
}

/// SIMD_Subtraction

case class SIMD_Subtraction(var left : IR_Expression, var right : IR_Expression) extends SIMD_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Platform.simd_instructionSet match {
      case "SSE3"            => out << "_mm_sub_p" << prec
      case "AVX" | "AVX2"    => out << "_mm256_sub_p" << prec
      case "AVX512" | "IMCI" => out << "_mm512_sub_p" << prec
      case "QPX"             => out << "vec_sub"
      case "NEON"            => out << "vsubq_f32"
    }
    out << '(' << left << ", " << right << ')'
  }
}

/// SIMD_Multiplication

case class SIMD_Multiplication(var left : IR_Expression, var right : IR_Expression) extends SIMD_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Platform.simd_instructionSet match {
      case "SSE3"            => out << "_mm_mul_p" << prec
      case "AVX" | "AVX2"    => out << "_mm256_mul_p" << prec
      case "AVX512" | "IMCI" => out << "_mm512_mul_p" << prec
      case "QPX"             => out << "vec_mul"
      case "NEON"            => out << "vmulq_f32"
    }
    out << '(' << left << ", " << right << ')'
  }
}

/// SIMD_Division

case class SIMD_Division(var left : IR_Expression, var right : IR_Expression) extends SIMD_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  // FIXME
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Platform.simd_instructionSet match {
      case "SSE3"         => out << "_mm_div_p" << prec
      case "AVX" | "AVX2" => out << "_mm256_div_p" << prec
      case "AVX512"       => out << "_mm512_div_p" << prec
      case "IMCI"         => throw new InternalError("not yet supported...") // TODO: support it! but there is no div :(
      case "QPX"          => out << "vec_swdiv_nochk" // double precision division performed here, single precision would also be possible... what's better?
      case "NEON"         => out << "vdivq_f32"
    }
    out << '(' << left << ", " << right << ')'
  }
}

/// SIMD_Minimum

case class SIMD_Minimum(var left : IR_Expression, var right : IR_Expression) extends SIMD_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = {
    if (Platform.simd_instructionSet == "QPX") // TODO: export function
      out << "vec_sel(" << right << ", " << left << ", vec_cmplt(" << left << ", " << right << "))" // vec_sel selects the second if the third represents true...
    else {
      val prec = if (Knowledge.useDblPrecision) 'd' else 's'
      Platform.simd_instructionSet match {
        case "SSE3"         => out << "_mm_min_p" << prec
        case "AVX" | "AVX2" => out << "_mm256_min_p" << prec
        case "AVX512"       => out << "_mm512_min_p" << prec
        case "IMCI"         => out << "_mm512_gmin_p" << prec
        case "NEON"         => out << "vmin_f32"
      }
      out << '(' << left << ", " << right << ')'
    }
  }
}

/// SIMD_Maximum

case class SIMD_Maximum(var left : IR_Expression, var right : IR_Expression) extends SIMD_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = {
    if (Platform.simd_instructionSet == "QPX") // TODO: export function
      out << "vec_sel(" << right << ", " << left << ", vec_cmpgt(" << left << ", " << right << "))" // vec_sel selects the second if the third represents true...
    else {
      val prec = if (Knowledge.useDblPrecision) 'd' else 's'
      Platform.simd_instructionSet match {
        case "SSE3"         => out << "_mm_max_p" << prec
        case "AVX" | "AVX2" => out << "_mm256_max_p" << prec
        case "AVX512"       => out << "_mm512_max_p" << prec
        case "IMCI"         => out << "_mm512_gmax_p" << prec
        case "NEON"         => out << "vmax_f32"
      }
      out << '(' << left << ", " << right << ')'
    }
  }
}

/// logical comparison operations

trait SIMD_Compare extends SIMD_Expression {
  def left : IR_Expression
  def right : IR_Expression
  def op : String
  override def datatype = SIMD_RealDatatype
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Platform.simd_instructionSet match {
      case "SSE3"         => out << s"_mm_cmp${op}_p" << prec
      case "AVX" | "AVX2" => out << s"_mm256_cmp${op}_p" << prec // TODO: wrong signature for both
      case "AVX512"       => Logger.error("Currently unsupported")
      case "IMCI"         => Logger.error("Currently unsupported")
      case "NEON"         => Logger.error("Currently unsupported")
      case "QPX"          => Logger.error("Currently unsupported")
    }
    out << '(' << left << ", " << right << ')'
  }
}

case class SIMD_EqEq(var left : IR_Expression, var right : IR_Expression) extends SIMD_Compare {
  override def op : String = "eq"
}

case class SIMD_Neq(var left : IR_Expression, var right : IR_Expression) extends SIMD_Compare {
  override def op : String = "neq"
}

case class SIMD_Lower(var left : IR_Expression, var right : IR_Expression) extends SIMD_Compare {
  override def op : String = "lt"
}

case class SIMD_Greater(var left : IR_Expression, var right : IR_Expression) extends SIMD_Compare {
  override def op : String = "gt"
}

case class SIMD_LowerEqual(var left : IR_Expression, var right : IR_Expression) extends SIMD_Compare {
  override def op : String = "le"
}

case class SIMD_GreaterEqual(var left : IR_Expression, var right : IR_Expression) extends SIMD_Compare {
  override def op : String = "ge"
}

/// (scalar) logical operations

trait SIMD_BitwiseOp extends SIMD_Expression {
  def left : IR_Expression
  def right : IR_Expression
  def op : String
  override def datatype = SIMD_RealDatatype
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Platform.simd_instructionSet match {
      case "SSE3"         => out << s"_mm_${op}_p" << prec
      case "AVX" | "AVX2" => out << s"_mm256_${op}_p" << prec
      case "AVX512"       => Logger.error("Currently unsupported")
      case "IMCI"         => Logger.error("Currently unsupported")
      case "NEON"         => Logger.error("Currently unsupported")
      case "QPX"          => Logger.error("Currently unsupported")
    }
    out << '(' << left << ", " << right << ')'
  }
}

case class SIMD_BitwiseAnd(var left : IR_Expression, var right : IR_Expression) extends SIMD_BitwiseOp {
  override def op : String = "and"
}

case class SIMD_BitwiseOr(var left : IR_Expression, var right : IR_Expression) extends SIMD_BitwiseOp {
  override def op : String = "or"
}
