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
import exastencils.optimization.ir.VectorizationException
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
      case "NEON"            => out << "vaddq_" << (if (Knowledge.useDblPrecision) "f64" else "f32")
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
      case "NEON"            => out << "vsubq_" << (if (Knowledge.useDblPrecision) "f64" else "f32")
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
      case "NEON"            => out << "vmulq_" << (if (Knowledge.useDblPrecision) "f64" else "f32")
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
      case "NEON"         => out << "vdivq_" << (if (Knowledge.useDblPrecision) "f64" else "f32")
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
        case "NEON"         => out << "vminq_" << (if (Knowledge.useDblPrecision) "f64" else "f32")
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
        case "NEON"         => out << "vmaxq_" << (if (Knowledge.useDblPrecision) "f64" else "f32")
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
  def ordered : Boolean = true
  def signalling : Boolean = false
  override def datatype = SIMD_MaskDatatype
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    val precBits = if (Knowledge.useDblPrecision) 64 else 32
    val ord = if (ordered) "O" else "U"
    val signal = if (signalling) "S" else "Q"
    Platform.simd_instructionSet match {
      case "SSE3"         => out << s"_mm_cmp${op}_p" << prec
      case "AVX" | "AVX2" => out << s"_mm256_cmp_p" << prec
      case "AVX512"       => out << s"_mm512_cmp_p" << prec << "_mask"
      case "NEON"         => out << s"vc${op}q_f$precBits"
      // TODO
      case "IMCI"         => new VectorizationException("SIMD_Compare: Currently unsupported for " + Platform.simd_instructionSet)
      case "QPX"          => new VectorizationException("SIMD_Compare: Currently unsupported for " + Platform.simd_instructionSet)
    }
    out << '(' << left << ", " << right
    Platform.simd_instructionSet match {
      case "AVX" | "AVX2" | "AVX512" => // op as parameter
        out << ", _CMP_" << op.toUpperCase << "_" << ord << signal
      case _ =>
    }
    out << ')'
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
  override def datatype = SIMD_MaskDatatype
  override def prettyprint(out : PpStream) : Unit = {
    val prec = if (Knowledge.useDblPrecision) 'd' else 's'
    Platform.simd_instructionSet match {
      case "SSE3"         => out << s"_mm_${op}_p" << prec
      case "AVX" | "AVX2" => out << s"_mm256_${op}_p" << prec
      case "AVX512"       => out << s"_k${op}_mask" << Platform.simd_vectorSize
      // TODO
      case "IMCI"         => new VectorizationException("SIMD_BitwiseOp: Currently unsupported for " + Platform.simd_instructionSet)
      case "QPX"          => new VectorizationException("SIMD_BitwiseOp: Currently unsupported for " + Platform.simd_instructionSet)
    }
    out << '(' << left << ", " << right << ')'
  }
}

case class SIMD_BitwiseAnd(var left : IR_Expression, var right : IR_Expression) extends SIMD_BitwiseOp {
  override def op : String = "and"
  override def prettyprint(out : PpStream) : Unit = {
    if (Platform.simd_instructionSet == "NEON") {
      val prec = if (Knowledge.useDblPrecision) 64 else 32
      out << "vandq_u" << prec << "(" << left << ", " << right << ")"
    } else {
      super.prettyprint(out)
    }
  }
}

case class SIMD_BitwiseOr(var left : IR_Expression, var right : IR_Expression) extends SIMD_BitwiseOp {
  override def op : String = "or"
  override def prettyprint(out : PpStream) : Unit = {
    if (Platform.simd_instructionSet == "NEON") {
      val prec = if (Knowledge.useDblPrecision) 64 else 32
      out << "vorrq_u" << prec << "(" << left << ", " << right << ")"
    } else {
      super.prettyprint(out)
    }
  }
}
