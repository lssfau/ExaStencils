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

import exastencils.base.ir.IR_FunctionLike
import exastencils.prettyprinting.PpStream

/// SIMD_NeonDivision

case object SIMD_NeonDivision extends IR_FunctionLike {
  override var name = "vdivq_f32"
  isHeaderOnly = true

  override def prettyprint_decl() : String = "\n --- NOT VALID ; no prototype for vdivq_f32\n"

  override def prettyprint(out : PpStream) : Unit = {
    out <<
      s"""static inline float32x4_t ${ name }(const float32x4_t &a, const float32x4_t &b) {
  // get an initial estimate of 1/b.
  float32x4_t reciprocal = vrecpeq_f32(b);

  // use a couple Newton-Raphson steps to refine the estimate.  Depending on your
  // application's accuracy requirements, you may be able to get away with only
  // one refinement (instead of the two used here).  Be sure to test!
  reciprocal = vmulq_f32(vrecpsq_f32(b, reciprocal), reciprocal);
  reciprocal = vmulq_f32(vrecpsq_f32(b, reciprocal), reciprocal);

  // and finally, compute a/b = a*(1/b)
  return vmulq_f32(a,reciprocal);
}"""
  }
}
