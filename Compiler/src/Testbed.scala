import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.prettyprinting._
import exastencils.stencil.ir._
import exastencils.strategies._

object Testbed {
  def test : Unit = {
    //////////////////////////////////////////////////////////////////////////////
    val test = new java.util.IdentityHashMap[Node, Any]()
    new exastencils.datastructures.DefaultStrategy("TestStrategy") {
      this += new exastencils.datastructures.Transformation("test", {
        case n =>
          if (test.containsKey(n))
            println("error: " + n.getClass() + "   " + n)
          test.put(n, null)
          n
      })
    }.apply()
    println("ende...")
    return
    //////////////////////////////////////////////////////////////////////////////
  }

  object ResolveCoordinates0 extends DefaultStrategy("ResolveCoordinates0") {
    var replacement : IR_ExpressionIndex = IR_LoopOverDimensions.defIt(Knowledge.dimensionality) // to be overwritten

    Knowledge.dimensionality match {
      case 1 => this += new Transformation("SearchAndReplace", {
        case IR_StringLiteral("x0") => replacement(0)
      })
      case 2 => this += new Transformation("SearchAndReplace", {
        case IR_StringLiteral("x0") => replacement(0)
        case IR_StringLiteral("y0") => replacement(1)
      })
      case 3 => this += new Transformation("SearchAndReplace", {
        case IR_StringLiteral("x0") => replacement(0)
        case IR_StringLiteral("y0") => replacement(1)
        case IR_StringLiteral("z0") => replacement(2)
      })
    }
  }

  def rap(R : IR_Stencil, A : IR_Stencil, P : IR_Stencil) : IR_Stencil = {

    var RAP = IR_StencilStencilConvolution(A, R).expand.inner.stencil
    //    for (e <- RAP.entries)
    //      println(e.offset.prettyprint + "\t>>\t" + e.weight.prettyprint)

    RAP = IR_StencilStencilConvolution(P, RAP).expand.inner.stencil

    //var RAP : Stencil = StencilStencilConvolution(P, StencilStencilConvolution(A, R).expand).expand

    for (e <- RAP.entries)
      println(e.offset.prettyprint() + "\t>>\t" + e.coefficient.prettyprint)

    /*{
      var entries : ListBuffer[StencilEntry] = ListBuffer()

      for (re <- AP.entries) {
        for (le <- P.entries) {
          var rightOffset = Duplicate(re.offset)
          var leftOffset = Duplicate(le.offset)
          for (d <- 0 until Knowledge.dimensionality)
            leftOffset(d) = dimToString(d) / 2 + leftOffset(d)

          var combOff = leftOffset
          ResolveCoordinates.replacement = rightOffset
          ResolveCoordinates.apply(Some(combOff))

          var combCoeff : Expression = (re.weight * le.weight)
          SimplifyStrategy.doUntilDoneStandalone(combOff)
          SimplifyStrategy.doUntilDoneStandalone(combCoeff)
          var addToEntry = entries.find(e => e.offset match { case o if (combOff == o) => true; case _ => false })
          if (addToEntry.isDefined) {
            combCoeff += addToEntry.get.weight
            SimplifyStrategy.doUntilDoneStandalone(combCoeff)
            addToEntry.get.weight = combCoeff
          } else entries += new StencilEntry(combOff, combCoeff)
        }
      }

      new Stencil(P.identifier + "_" + AP.identifier, P.level, entries)
    }*/

    //    for (e <- RAP.entries) {
    //      ResolveCoordinates0.replacement = new MultiIndex(0, 0, 0, 0)
    //      ResolveCoordinates0.apply(Some(e.offset))
    //    }

    /*var AP : Stencil = StencilStencilConvolution(A, P).expand
    println(AP.printStencilToStr)

    var RAP : Stencil = {

      var entries : ListBuffer[StencilEntry] = ListBuffer()

      for (re <- AP.entries) {
        for (le <- P.entries) {
          var combOff : MultiIndex = re.offset + le.offset
          var combCoeff : Expression = (re.weight * le.weight)

          var valid = true
          for (d <- 0 until Knowledge.dimensionality) {
            if (0 != SimplifyExpression.evalIntegral(combOff(d)) % 2) valid = false
            combOff(d) /= 2
          }

          if (valid) {
            SimplifyStrategy.doUntilDoneStandalone(combOff)
            SimplifyStrategy.doUntilDoneStandalone(combCoeff)
            var addToEntry = entries.find(e => e.offset match { case o if (combOff == o) => true; case _ => false })
            if (addToEntry.isDefined) {
              combCoeff += addToEntry.get.weight
              SimplifyStrategy.doUntilDoneStandalone(combCoeff)
              addToEntry.get.weight = SimplifyExpression.evalFloating(combCoeff)
              //              combCoeff += addToEntry.get.weight
              //              SimplifyStrategy.doUntilDoneStandalone(combCoeff)
              //              addToEntry.get.weight = combCoeff
            } else entries += new StencilEntry(combOff, combCoeff)
          }
        }
      }

      new Stencil(P.identifier + "_" + AP.identifier, P.level, entries)
    }*/
    RAP
  }

  def main(args : Array[String]) : Unit = {
    Knowledge.dimensionality = 2

    var A : IR_Stencil = new IR_Stencil("A", 5,
      if (false) {
        ListBuffer(
          new IR_StencilEntry(IR_ExpressionIndex(0, 0, 0), 3.0),
          new IR_StencilEntry(IR_ExpressionIndex(-1, 0, 0), -0.5),
          new IR_StencilEntry(IR_ExpressionIndex(1, 0, 0), -0.5),
          new IR_StencilEntry(IR_ExpressionIndex(0, -1, 0), -0.5),
          new IR_StencilEntry(IR_ExpressionIndex(0, 1, 0), -0.5),
          new IR_StencilEntry(IR_ExpressionIndex(-1, -1, 0), -0.25),
          new IR_StencilEntry(IR_ExpressionIndex(-1, 1, 0), -0.25),
          new IR_StencilEntry(IR_ExpressionIndex(1, -1, 0), -0.25),
          new IR_StencilEntry(IR_ExpressionIndex(1, 1, 0), -0.25))
      } else if (true) {
        ListBuffer(
          new IR_StencilEntry(IR_ExpressionIndex(0, 0, 0), 4.0),
          new IR_StencilEntry(IR_ExpressionIndex(-1, 0, 0), -1.0),
          new IR_StencilEntry(IR_ExpressionIndex(1, 0, 0), -1.0),
          new IR_StencilEntry(IR_ExpressionIndex(0, -1, 0), -1.0),
          new IR_StencilEntry(IR_ExpressionIndex(0, 1, 0), -1.0))
      } else {
        ListBuffer(
          new IR_StencilEntry(IR_ExpressionIndex(0, 0, 0), "C"),
          new IR_StencilEntry(IR_ExpressionIndex(-1, 0, 0), "W"),
          new IR_StencilEntry(IR_ExpressionIndex(1, 0, 0), "E"),
          new IR_StencilEntry(IR_ExpressionIndex(0, -1, 0), "S"),
          new IR_StencilEntry(IR_ExpressionIndex(0, 1, 0), "N"))
      })

    var R : IR_Stencil = new IR_Stencil("R", 4, ListBuffer(
      new IR_StencilEntry(IR_ExpressionIndex(0, 0, 0), 1.0),
      new IR_StencilEntry(IR_ExpressionIndex(-1, 0, 0), 0.5),
      new IR_StencilEntry(IR_ExpressionIndex(1, 0, 0), 0.5),
      new IR_StencilEntry(IR_ExpressionIndex(0, -1, 0), 0.5),
      new IR_StencilEntry(IR_ExpressionIndex(0, 1, 0), 0.5),
      new IR_StencilEntry(IR_ExpressionIndex(-1, -1, 0), 0.25),
      new IR_StencilEntry(IR_ExpressionIndex(-1, 1, 0), 0.25),
      new IR_StencilEntry(IR_ExpressionIndex(1, -1, 0), 0.25),
      new IR_StencilEntry(IR_ExpressionIndex(1, 1, 0), 0.25)))

    var P : IR_Stencil = new IR_Stencil("P", 4, ListBuffer(
      new IR_StencilEntry(IR_ExpressionIndex(0, 0, 0), 0.25),
      new IR_StencilEntry(IR_ExpressionIndex("x" Mod 2, 0, 0), 0.25),
      new IR_StencilEntry(IR_ExpressionIndex(0, "y" Mod 2, 0), 0.25),
      new IR_StencilEntry(IR_ExpressionIndex("x" Mod 2, "y" Mod 2, 0), 0.25)))

    println(R.printStencilToStr)
    println(A.printStencilToStr)
    //P.printStencil

    var RAP = A

    for (i <- 0 until 1) {
      RAP.level = R.level + 1
      RAP = rap(R, RAP, P)
      println(RAP.printStencilToStr)
    }

    return

    /*  val index = new MultiIndex(1, 2, 3)
  val aabb = new IndexRange(new MultiIndex(0, 0, 0), new MultiIndex(33, 33, 33))

  val node : Statement = Knowledge.dimensionality match {
    case 1 => (index(0))
    case 2 => (index(1) * (aabb.end(0) - aabb.begin(0)) + index(0))
    case 3 => (index(2) * ((aabb.end(1) - aabb.begin(1)) * (aabb.end(0) - aabb.begin(0))) + index(1) * (aabb.end(0) - aabb.begin(0)) + index(0))
  }

  println(node)
  println(node.prettyprint)

  do { SimplifyStrategy.apply(Some(node), StateManager.History.currentToken) }
  while (SimplifyStrategy.results.last._2.replacements > 0) // FIXME: cleaner code

  println(node)
  println(node.prettyprint)
*/

    var statements = new ListBuffer[IR_Statement]

    statements +=
      """void tet_gs_coeff_1c(double* u, double* f, double *koe, double* stiff, int *p_tsize) {

   int tsize = p_tsize[0];
   int mp, tp, bp;
   int mp_mr, mp_tr, mp_br;
   int tp_mr, tp_br;
   int bp_mr, bp_tr;
   bp = 0;
   mp = PLAINSIZE(tsize);
   tp = mp + PLAINSIZE(tsize - 1);
   //HS const int stiffsize = 1;
   //HS  double** c = new double*[stiffsize]; //stiff.size
   double* c = new double[6 * 4 * 4];
   double* stencil = new double[16];
   double k_tw_tc, k_tc_tse, k_tc_mn, k_ts_mc, k_tse_mse, k_tw_mw, k_mnw_mc, k_mc_me, k_ms_mse, k_mw_ms;
   double k_mc_bc, k_mn_bn, k_mse_be, k_mw_bnw, k_bnw_bn, k_bn_be;
   double k_el;
   //HS
   for (int i = 0; i < 6 * 4 * 4; ++i) {
      c[i] = 0.25 * stiff[i];
   }
   for (int k = 1; k < (tsize - 3); ++k) {
      bp_mr = bp + tsize - k + 1;
      bp_tr = bp + 2 * (tsize - k + 1) - 1;
      mp_br = mp;
      mp_mr = mp + tsize - k;
      mp_tr = mp + 2 * (tsize - k) - 1;
      tp_br = tp;
      tp_mr = tp + tsize - k - 1;
      for (int j = 1; j < (tsize - k - 2); ++j) {
#pragma simd
         for (int i = 1; i < (tsize - j - k - 1); i = i + 1) {
            // 16 FLOPs
            k_tw_tc   = koe[tp_mr + i - 1] + koe[tp_mr + i];
            k_tc_tse  = koe[tp_mr + i]     + koe[tp_br + i + 1];
            k_tc_mn   = koe[tp_mr + i]     + koe[mp_tr + i];
            k_ts_mc   = koe[tp_br + i]     + koe[mp_mr + i];
            k_tse_mse = koe[tp_br + i + 1] + koe[mp_br + i + 1];
            k_tw_mw   = koe[tp_mr + i - 1] + koe[mp_mr + i - 1];
            k_mnw_mc  = koe[mp_tr + i - 1] + koe[mp_mr + i];
            k_mc_me   = koe[mp_mr + i]     + koe[mp_mr + i + 1];
            k_ms_mse  = koe[mp_br + i]     + koe[mp_br + i + 1];
            k_mw_ms   = koe[mp_mr + i - 1] + koe[mp_br + i];
            k_mc_bc   = koe[mp_mr + i]     + koe[bp_mr + i];
            k_mn_bn   = koe[mp_tr + i]     + koe[bp_tr + i];
            k_mse_be  = koe[mp_br + i + 1] + koe[bp_mr + i + 1];
            k_mw_bnw  = koe[mp_mr + i - 1] + koe[bp_tr + i - 1];
            k_bnw_bn  = koe[bp_tr + i - 1] + koe[bp_tr + i];
            k_bn_be   = koe[bp_tr + i]     + koe[bp_mr + i + 1];
            // 196-15 = 181 Flops
      """

    var aabb = IR_ExpressionIndexRange(IR_ExpressionIndex(0, 0, 0), IR_ExpressionIndex(4, 4, 4))
    var k : IR_Expression = "k_el"
    for (group <- 0 to 5) {
      for (position <- 0 to 3) {
        statements += IR_Assignment(k, "k_tc_mn + k_mc_me")
        for (i <- 0 to 4) {
          statements += IR_Assignment(IR_ArrayAccess("stencil", "tet_mc"), k * IR_ArrayAccess("c", aabb.linearizeIndex(IR_ExpressionIndex(i, position, group))), "+=")
        }
      }
    }

    /*
            /////////////////////////////////////////
            // group zero element
            /////////////////////////////////////////
            k_el = k_tc_mn + k_mc_me;
            // position zero
            stencil[tet_mc] = k_el * c[0 * 4 * 4 + 0 * 4 + 0];
            stencil[tet_me] = k_el * c[0 * 4 * 4 + 0 * 4 + 1];
            stencil[tet_mn] = k_el * c[0 * 4 * 4 + 0 * 4 + 2];
            stencil[tet_tc] = k_el * c[0 * 4 * 4 + 0 * 4 + 3];
            k_el = k_mnw_mc + k_tw_mw;
            // position one
            stencil[tet_mw] = k_el * c[0 * 4 * 4 + 1 * 4 + 0];
            stencil[tet_mc] += k_el * c[0 * 4 * 4 + 1 * 4 + 1];
            stencil[tet_mnw] = k_el * c[0 * 4 * 4 + 1 * 4 + 2];
            stencil[tet_tw] = k_el * c[0 * 4 * 4 + 1 * 4 + 3];
            k_el = k_ts_mc + k_ms_mse;
            // position two
            stencil[tet_ms] = k_el * c[0 * 4 * 4 + 2 * 4 + 0];
            stencil[tet_mse] = k_el * c[0 * 4 * 4 + 2 * 4 + 1];
            stencil[tet_mc] += k_el * c[0 * 4 * 4 + 2 * 4 + 2];
            stencil[tet_ts] = k_el * c[0 * 4 * 4 + 2 * 4 + 3];
            k_el = k_mc_bc + k_bn_be;
            // position three
            stencil[tet_bc] = k_el * c[0 * 4 * 4 + 3 * 4 + 0];
            stencil[tet_be] = k_el * c[0 * 4 * 4 + 3 * 4 + 1];
            stencil[tet_bn] = k_el * c[0 * 4 * 4 + 3 * 4 + 2];
            stencil[tet_mc] += k_el * c[0 * 4 * 4 + 3 * 4 + 3];
            /////////////////////////////////////////
            // group one element
            /////////////////////////////////////////
            k_el = k_tw_tc + k_ts_mc;
            // position zero
            stencil[tet_mc] += k_el * c[1 * 4 * 4 + 0 * 4 + 0];
            stencil[tet_ts] += k_el * c[1 * 4 * 4 + 0 * 4 + 1];
            stencil[tet_tw] += k_el * c[1 * 4 * 4 + 0 * 4 + 2];
            stencil[tet_tc] += k_el * c[1 * 4 * 4 + 0 * 4 + 3];
            k_el = k_mnw_mc + k_mn_bn;
            // position one
            stencil[tet_bn] += k_el * c[1 * 4 * 4 + 1 * 4 + 0];
            stencil[tet_mc] += k_el * c[1 * 4 * 4 + 1 * 4 + 1];
            stencil[tet_mnw] += k_el * c[1 * 4 * 4 + 1 * 4 + 2];
            stencil[tet_mn] += k_el * c[1 * 4 * 4 + 1 * 4 + 3];
            k_el = k_mc_me + k_mse_be;
            // position two
            stencil[tet_be] += k_el * c[1 * 4 * 4 + 2 * 4 + 0];
            stencil[tet_mse] += k_el * c[1 * 4 * 4 + 2 * 4 + 1];
            stencil[tet_mc] += k_el * c[1 * 4 * 4 + 2 * 4 + 2];
            stencil[tet_me] += k_el * c[1 * 4 * 4 + 2 * 4 + 3];
            k_el = k_mw_ms + k_mc_bc;
            // position three
            stencil[tet_bc] += k_el * c[1 * 4 * 4 + 3 * 4 + 0];
            stencil[tet_ms] += k_el * c[1 * 4 * 4 + 3 * 4 + 1];
            stencil[tet_mw] += k_el * c[1 * 4 * 4 + 3 * 4 + 2];
            stencil[tet_mc] += k_el * c[1 * 4 * 4 + 3 * 4 + 3];
            /////////////////////////////////////////
            // group two element
            /////////////////////////////////////////
            k_el = k_tw_tc + k_mnw_mc;
            // position zero
            stencil[tet_mc] += k_el * c[2 * 4 * 4 + 0 * 4 + 0];
            stencil[tet_mnw] += k_el * c[2 * 4 * 4 + 0 * 4 + 1];
            stencil[tet_tw] += k_el * c[2 * 4 * 4 + 0 * 4 + 2];
            stencil[tet_tc] += k_el * c[2 * 4 * 4 + 0 * 4 + 3];
            k_el = k_ts_mc + k_tse_mse;
            // position one
            stencil[tet_mse] += k_el * c[2 * 4 * 4 + 1 * 4 + 0];
            stencil[tet_mc] += k_el * c[2 * 4 * 4 + 1 * 4 + 1];
            stencil[tet_ts] += k_el * c[2 * 4 * 4 + 1 * 4 + 2];
            stencil[tet_tse] = k_el * c[2 * 4 * 4 + 1 * 4 + 3];
            k_el = k_mc_me + k_bn_be;
            // position two
            stencil[tet_be] += k_el * c[2 * 4 * 4 + 2 * 4 + 0];
            stencil[tet_bn] += k_el * c[2 * 4 * 4 + 2 * 4 + 1];
            stencil[tet_mc] += k_el * c[2 * 4 * 4 + 2 * 4 + 2];
            stencil[tet_me] += k_el * c[2 * 4 * 4 + 2 * 4 + 3];
            k_el = k_mc_bc + k_mw_bnw;
            // position three
            stencil[tet_bc] += k_el * c[2 * 4 * 4 + 3 * 4 + 0];
            stencil[tet_bnw] = k_el * c[2 * 4 * 4 + 3 * 4 + 1];
            stencil[tet_mw] += k_el * c[2 * 4 * 4 + 3 * 4 + 2];
            stencil[tet_mc] += k_el * c[2 * 4 * 4 + 3 * 4 + 3];
            /////////////////////////////////////////
            // group three element
            /////////////////////////////////////////
            k_el = k_tc_tse + k_mc_me;
            // position zero
            stencil[tet_mc] += k_el * c[3 * 4 * 4 + 0 * 4 + 0];
            stencil[tet_me] += k_el * c[3 * 4 * 4 + 0 * 4 + 1];
            stencil[tet_tse] += k_el * c[3 * 4 * 4 + 0 * 4 + 2];
            stencil[tet_tc] += k_el * c[3 * 4 * 4 + 0 * 4 + 3];
            k_el = k_tw_mw + k_ts_mc;
            // position one
            stencil[tet_mw] += k_el * c[3 * 4 * 4 + 1 * 4 + 0];
            stencil[tet_mc] += k_el * c[3 * 4 * 4 + 1 * 4 + 1];
            stencil[tet_ts] += k_el * c[3 * 4 * 4 + 1 * 4 + 2];
            stencil[tet_tw] += k_el * c[3 * 4 * 4 + 1 * 4 + 3];
            k_el = k_mnw_mc + k_bnw_bn;
            // position two
            stencil[tet_bnw] += k_el * c[3 * 4 * 4 + 2 * 4 + 0];
            stencil[tet_bn] += k_el * c[3 * 4 * 4 + 2 * 4 + 1];
            stencil[tet_mc] += k_el * c[3 * 4 * 4 + 2 * 4 + 2];
            stencil[tet_mnw] += k_el * c[3 * 4 * 4 + 2 * 4 + 3];
            k_el = k_mse_be + k_mc_bc;
            // position three
            stencil[tet_bc] += k_el * c[3 * 4 * 4 + 3 * 4 + 0];
            stencil[tet_be] += k_el * c[3 * 4 * 4 + 3 * 4 + 1];
            stencil[tet_mse] += k_el * c[3 * 4 * 4 + 3 * 4 + 2];
            stencil[tet_mc] += k_el * c[3 * 4 * 4 + 3 * 4 + 3];
            /////////////////////////////////////////
            // group four element
            /////////////////////////////////////////
            k_el = k_tc_tse + k_ts_mc;
            // position zero
            stencil[tet_mc] += k_el * c[4 * 4 * 4 + 0 * 4 + 0];
            stencil[tet_ts] += k_el * c[4 * 4 * 4 + 0 * 4 + 1];
            stencil[tet_tse] += k_el * c[4 * 4 * 4 + 0 * 4 + 2];
            stencil[tet_tc] += k_el * c[4 * 4 * 4 + 0 * 4 + 3];
            k_el = k_mc_me + k_mn_bn;
            // position one
            stencil[tet_bn] += k_el * c[4 * 4 * 4 + 1 * 4 + 0];
            stencil[tet_mc] += k_el * c[4 * 4 * 4 + 1 * 4 + 1];
            stencil[tet_me] += k_el * c[4 * 4 * 4 + 1 * 4 + 2];
            stencil[tet_mn] += k_el * c[4 * 4 * 4 + 1 * 4 + 3];
            k_el = k_mnw_mc + k_mw_bnw;
            // position two
            stencil[tet_bnw] += k_el * c[4 * 4 * 4 + 2 * 4 + 0];
            stencil[tet_mw] += k_el * c[4 * 4 * 4 + 2 * 4 + 1];
            stencil[tet_mc] += k_el * c[4 * 4 * 4 + 2 * 4 + 2];
            stencil[tet_mnw] += k_el * c[4 * 4 * 4 + 2 * 4 + 3];
            k_el = k_ms_mse + k_mc_bc;
            // position three
            stencil[tet_bc] += k_el * c[4 * 4 * 4 + 3 * 4 + 0];
            stencil[tet_ms] += k_el * c[4 * 4 * 4 + 3 * 4 + 1];
            stencil[tet_mse] += k_el * c[4 * 4 * 4 + 3 * 4 + 2];
            stencil[tet_mc] += k_el * c[4 * 4 * 4 + 3 * 4 + 3];
            /////////////////////////////////////////
            // group five element
            /////////////////////////////////////////
            k_el = k_tc_mn + k_mnw_mc;
            // position zero
            stencil[tet_mc] += k_el * c[5 * 4 * 4 + 0 * 4 + 0];
            stencil[tet_mnw] += k_el * c[5 * 4 * 4 + 0 * 4 + 1];
            stencil[tet_mn] += k_el * c[5 * 4 * 4 + 0 * 4 + 2];
            stencil[tet_tc] += k_el * c[5 * 4 * 4 + 0 * 4 + 3];
            k_el = k_tse_mse + k_mc_me;
            // position one
            stencil[tet_mse] += k_el * c[5 * 4 * 4 + 1 * 4 + 0];
            stencil[tet_mc] += k_el * c[5 * 4 * 4 + 1 * 4 + 1];
            stencil[tet_me] += k_el * c[5 * 4 * 4 + 1 * 4 + 2];
            stencil[tet_tse] += k_el * c[5 * 4 * 4 + 1 * 4 + 3];
            k_el = k_ts_mc + k_mw_ms;
            // position two
            stencil[tet_ms] += k_el * c[5 * 4 * 4 + 2 * 4 + 0];
            stencil[tet_mw] += k_el * c[5 * 4 * 4 + 2 * 4 + 1];
            stencil[tet_mc] += k_el * c[5 * 4 * 4 + 2 * 4 + 2];
            stencil[tet_ts] += k_el * c[5 * 4 * 4 + 2 * 4 + 3];
            k_el = k_mc_bc + k_bnw_bn;
            // position three
            stencil[tet_bc] += k_el * c[5 * 4 * 4 + 3 * 4 + 0];
            stencil[tet_bnw] += k_el * c[5 * 4 * 4 + 3 * 4 + 1];
            stencil[tet_bn] += k_el * c[5 * 4 * 4 + 3 * 4 + 2];
            stencil[tet_mc] += k_el * c[5 * 4 * 4 + 3 * 4 + 3];
*/
    statements +=
      """// compute center weight quotient
            stencil[tet_mcq] = 1.0 / stencil[tet_mc];
            u[mp_mr + i] =   stencil[0]  *(f[mp_mr + i] -
                  stencil[1]  * u[mp_mr + i + 1] -
                  stencil[2]  * u[mp_tr + i - 1] -
                  stencil[3]  * u[mp_tr + i] -
                  stencil[4]  * u[tp_br + i] -
                  stencil[5]  * u[tp_br + i + 1] -
                  stencil[6]  * u[tp_mr + i - 1] -
                  stencil[7]  * u[tp_mr + i] -
                  stencil[8]  * u[bp_mr + i] -
                  stencil[9]  * u[bp_mr + i + 1] -
                  stencil[10] * u[bp_tr + i - 1] -
                  stencil[11] * u[bp_tr + i] -
                  stencil[12] * u[mp_br + i] -
                  stencil[13] * u[mp_br + i + 1] -
                  stencil[14] * u[mp_mr + i - 1]);
         }  // i
         bp_mr = bp_tr;
         bp_tr = bp_tr + tsize - j - k;
         mp_br = mp_mr;
         mp_mr = mp_tr;
         mp_tr = mp_tr + tsize - j - k - 1;
         tp_br = tp_mr;
         tp_mr = tp_mr + tsize - j - k - 1;
      }  // j
      bp = mp;
      mp = tp;
      tp = tp + PLAINSIZE(tsize-k-1);
   }  // k
   delete[] stencil;
   stencil=0;
   delete[] c;
   c=0;
}  // tet_gs_coeff
      """

    var root = IR_Scope(statements)

    StateManager.root_ = root

    SimplifyStrategy.doUntilDone()

    Settings.outputPath = "Heap/"
    var printer = PrettyprintingManager.getPrinter("tet_gs_coeff_gen.cc")
    printer << root.prettyprint
    PrettyprintingManager.finish
  }
}
