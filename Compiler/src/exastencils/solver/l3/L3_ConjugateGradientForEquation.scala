package exastencils.solver.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._

/// L3_ConjugateGradientForEquation

object L3_ConjugateGradientForEquation {
  def generateFor(entries : ListBuffer[L3_SolverForEqEntry], level : Int) = {
    val stmts = ListBuffer[L3_Statement]()

    // FIXME: placeholder

    stmts ++= L3_VankaForEquation.generateFor(entries, level)

//    Residual = RHS - ( Laplace_Stencil * Solution )
//    Var res : Real = NormResidual ( )
//    Var initialRes : Real = res
//    VecP = Residual
//    Var cgSteps : Integer = 0
//
//    repeat 512 times count cgSteps {
//      VecGradP = Laplace_Stencil * VecP
//      Var alphaDenom : Real = VecP * VecGradP
//      Var alpha : Real = res * res / alphaDenom
//      Solution += alpha * VecP
//      Residual -= alpha * VecGradP
//      Var nextRes : Real = NormResidual ( )
//
//      if ( nextRes <= 0.001 * initialRes ) {
//        return
//      }
//
//      Var beta : Real = (nextRes * nextRes) / (res * res)
//      VecP = Residual + beta * VecP
//      res = nextRes
//    }
//    print ( 'Maximum number of cgs iterations (', 512, ') was exceeded' )

    stmts
  }
}