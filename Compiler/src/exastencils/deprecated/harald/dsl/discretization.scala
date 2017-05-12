package exastencils.deprecated.harald.dsl

import scala.collection.mutable.ListBuffer
import exastencils.deprecated.harald.Impl.ImplFunction
import exastencils.deprecated.harald.Impl.ImplExternalStatement
import exastencils.deprecated.harald.ast.TreeL2
import Array._

class Discretization(treel2 : TreeL2) {

  def generatediscretization() = {

    for (o <- treel2.Stencils)
      if (o.name.equals(DomainKnowledge.operator_L1(0)._1))
        if (!o.weakform(0).equals("")) {

          var s = ""
          var dt = "double"
          var anzentr = 1
          var dim = DomainKnowledge.rule_dim
          var stsize = o.length

          s += "using namespace ::_COLSAMM_;\n"

          if (DomainKnowledge.fragment_L2.get._2.equals("Regular_Cube")) {
            anzentr = 8
            s += s"ELEMENTS::_Cuboid_<Gauss2,${dt}> Cube;\n"
            o.length(0) = 27
          } else {
            anzentr = 4
            s += s"ELEMENTS::_Quadrangle_<Gauss2,${dt}> Quadr;\n"
            o.length(0) = 9
          }

          s += s"std::vector<${dt}> nodedata(${DomainKnowledge.rule_dim()}*${anzentr},0.);\n"
          //      s+= s"std::vector<int> nodenr(${anzentr});\n"
          s += s"std::vector<std::vector<${dt}> > c_all_xx;\n"

          //    s+= s"std::vector<int> addkoeffi(${anzentr});\n"
          //    s+= s"std::vector<int> addkoeffj(${anzentr});\n"
          //    s+= s"std::vector<int> addkoeffk(${anzentr});\n"
          var addkoeff : ListBuffer[ListBuffer[Int]] = ListBuffer()

          if (DomainKnowledge.fragment_L2.get._2.equals("Regular_Cube")) {

            s += s"nodedata[0] = 0;\n"
            s += s"nodedata[1] = 0;\n"
            s += s"nodedata[2] = 0;\n"

            s += s"nodedata[3] = 1;\n"
            s += s"nodedata[4] = 0;\n"
            s += s"nodedata[5] = 0;\n"

            s += s"nodedata[6] = 0;\n"
            s += s"nodedata[7] = 1;\n"
            s += s"nodedata[8] = 0;\n"

            s += s"nodedata[9] = 0;\n"
            s += s"nodedata[10] = 0;\n"
            s += s"nodedata[11] = 1;\n"

            // Transformationsformel fuer 4 Punkte verwenden! Cuboid_Transformation_2
            s += s"Cube(nodedata,4);\n"
            //   Cube(nodedata,8);

            addkoeff += ListBuffer(0, 0, 0)
            addkoeff += ListBuffer(1, 0, 0)
            addkoeff += ListBuffer(1, 1, 0)
            addkoeff += ListBuffer(0, 1, 0)
            addkoeff += ListBuffer(0, 0, 1)
            addkoeff += ListBuffer(1, 0, 1)
            addkoeff += ListBuffer(1, 1, 1)
            addkoeff += ListBuffer(0, 1, 1)

          } else {

            addkoeff += ListBuffer(0, 0)
            addkoeff += ListBuffer(1, 0)
            addkoeff += ListBuffer(1, 1)
            addkoeff += ListBuffer(0, 1)
            /*    s+= s"addkoeffi[0] = 0;\n"
    s+= s"addkoeffj[0] = 0;\n"
    s+= s"addkoeffi[1] = 1;\n"
    s+= s"addkoeffj[1] = 0;\n"
    s+= s"addkoeffi[2] = 1;\n"
    s+= s"addkoeffj[2] = 1;\n"
    s+= s"addkoeffi[3] = 0;\n"
    s+= s"addkoeffj[3] = 1;\n"
*/
            s += s"nodedata[0] = 0; //i+1;\n"
            s += s"nodedata[1] = 0; //j;\n"
            s += s"nodedata[2] = 1; //i+1;\n"
            s += s"nodedata[3] = 0; //j+1;\n"
            s += s"nodedata[4] = 1; //i;\n"
            s += s"nodedata[5] = 1; //j;\n"
            s += s"nodedata[6] = 0; //i;\n"
            s += s"nodedata[7] = 1; //j+1;\n"

            s += s"Quadr(nodedata);\n"

          }

          s += s"for(int i = 0; i < ${o.name}[lev].x1_; i++)\n"
          s += s"for(int j = 0; j < ${o.name}[lev].x2_; j++) {\n"
          if (DomainKnowledge.rule_dim == 3)
            s += s"for(int k = 0; k < ${o.name}[lev].x3_; k++) {\n"

          //c_all_xy = Cube.integrate( Id[clr][IX][l](i,j,k)*Id[clr][IY][l](i,j,k)*v_()*w_());

          /*              s11 = 1; //0.05050; //coeff[SIGMA11][l](i,j,k);
              s12 = 0; //0.35; //coeff[SIGMA12][l](i,j,k);
              s13 = 0; //0.35; //coeff[SIGMA13][l](i,j,k);
              s22 = 1; //0.2575; //coeff[SIGMA22][l](i,j,k);
              s23 = 0; //0.2475; //coeff[SIGMA23][l](i,j,k);
              s33 = 1; //0.2575; //coeff[SIGMA33][l](i,j,k);
*/
          if (DomainKnowledge.fragment_L2.get._2.equals("Regular_Cube")) {
            s += s"c_all_xx = Cube.integrate(${o.weakform(0)});\n"
            //c_all_xx = Cube.integrate( (s11* (d_dx(v_()))*(d_dx(w_()))) + (s12* (d_dx(v_()))*(d_dy(w_()))) + (s12* (d_dy(v_()))*(d_dx(w_()))) + (s22* (d_dy(v_()))*(d_dy(w_()))) + (s13* (d_dx(v_()))*(d_dz(w_()))) + (s13* (d_dz(v_()))*(d_dx(w_()))) + (s23* (d_dy(v_()))*(d_dz(w_()))) + (s23* (d_dz(v_()))*(d_dy(w_()))) + (s33* (d_dz(v_()))*(d_dz(w_()))) + (Id[clr][IX][l](i,j,k)*Id[clr][IX][l](i,j,k)*v_()*w_()) );

            //        c_all_2 = Cube.integrate(s12* (d_dx(v_()))*(d_dy(w_())) + s12* (d_dy(v_()))*(d_dx(w_())) + s13* (d_dx(v_()))*(d_dz(w_())) + s13* (d_dz(v_()))*(d_dx(w_())) + s23* (d_dy(v_()))*(d_dz(w_())) + s23* (d_dz(v_()))*(d_dy(w_())));

            for (i1 <- 0 to anzentr - 1)
              for (j1 <- 0 to anzentr - 1)
                //                 s += s"A->addvs(i+addkoeffi[i1],j+addkoeffj[i1],k+addkoeffk[i1],1+addkoeffi[j1]-addkoeffi[i1],1+addkoeffj[j1]-addkoeffj[i1],1+addkoeffk[j1]-addkoeffk[i1],c_all_xx[i1][j1]);\n"
                s += s"${o.name}[lev].add(i+${addkoeff(i1)(0)},j+${addkoeff(i1)(1)},k+${addkoeff(i1)(2)},${IdxKnowledge.IdxToStencilEntry(dim, stsize(0), ListBuffer(addkoeff(j1)(0) - addkoeff(i1)(0), addkoeff(j1)(1) - addkoeff(i1)(1), addkoeff(j1)(2) - addkoeff(i1)(2)))},c_all_xx[${i1}][${j1}]);\n"

          } else {

            s += s"c_all_xx = Quadr.integrate(${o.weakform(0)});\n"

            for (i1 <- 0 to anzentr - 1)
              for (j1 <- 0 to anzentr - 1)
                //                 s += s"A->addvs(i+addkoeffi[i1],j+addkoeffj[i1],1+addkoeffi[j1]-addkoeffi[i1],1+addkoeffj[j1]-addkoeffj[i1],c_all_xx[i1][j1]);\n"
                s += s"${o.name}[lev].add(i+${addkoeff(i1)(0)},j+${addkoeff(i1)(1)},${IdxKnowledge.IdxToStencilEntry(dim, stsize(0), ListBuffer(addkoeff(j1)(0) - addkoeff(i1)(0), addkoeff(j1)(1) - addkoeff(i1)(1)))},c_all_xx[${i1}][${j1}]);\n"

          }

          s += "}\n"

          if (DomainKnowledge.rule_dim == 3)
            s += "}\n"

          treel2.Functions += "setdiscretizationFE" -> new ImplFunction("setdiscretizationFE", "void", ListBuffer(new ParameterInfo("lev", "int")), ListBuffer(new ImplExternalStatement(s)), Map(), "cpu")
        }

  }

}