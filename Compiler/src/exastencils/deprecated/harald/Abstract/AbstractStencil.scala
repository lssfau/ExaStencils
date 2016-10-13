package exastencils.deprecated.harald.Abstract

import scala.collection.mutable.ListBuffer

import exastencils.deprecated.harald.Continuous._
import exastencils.deprecated.harald.Impl._
import exastencils.deprecated.harald.dsl._

class AbstractStencil(val name : String, val dim : Int, val datatype : String, val matlength1 : String, val matlength2 : String, val distype : String, val order : String, val location : String) {

  override def toString = "ExaOp: '" + name + "', type = '" + datatype + "', disc_type = '" + distype + "', order = '" + order + "'" + location

  def transform : ListBuffer[ImplStencil] = {

    var stencilbuf : ListBuffer[ImplStencil] = ListBuffer()
    //var entries: ListBuffer[Double] = ListBuffer[Double]()

    var wf : Array[String] = new Array[String](matlength1.toInt * matlength2.toInt)
    for (i <- 0 to matlength1.toInt * matlength2.toInt - 1)
      wf(i) = ""
    var ent : ListBuffer[ListBuffer[Double]] = ListBuffer() //new ListBuffer(matlength1.toInt*matlength2.toInt)

    var stlength : Array[Int] = new Array(matlength1.toInt * matlength2.toInt)
    var storestencil = 0
    //var weakform = ""

    var stencilval : Array[Array[Array[Double]]] = Array()

    for (o <- DomainKnowledge.discr_operators) {
      if ((o.name.equals(DomainKnowledge.operator_L1(0)._1)) && (name.equals(o.name))) {
        println("now working on: " + o)
        for (e <- DomainKnowledge.cont_equations) {
          e.lhs match {
            case ContVariable(cobj, _) => {
              if (cobj.name.equals(DomainKnowledge.operator_L1(0)._1)) {
                println(e.rhs)
                println(e.rhs.ToStringClass)
                val res = e.rhs.discretize(distype, order.toInt)
                println(res)
                println(res.value)
                println(res.ToStringClass)
                stencilval = res.value
              }
            }
            case _                     =>
          }
        }
      }
    }



    for (i <- 0 to stencilval.size - 1) {
      for (j <- 0 to stencilval(0).size - 1) {
        for (k <- 0 to stencilval(0)(0).size - 1)
          print(" " + stencilval(i)(j)(k))
        print("\n")
      }
      print("\n")
    }

    var stencilsize = stencilval.length * stencilval.length
    if (DomainKnowledge.rule_dim == 3)
      stencilsize *= stencilval.length
    println(stencilsize)
    if (stencilsize <= 0)
      stencilsize = 1

    val stencilidx = IdxKnowledge.StencilToidx(DomainKnowledge.rule_dim, stencilsize)

//      println(stencilval(1)(1+stencilidx(0)(0))(1+stencilidx(0)(1)) + " " + stencilval(1)(1+stencilidx(1)(0))(1+stencilidx(1)(1)))

    // if current Operator is the one defined on level L1
    if (name.equals(DomainKnowledge.operator_L1(0)._1)) {
      var strop : ListBuffer[String] = ListBuffer()
      for (i <- 0 to DomainKnowledge.operator_L1.length - 1)
        strop ++= DomainKnowledge.operator_L1(i)._2

      val stkn = new DomainKnowledge.StencilKnowledge(DomainKnowledge.domain_L1.get._2, distype, order, location, strop)


      ent =
        stkn match {

//          case DomainKnowledge.StencilKnowledge("UnitSquare", "FD", "2",loc, ListBuffer("Laplacian")) => ListBuffer[ListBuffer[Double]](ListBuffer[Double](4.0, -1.0, -1.0, -1.0, -1.0))
          case DomainKnowledge.StencilKnowledge("UnitSquare", "FD", "2", loc, ListBuffer("Laplacian")) => {
            var li : ListBuffer[Double] = ListBuffer()
            for (i <- 0 to stencilsize - 1)
              li += stencilval(1)(1 + stencilidx(i)(0))(1 + stencilidx(i)(1))

            ListBuffer[ListBuffer[Double]](li)
//            ListBuffer[ListBuffer[Double]](ListBuffer[Double](stencilval(1)(1+stencilidx(0)(0))(1+stencilidx(0)(1)), stencilval(1)(1+stencilidx(1)(0))(1+stencilidx(1)(1)), stencilval(1)(1+stencilidx(2)(0))(1+stencilidx(2)(1)), stencilval(1)(1+stencilidx(3)(0))(1+stencilidx(3)(1)), stencilval(1)(1+stencilidx(4)(0))(1+stencilidx(4)(1))))

          }

          case DomainKnowledge.StencilKnowledge("UnitSquare", "FD", "2", loc, ListBuffer("VecLaplacian"))         => ListBuffer[ListBuffer[Double]](ListBuffer[Double](4.0, -1.0, -1.0, -1.0, -1.0),
            ListBuffer[Double](0.0),
            ListBuffer[Double](0.0),
            ListBuffer[Double](4.0, -1.0, -1.0, -1.0, -1.0))
          case DomainKnowledge.StencilKnowledge("UnitSquare", "FE", "2", loc, ListBuffer("Laplacian"))            =>
            stlength(0) = 9; storestencil = 1; DomainKnowledge.use_FE = true; wf(0) = "grad(v_())*grad(w_())"; ListBuffer[ListBuffer[Double]](ListBuffer[Double]())
          case DomainKnowledge.StencilKnowledge("UnitCube", "FD", "2", loc, ListBuffer("Laplacian"))              => ListBuffer[ListBuffer[Double]](ListBuffer[Double](6.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0))
          case DomainKnowledge.StencilKnowledge("UnitCube", "FE", "2", loc, ListBuffer("Laplacian"))              =>
            stlength(0) = 27; storestencil = 1; DomainKnowledge.use_FE = true; wf(0) = "grad(v_())*grad(w_())"; ListBuffer[ListBuffer[Double]](ListBuffer[Double]())
          case DomainKnowledge.StencilKnowledge("UnitSquare", "FD", "2", "cells", ListBuffer("ComplexDiffusion")) => ListBuffer[ListBuffer[Double]](ListBuffer[Double](4.0, -1.0, -1.0, -1.0, -1.0))
          case DomainKnowledge.StencilKnowledge("UnitCube", "FD", "2", "cells", ListBuffer("ComplexDiffusion"))   => ListBuffer[ListBuffer[Double]](ListBuffer[Double](6.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0))
          case _                                                                                                  => ListBuffer[ListBuffer[Double]](ListBuffer[Double]())
        }
    }

    if (distype.equals("TOCOARSE")) {

      val stkn = new DomainKnowledge.IntStencilKnowledge(dim, order, location)

      ent =
        stkn match {

          case DomainKnowledge.IntStencilKnowledge(2, "2", "nodes") => ListBuffer[ListBuffer[Double]](ListBuffer[Double](1.0, 0.5, 0.5, 0.5, 0.5, 0.25, 0.25, 0.25, 0.25))
          case DomainKnowledge.IntStencilKnowledge(_, "1", "nodes") => ListBuffer[ListBuffer[Double]](ListBuffer[Double](1.0))
          case DomainKnowledge.IntStencilKnowledge(3, "2", "nodes") => ListBuffer[ListBuffer[Double]](ListBuffer[Double](1.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
            0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
            0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125))
          case DomainKnowledge.IntStencilKnowledge(2, "2", "cells") => ListBuffer[ListBuffer[Double]](ListBuffer[Double](0.25, 0.25, 0.25, 0.25))
          case DomainKnowledge.IntStencilKnowledge(3, "2", "cells") => ListBuffer[ListBuffer[Double]](ListBuffer[Double](0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125))
          case _                                                    => ListBuffer[ListBuffer[Double]](ListBuffer[Double]())
        }
    }


    var xs = 1
    var ys = 1
    var zs = 1

    if (storestencil == 1) {
      xs = DomainKnowledge.xsize_L2.getOrElse(1)
      ys = DomainKnowledge.ysize_L2.getOrElse(1)
      zs = DomainKnowledge.zsize_L2.getOrElse(1)
    }

    var dt : ListBuffer[String] = ListBuffer() //new ListBuffer(matlength1.toInt*matlength2.toInt)

    for (i <- 0 to matlength1.toInt * matlength2.toInt - 1) {
      dt += DomainKnowledge.transform_datatype_cpp(datatype)

      if (ent(i).length != 0)
        stlength(i) = ent(i).length

      //wf(i) = weakform
      // ent(i) = entries

      //   stencilbuf += new ImplStencil(name+s"_${i}", s"${i}", DomainKnowledge.transform_datatype_cpp(datatype), xs, ys, zs, stlength, entries, weakform, DomainKnowledge.rule_addpoints(location))
    }

    println(ent)

    stencilbuf += new ImplStencil(name, matlength1.toInt, matlength2.toInt, dt, xs, ys, zs, stlength, ent, wf, DomainKnowledge.rule_addpoints(location))

    return stencilbuf
  }
}
