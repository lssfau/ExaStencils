package harald.dsl

import scala.collection.mutable.ListBuffer
import java.io._
import harald.Impl._
import harald.ast.TreeL2

class TransformL2(treel2 : TreeL2) {

  def setglobalobjects() = {

    for (c <- treel2.Fields)
      if (c.name.contains("solution"))
        if (c.ghostlayers > 0) {
          for (f <- DomainKnowledge.fragments)
            for (i <- 0 to f.edges.length - 1) {
              treel2.GhostFields += new ImplField(s"${c.name}_ghost_edge${i}_send", "", c.datatype, c.veclength, "MyArray", Math.max(c.sizex, c.sizey), 1, 1, c.addpoints, 0)
              treel2.GhostFields += new ImplField(s"${c.name}_ghost_edge${i}_recv", "", c.datatype, c.veclength, "MyArray", Math.max(c.sizex, c.sizey), 1, 1, c.addpoints, 0)
            }
        }

    for (gc <- treel2.GhostFields) {

      var sx : Int = gc.sizex;
      var sy : Int = gc.sizey;
      var sz : Int = gc.sizez;
      // for (i <- 0 to (nlevels_L3.get - 1)) {
      DomainKnowledge.global_ghost_fields += new ImplField(s"${gc.name}[0]", "", gc.datatype, gc.veclength, "MyArray", sx, sy, sz, gc.addpoints, 0)
      //        global_ghost_fields += new TransformL2.FieldImpl(s"${gc.name}[${i}]", gc.datatype, "MyArray",sx, sy, sz, gc.addpoints,0)
      /*
        if (sx > 1)
         sx = sx / 2;
        if (sy > 1)
         sy = sy / 2;
        if (sz > 1)
         sz = sz / 2;
      }
      
      */
    }

    for (c <- treel2.Fields) {

      var sx : Int = c.sizex;
      var sy : Int = c.sizey;
      var sz : Int = c.sizez;
      for (i <- 0 to (DomainKnowledge.nlevels_L2.get - 1)) {
        DomainKnowledge.global_fields += new ImplField(s"${c.name}", s"${i}", c.datatype, c.veclength, "MyArray", sx, sy, sz, c.addpoints, 0)

        //        if (c.name.equals("solution"))
        //          global_fields += new TransformL2.FieldImpl(s"${c.name}_ghost[${i}]", c.datatype, "MyArray",sx, sy, 0, c.addpoints,0)

        sx = sx / 2;
        sy = sy / 2;
        if (DomainKnowledge.rule_dim() == 3) {
          sz = sz / 2;
        }
      }
    }

    for (c <- treel2.Stencils) {
      var sx : Int = c.sizex;
      var sy : Int = c.sizey;
      var sz : Int = c.sizez;

      if (sx != 1) {
        for (i <- 0 to (DomainKnowledge.nlevels_L2.get - 1)) {
          DomainKnowledge.global_stencils += new ImplStencil(s"${c.name}[${i}]", c.matlength1, c.matlength2, c.datatype, sx, sy, sz, c.length, c.entries, c.weakform, c.addpoints)

          sx = sx / 2;
          sy = sy / 2;
          if (DomainKnowledge.rule_dim() == 3)
            sz = sz / 2;
        }
      } else {

        if (c.name.equals("RestrictionStencil")) {

          DomainKnowledge.global_stencils += new ImplStencil(s"${c.name}[0]", c.matlength1, c.matlength2, c.datatype, c.sizex, c.sizey, c.sizez, c.length, c.entries, c.weakform, c.addpoints)

        } else {

          // TODO: assume constant mesh sizes for now!
          var h2 = (1.0 / (DomainKnowledge.xsize_L2.get * DomainKnowledge.xsize_L2.get))

          //println("meshsize " + h2 + "\n")
          for (i <- 0 to (DomainKnowledge.nlevels_L2.get - 1)) {
            var ent : ListBuffer[ListBuffer[Double]] = ListBuffer() //new Array[Array[Double]](c.matlength1*c.matlength2)
            ent += ListBuffer[Double]() //ListBuffer[Double](c.length(0))

            for (e <- 0 to c.entries(0).length - 1)
              ent(0) += (c.entries(0)(e)) * 1.0 / h2 // scala.math.pow(4.0,i) *

            h2 = h2 * 4

            DomainKnowledge.global_stencils += new ImplStencil(s"${c.name}[${i}]", c.matlength1, c.matlength2, c.datatype, c.sizex, c.sizey, c.sizez, c.length, ent, c.weakform, c.addpoints)
          }
        }
      }
    }

  }

}

