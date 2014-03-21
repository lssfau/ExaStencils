package harald.dsl

import scala.collection.mutable.ListBuffer
import java.io._
import harald.Impl._
import harald.ast.TreeL2
import exastencils.knowledge._

class TransformL2(treel2 : TreeL2) {

    def setglobalobjects() {

    for (c <- treel2.Stencils) {
      var sx: Int = c.sizex;
      var sy: Int = c.sizey;
      var sz: Int = c.sizez;

      if (sx != 1) {
        for (i <- 0 to Knowledge.maxLevel) {
          DomainKnowledge.global_stencils += new ImplStencil(s"${c.name}[${i}]", c.part, c.datatype, sx, sy, sz, c.length, c.entries, c.weakform, c.addpoints)

          sx = sx / 2;
          sy = sy / 2;
          if (DomainKnowledge.rule_dim() == 3)
            sz = sz / 2;
        }
      } else
        DomainKnowledge.global_stencils += new ImplStencil(s"${c.name}[0]", c.part, c.datatype, c.sizex, c.sizey, c.sizez, c.length, c.entries, c.weakform, c.addpoints)
    }

  }

}

