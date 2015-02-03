package exastencils.datastructures.l4

import exastencils.datastructures.Node
import exastencils.prettyprinting.PrettyPrintable
import exastencils.prettyprinting.PpStream

sealed trait SlotModifier extends Node with PrettyPrintable {}

object SlotModifier {
  case class Active() extends SlotModifier {
    override def prettyprint(out : PpStream) = {
      out << "activeSlot"
    }
  }

  case class Next() extends SlotModifier {
    override def prettyprint(out : PpStream) = {
      out << "nextSlot"
    }
  }

  case class Previous() extends SlotModifier {
    override def prettyprint(out : PpStream) = {
      out << "previousSlot"
    }
  }

  case class Constant(number : Long) extends SlotModifier {
    override def prettyprint(out : PpStream) = {
      out << number
    }
  }
}
