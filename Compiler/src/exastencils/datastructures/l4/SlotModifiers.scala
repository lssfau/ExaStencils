package exastencils.datastructures.l4

import exastencils.datastructures.Node
import exastencils.prettyprinting.PrettyPrintable
import exastencils.prettyprinting.PpStream

sealed trait SlotModifier extends Node with PrettyPrintable {}

object SlotModifier {
  case object Active extends SlotModifier {
    override def prettyprint(out : PpStream) = {
      out << "activeSlot"
    }
  }

  case object Next extends SlotModifier {
    override def prettyprint(out : PpStream) = {
      out << "nextSlot"
    }
  }

  case object Previous extends SlotModifier {
    override def prettyprint(out : PpStream) = {
      out << "previousSlot"
    }
  }

  case class Constant(number : Integer) extends SlotModifier {
    override def prettyprint(out : PpStream) = {
      out << number
    }
  }
}
