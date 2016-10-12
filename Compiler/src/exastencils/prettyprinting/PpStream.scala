package exastencils.prettyprinting

import exastencils.prettyprinting.PrintEnvironment.PrintEnvironment

/// PpStream

final class PpStream(val env : PrintEnvironment) extends RawStream[PrettyPrintable]() {
  override def <<(node : PrettyPrintable) : this.type = {
    node.prettyprint(this)
    this
  }
}

/// RawStream

abstract class RawStream[PP <: PrettyPrintable]() {
  val sb : StringBuilder = new StringBuilder()

  def <<(node : PP) : this.type

  def <<(c : Char) : this.type = {
    sb.append(c)
    this
  }

  def <<(s : String) : this.type = {
    sb.append(s)
    this
  }

  def <<(a : Any) : this.type = {
    sb.append(a)
    this
  }

  def <<<(xs : TraversableOnce[PP]) : this.type = {
    for (x <- xs)
      this << x
    this
  }

  def <<<(iterable : Iterable[PP], sep : String) : this.type = {
    if (iterable.isEmpty)
      return this

    val it : Iterator[PP] = iterable.iterator
    this << it.next()
    while (it.hasNext)
      this << sep << it.next()
    this
  }

  def last() : Char = sb.last
  def removeLast() : Unit = sb.deleteCharAt(sb.length - 1)
  def removeLast(i : Int) : Unit = sb.delete(sb.length - i, sb.length)
  override def toString : String = sb.toString()
}
