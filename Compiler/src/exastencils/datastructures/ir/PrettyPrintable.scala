package exastencils.datastructures.ir

trait FilePrettyPrintable {
  def printToFile() : Unit
}

trait PrettyPrintable {
  def prettyprint(out : PpStream) : Unit

  final def prettyprint() : String = {
    val out = new PpStream()
    prettyprint(out)
    return out.toString()
  }
}

abstract class RawStream[PP <: PrettyPrintable](sb : StringBuilder) {
  def <<(node : PP) : this.type

  def <<(c : Char) : this.type = {
    sb.append(c)
    return this
  }

  def <<(s : String) : this.type = {
    sb.append(s)
    return this
  }

  def <<(a : Any) : this.type = {
    sb.append(a)
    return this
  }

  def <<<(xs : TraversableOnce[PP]) : this.type = {
    for (x <- xs)
      this << x
    return this
  }

  def <<<(iterable : Iterable[PP], sep : String) : this.type = {
    if (iterable.isEmpty)
      return this

    val it : Iterator[PP] = iterable.iterator
    this << it.next()
    while (it.hasNext)
      this << sep << it.next()
    return this
  }

  def last() : Char = sb.last
  def removeLast() : Unit = sb.deleteCharAt(sb.length - 1)
  def removeLast(i : Int) : Unit = sb.delete(sb.length - i, sb.length)
  override def toString() : String = sb.toString()
}

final class PpStream(sb : StringBuilder = new StringBuilder()) extends RawStream[PrettyPrintable](sb) {
  override def <<(node : PrettyPrintable) : this.type = {
    node.prettyprint(this)
    return this
  }
}

