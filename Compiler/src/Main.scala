import exastencils.datastructures._
import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.parsers.l4
import exastencils.parsers.l4.ParserL4

object Main {
  object CountingStrategy extends Strategy {
    override def apply : Boolean = {
      var constcount = 0
      var forcount = 0

      StateManager.apply(new Transformation({ case x : Constant => constcount += 1; WARN(x); Some(x) }))
      StateManager.apply(new Transformation({ case a : ForStatement => forcount += 1; Some(a) }))

      WARN(f"Counted $constcount consts and $forcount fors")

      true
    }
  }

  def main(args : Array[String]) : Unit = {
    //    val newt = Duplicate(TreeManager.root)
    //    // annotations copied?
    //    println("original annotations")
    //    TreeManager.root.getAnnotations.foreach(a => println(a.name + " <-> " + a.value))
    //    println("cloned annotations")
    //    newt.getAnnotations.foreach(a => println(a.name + " <-> " + a.value))

    //    DBG("previous:")
    //    DBG(StateManager.root)
    //    
    //    var replacingStrategy = new Strategy
    //    replacingStrategy += new Transformation({case x : ConstantExpression if(x.Value == 1) => Some(ConstantExpression(3)) })
    //    replacingStrategy += new Transformation({case x : ConstantExpression if(x.Value.isInstanceOf[Int]) => Some(ConstantExpression(x.Value.asInstanceOf[Int] * 10)) })
    //    replacingStrategy.apply
    //    var absvarStrategy = new Strategy
    //    absvarStrategy += new Transformation({ case x : Variable => Some(AbstractVariable("j", x.Type)) })
    //    absvarStrategy.apply
    //
    //    DBG("===========================")
    //    DBG("after:")
    //    DBG(StateManager.root)
    //
    //    StateManager.apply(CountingStrategy)

//    val source = Source.fromFile("data/log-short")
//	val reader = new BufferingReader(source.getLines)
//	val result = parsers.commit.*(reader)
//	result.get.map(Console println _)
    
    class BufferingReader(iterator: Iterator[String], val position: Int) extends scala.util.parsing.input.Reader[String] {
	def this(iterator: Iterator[String]) = this(iterator, 0)

	val _atEnd = !iterator.hasNext
	val _first = if (!_atEnd) iterator.next else ""
	lazy val _rest = new BufferingReader(iterator, position + 1)

	Console println first

	def atEnd = _atEnd
	def first = _first
	def rest = if (atEnd) this else _rest

	def pos = new scala.util.parsing.input.Position { def line = position; def column = 0; def lineContents = _first }
}
    
    val source = io.Source.fromFile("/scratch-local/schmittch/ExaStencils/ScalaExaStencil/Compiler/examples/level4_simple.exa")
    val reader = new BufferingReader(source.getLines)
    val p4 = new ParserL4
    val result = p4.parseFile("/scratch-local/schmittch/ExaStencils/ScalaExaStencil/Compiler/examples/level4_simple.exa")
    
    StateManager.root_ = result
    
    var positionStrategy = new Strategy
    //positionStrategy += new Transformation({case x : scala.util.parsing.input.Positional => println(f"$x @ " +  x.pos); Some(x) })
    positionStrategy += new Transformation({case x : Annotatable => x.getAnnotations.foreach(a => println(x + " " + a.name +" - "+ a.value)); Some(x) })
    positionStrategy.apply
    
    
    //val p4 = new ParserL4
    //val input4 = scala.io.Source.fromFile("/scratch-local/schmittch/ExaStencils/ScalaExaStencil/Compiler/examples/level4_simple.exa").getLines.mkString("\n")
    //DBG(input4)
    //p4.parse(input4)
    
//        val p4 = new ParserL4
//    val input4 = scala.io.Source.fromFile("/scratch-local/schmittch/ExaStencils/ScalaExaStencil/Compiler/examples/level4_simple.exa").getLines.mkString("\n")
//    DBG(input4)
//    p4.parse(input4)
    
  }
}