package harald.dsl

import scala.collection.mutable.ListBuffer
import harald.Abstract._
import harald.ast.TreeL2

class TransformL4(treel2 : TreeL2) {

  def assignGlobalDefines = {
    /*  
  	  for (e <- exaDefinitions) {
  	   GlobalDefines += e.transform
  	  }
	 */
  }
}

/*
  class Node(parent: Node) {
    var childs: ListBuffer[Node] = new ListBuffer()
  }
*/
// parser DSL level representation

//class BaseL4()

case class Arg(val name : String, val dtype : String)
case class Param(val name : String, val dtype : String) {
  override def toString = dtype + " " + name
  //      def toString_cpp = dtype + " " + name
}

// implementation level representation
/*
  class ParamInfo(val name: String, val dtype: String) {
    override def toString = dtype + " " + name
    //     override def toString = name + ":" + dtype
  }
*/
// transformations

class Context(ids : Map[String, AbstractLet], parent : Option[Context]) {
  lazy val child = new Context(Map[String, AbstractLet](), Some(this))

  def +(binding : AbstractLet) = {
    val newIDs = ids + (binding.id -> binding)
    new Context(newIDs, parent)
  }

  def resolve(id : String) : Option[AbstractLet] = {
    if (ids contains id) {
      Some(ids(id))
    } else {
      parent match {
        case Some(c) => c resolve id
        case None    => None
      }
    }
  }
}

object EmptyContext extends Context(Map[String, AbstractLet](), None)

class Interpreter(tree : List[AbstractFunction]) {
  def run() = {
    walkTree(tree, EmptyContext)
  }

  private def walkTree(tree : List[AbstractFunction], context : Context) : Unit = {
    tree match {
      case f :: rest => {
        //println(f.toString)
        walkTree(rest, context)
      }
      case Nil => ()
    }
  }
}

class TypeInfo(n : String, dim : Int, dt : String = "") {

  val d : Int = dim
  val datatype : String = dt

  def toString_cpp : String = {
    var s : String = ""
    if (dim == 0)
      s = "" //"Scalar" 
    else if (dim == 1) {
      s = DomainKnowledge.rule_idxArray_cpp()
    } else if (dim == 2)
      s = "(Matrix (i0,i1))"

    return s
  }

  def toString_cuda : String = {
    var s : String = ""
    if (dim == 0)
      s = "" //"Scalar" 
    else if (dim == 1) {
      s = "[global_idx]" // DomainKnowledge.rule_idxArray_cuda()
    } else if (dim == 2)
      s = "(Matrix (i0,i1))"

    return s
  }
}

class OperatorInfo(n : String) {
  val name = n
  override def toString = name
  def toString_cpp : String = name
}

class ParameterInfo(val name : String, val dtype : String, val value : Double = 0.0, val valstring : String = "", val modifier : String = "const") {
  var v = value
  override def toString = name + ":" + dtype + " = " + v
  def toString_cpp : String = dtype + " " + name
}
