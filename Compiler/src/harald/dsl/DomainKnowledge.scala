package harald.dsl

import scala.collection.mutable.ListBuffer
import harald.Impl._
import harald.ast.TreeL2

object DomainKnowledge extends ExaKnowledge {
    
  var tree : TreeL2 = new TreeL2()
  
  var name: String = "default domain" // always set - either default or by DSL

  // user input on level 1 
  var domain_L1: Option[Tuple2[String, String]] = None
  var function_L1: ListBuffer[Tuple2[String, Int]] = ListBuffer()
  var unknown_L1: ListBuffer[Tuple2[String, Int]] = ListBuffer()
  var operator_L1: ListBuffer[Tuple2[String, String]] = ListBuffer()
  var pde_L1: Option[String] = None
  var pdebc_L1: Option[Tuple2[String, String]] = None
  var accuracy_L1: Option[Int] = None
  var generate_L1: Option[Int] = None

  // user input on level 2
  var discrete_domain_L2: Option[String] = None
  var fragment_L2: Option[Tuple2[String, String]] = None
  var xsize_L2: Option[Int] = None
  var ysize_L2: Option[Int] = None
  var zsize_L2: Option[Int] = None
  var datatype_L2: Option[String] = None

  // user input on level 3
  var nprae_L3: Option[Int] = None
  var npost_L3: Option[Int] = None
  var ncoarse_L3: Option[Int] = None
  var smoother_L3: Option[String] = None
  var interpolation_L3: Option[String] = None
  var restriction_L3: Option[String] = None
  var coarsesolver_L3: Option[String] = None
  var nlevels_L3: Option[Int] = None
  var iters_L3: Option[Int] = None
  var restr_order_L3: Option[Int] = None
  var int_order_L3: Option[Int] = None
  var cycle_L3: Option[String] = None
  var omega_L3: Option[Double] = None
  var accuracy_L3: Option[Int] = None

  // user input HW
  var hardware_HW: Option[String] = None
  var node_HW: Option[String] = None
  var cluster_HW: Option[String] = None
  var bandwidth_HW: Option[Int] = None // in GB/s
  var peak_HW: Option[Int] = None // in GFLOPS
  var networkbandwidth_HW: Option[Int] = None
  var cores_HW: Option[Int] = None
  var sockets_HW: Option[Int] = None
  var nodes_HW: Option[Int] = None
  
  var use_FE = false // set to true if one or more stencils require it
  var use_gpu = false // set to true if hw is gpu
  var use_MPI = false // set to true if nodes > 1
  var use_Openmp = false // set to true if cores > 1

  var CUDA_BLOCKSIZE: ListBuffer[Int] = ListBuffer(16,16)
  
  var global_variables: ListBuffer[ParameterInfo] = ListBuffer()
  var global_fields: ListBuffer[ImplField] = new ListBuffer()
  var global_ghost_fields: ListBuffer[ImplField] = new ListBuffer()
  var global_stencils: ListBuffer[ImplStencil] = new ListBuffer()

  case class StencilKnowledge(val domain: String, val distype: String, val order: String, val operators: ListBuffer[String])
  case class LoopKnowledge(val domain: String, val where: String, val stride: String)

  def rule_idxArray_cpp(): String = {
    if (DomainKnowledge.fragment_L2.get._2.equals("Regular_Square"))
      return "(i0,i1)" //List("i0","i1") //
    else if (DomainKnowledge.fragment_L2.get._2.equals("Regular_Cube"))
      return "(i0,i1,i2)" //List("i0","i1","i2") //
    else
      return "(i0)" //List("i0") // 
  }

  def rule_idxArray_cuda(): String = {
    if (DomainKnowledge.fragment_L2.get._2.equals("Regular_Square"))
      return "[" + IdxKnowledge.mapidxToLinear(ListBuffer("i0", "i1"), ListBuffer("s1", "s2")) + "]"
    else if (DomainKnowledge.fragment_L2.get._2.equals("Regular_Cube"))
      return "[" + IdxKnowledge.mapidxToLinear(ListBuffer("i0", "i1", "i2"), ListBuffer("s1", "s2", "s3")) + "]"
    else
      return "[i0]" //List("i0") // 
  }

  def rule_dim(): Int = {
    if (DomainKnowledge.domain_L1.get._2.equals("UnitSquare"))
      return 2
    else if (DomainKnowledge.domain_L1.get._2.equals("UnitCube"))
      return 3
    else
      return 1
  }

  def rule_mapfineTocoarse(s: String): String = "(" + s + ")/2"
  def rule_mapcoarseTofine(s: String): String = "2*(" + s + ")"

  def rule_addpoints(location: String): Int = {
    location match {
      case "nodes" => 1
      case "cells" => 2
    }
  }

  //		var CostInfo: collection.mutable.Map[String, Int] = collection.mutable.Map()

  var arraysizes: ListBuffer[ListBuffer[Long]] = ListBuffer()
  var arraysizeslocal: ListBuffer[ListBuffer[Long]] = ListBuffer()

  def initarraysizes {
    var sx: Int = xsize_L2.getOrElse(1);
    var sy: Int = ysize_L2.getOrElse(1);
    var sz: Int = zsize_L2.getOrElse(1);
    var points: Long = sx * sy * sz

    var clist: ListBuffer[Long] = ListBuffer()
    clist += points
    clist += sx
    clist += sy
    if (DomainKnowledge.rule_dim() == 3)
      clist += sz
    DomainKnowledge.arraysizes += clist

    for (i <- 0 to (DomainKnowledge.nlevels_L3.getOrElse(1) - 1)) {
      var clist: ListBuffer[Long] = ListBuffer()
      sx = sx / 2;
      sy = sy / 2;
      if (DomainKnowledge.rule_dim() == 3) {
        sz = sz / 2;
      }

      clist += sx * sy * sz
      clist += sx
      clist += sy
      if (DomainKnowledge.rule_dim() == 3)
        clist += sz
      DomainKnowledge.arraysizes += clist
    }
  }

  var fragments: ListBuffer[Fragment] = ListBuffer()

  def initfragments() {
    var v: List[Vertex] = List()
    var e: List[Edge] = List()
    var f: List[Face] = List()

    if (DomainKnowledge.fragment_L2.get._2.equals("Regular_Square")) {
      v = List(new Vertex(ListBuffer(0.0, 0.0)), new Vertex(ListBuffer(0.0, 1.0)), new Vertex(ListBuffer(1.0, 0.0)), new Vertex(ListBuffer(1.0, 1.0)))
      e = List(new Edge(v(0), v(1)), new Edge(v(0), v(2)), new Edge(v(1), v(3)), new Edge(v(2), v(3)))

      f = List(new Face(List(e(0), e(1), e(2), e(3)), v))

      fragments += new Fragment(f, e, v)
    }

    if (DomainKnowledge.fragment_L2.get._2.equals("Regular_Cube")) {
      v = List(new Vertex(ListBuffer(0.0, 0.0, 0.0)), new Vertex(ListBuffer(0.0, 1.0, 0.0)), new Vertex(ListBuffer(1.0, 0.0, 0.0)), new Vertex(ListBuffer(0.0, 0.0, 1.0)),
        new Vertex(ListBuffer(1.0, 1.0, 0.0)), new Vertex(ListBuffer(0.0, 1.0, 1.0)), new Vertex(ListBuffer(1.0, 0.0, 1.0)), new Vertex(ListBuffer(1.0, 1.0, 1.0)))

      e = List(new Edge(v(0), v(1)), new Edge(v(0), v(2)), new Edge(v(0), v(3)), new Edge(v(1), v(4)), new Edge(v(1), v(5)), new Edge(v(2), v(4)), new Edge(v(2), v(6)),
        new Edge(v(3), v(5)), new Edge(v(3), v(6)), new Edge(v(4), v(7)), new Edge(v(5), v(7)), new Edge(v(6), v(7)))

      f = List(new Face(List(e(0), e(1), e(3), e(5)), List(v(0), v(1), v(2), v(4))), new Face(List(e(7), e(8), e(10), e(11)), List(v(3), v(5), v(6), v(7))),
        new Face(List(e(0), e(2), e(4), e(7)), List(v(0), v(1), v(3), v(5))), new Face(List(e(5), e(6), e(9), e(11)), List(v(2), v(4), v(6), v(7))),
        new Face(List(e(3), e(4), e(9), e(10)), List(v(1), v(4), v(5), v(7))), new Face(List(e(1), e(2), e(6), e(8)), List(v(0), v(2), v(3), v(6))))

      fragments += new Fragment(f, e, v)
    }

  }

  def setglobalvariables() {

    global_variables += new ParameterInfo("xsize", "int", DomainKnowledge.xsize_L2.getOrElse(1))
    global_variables += new ParameterInfo("ysize", "int", DomainKnowledge.ysize_L2.getOrElse(1))
    if (DomainKnowledge.rule_dim() == 3)
      global_variables += new ParameterInfo("zsize", "int", DomainKnowledge.zsize_L2.getOrElse(1))
    global_variables += new ParameterInfo("nprae", "int", DomainKnowledge.nprae_L3.getOrElse(1))
    global_variables += new ParameterInfo("npost", "int", DomainKnowledge.npost_L3.getOrElse(1))
    global_variables += new ParameterInfo("ncoarse", "int", DomainKnowledge.ncoarse_L3.getOrElse(1))
    global_variables += new ParameterInfo("nlevels", "int", DomainKnowledge.nlevels_L3.getOrElse(1))
    global_variables += new ParameterInfo("omega", "double", (DomainKnowledge.omega_L3.getOrElse(1)).toString.toInt)

    if (DomainKnowledge.nodes_HW.get > 1) {
      global_variables += new ParameterInfo("rank", "int", 0)
      global_variables += new ParameterInfo("noprocesses", "int", DomainKnowledge.nodes_HW.get, "")
      global_variables += new ParameterInfo("COMM_CART", "MPI_Comm")
      global_variables += new ParameterInfo(s"Pdims[${DomainKnowledge.rule_dim()}]", "int")
      global_variables += new ParameterInfo(s"Pcoords[${DomainKnowledge.rule_dim()}]", "int")
      val neighbors = DomainKnowledge.rule_dim() * 2
      global_variables += new ParameterInfo(s"Pnb[${neighbors}]", "int")

    }

  }


  def getglobaldatatype(): (String, Int) = {
    if (DomainKnowledge.accuracy_L1.get > 1)
      return "Double" -> 8
    else
      return "Float" -> 4
  }
  
    def transform_datatype_cpp(dt: String): String = {
    dt match {
      case "Unit" => return "void"
      case "Double" => return "double"
      case "Float" => return "float"
      case "Complex" => return "std::complex<double>"
      case "Int" => return "int"
      case "Array" => return "MyArray" + s"<${DomainKnowledge.datatype_L2.getOrElse("double")}>&" // extclasses.get("Array").get.name
      case _ => return "unknown type"
    }
  }

  def transform_datatype_cpp_cuda(dt: String): String = {
    dt match {
      case "Unit" => return "void"
      case "Double" => return "double"
      case "Float" => return "float"
      case "Complex" => return "std::complex<double>"
      case "Int" => return "int"
      case "Array" => return "MyArrayCuda" + s"<${DomainKnowledge.datatype_L2.getOrElse("double")}>&" // extclasses.get("Array").get.name
      case _ => return "unknown type"
    }
  }
  
  def transform_datatype_cuda(dt: String): String = {
    dt match {
      case "Unit" => return "void"
      case "Double" => return "double"
      case "Float" => return "float"
      case "Int" => return "int"
      case "Array" => return s"${DomainKnowledge.datatype_L2.getOrElse("double")}*" 
      case _ => return "unknown type"
    }
  }

}
