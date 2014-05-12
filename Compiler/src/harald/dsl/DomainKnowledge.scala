package harald.dsl

import scala.collection.mutable.ListBuffer
import harald.Impl._
import harald.Continuous._
import harald.Abstract._
import harald.Discrete._
import harald.ast.TreeL2

object DomainKnowledge extends ExaKnowledge {

  var tree : TreeL2 = new TreeL2()

  var debugmode = false

  var name : String = "default domain" // always set - either default or by DSL

  var optionslist : ListBuffer[String] = ListBuffer()

  var outputpath = ""

  // user input on level 1 
  var domain_L1 : Option[Tuple2[String, String]] = None
  var function_L1 : ListBuffer[Tuple2[String, Int]] = ListBuffer()
  var unknown_L1 : ListBuffer[Tuple2[String, Int]] = ListBuffer()
  var operator_L1 : ListBuffer[Tuple2[String, List[String]]] = ListBuffer()
  //  var pde_L1: Option[String] = None
  //  var functional_L1: Option[String] = None
  //  var dataterm_L1: Option[String] = None
  //  var regularizer_L1: Option[String] = None
  var pdebc_L1 : Option[Tuple2[String, String]] = None // TODO: remove!
  var accuracy_L1 : Option[Int] = None
  var system_L1 : Option[Int] = None // TODO: remove!
  var generate_L1 : Option[Int] = None

  // user input on level 2
  var discrete_domain_L2 : Option[String] = None
  var fragment_L2 : Option[Tuple2[String, String]] = None
  var xsize_L2 : Option[Int] = None
  var ysize_L2 : Option[Int] = None
  var zsize_L2 : Option[Int] = None
  var xcoarsefac_L2 : Option[Int] = None
  var ycoarsefac_L2 : Option[Int] = None
  var zcoarsefac_L2 : Option[Int] = None
  var nlevels_L2 : Option[Int] = None
  //  var datatype_L2: Option[String] = None
  var globaldatatype_L2 = "Double"
  var unknownlocation_L2 = "nodes"

  // user input on level 3
  //  var nprae_L3: Option[Int] = None
  //  var npost_L3: Option[Int] = None
  //  var ncoarse_L3: Option[Int] = None
  var smoother_L3 : Option[String] = None
  //  var interpolation_L3: Option[String] = None
  //  var restriction_L3: Option[String] = None
  //  var coarsesolver_L3: Option[String] = None
  var iters_L3 : Option[Int] = None
  //  var restr_order_L3: Option[Int] = None
  //  var int_order_L3: Option[Int] = None
  var cycle_L3 : Option[String] = None
  //  var omega_L3: Option[Double] = None
  var tau_L3 : Double = 1000.01
  var accuracy_L3 : Option[Int] = None

  // user input HW
  var hardware_HW : Option[String] = None
  var node_HW : Option[String] = None
  var cluster_HW : Option[String] = None
  var bandwidth_HW : Option[Int] = None // in GB/s
  var peak_HW : Option[Int] = None // in GFLOPS
  var networkbandwidth_HW : Option[Int] = None
  var cores_HW : Option[Int] = None
  var sockets_HW : Option[Int] = None
  var nodes_HW : Option[Int] = None

  var stenciltype = "constant" // variable, nonlinear
  var vectorentries = 1

  var use_FE = false // set to true if one or more stencils require it
  var use_gpu = false // set to true if hw is gpu
  var use_MPI = false // set to true if nodes > 1
  var use_Openmp = false // set to true if cores > 1
  var use_Windows = true

  var CUDA_BLOCKSIZE : ListBuffer[Int] = ListBuffer(16, 16)

  var global_variables : ListBuffer[ParameterInfo] = ListBuffer()
  var global_fields : ListBuffer[ImplField] = new ListBuffer()
  var global_ghost_fields : ListBuffer[ImplField] = new ListBuffer()
  var global_stencils : ListBuffer[ImplStencil] = new ListBuffer()

  var cont_domains : ListBuffer[ContDomain] = ListBuffer()
  var cont_functions : ListBuffer[ContFunction] = ListBuffer()
  var cont_operators : ListBuffer[ContOperator] = ListBuffer()
  var cont_consts : ListBuffer[ContConstant] = ListBuffer()
  var cont_equations : ListBuffer[ContEquation] = ListBuffer()

  var discr_functions : ListBuffer[DiscrFunction] = ListBuffer()
  var discr_operators : ListBuffer[DiscrOperator] = ListBuffer()
  var discr_equations : ListBuffer[DiscrEquation] = ListBuffer()
  var discr_consts : ListBuffer[DiscrConstant] = ListBuffer()
  var discr_sets : ListBuffer[DiscrSet] = ListBuffer()
  var discr_iterations : ListBuffer[DiscrIteration] = ListBuffer()

  var ArrayClassName = "YourArray"
  var StencilClassName = "YourStencil"
  var ArrayClassNameGPU = "YourArrayCuda"
  var StencilClassNameVar = "MyStencilVar"
  var MatrixClassName = "MyMatrix"

  def setglobalobjects_L1() = {
    DomainKnowledge.function_L1.clear
    DomainKnowledge.operator_L1.clear
    DomainKnowledge.unknown_L1.clear

    for (eq <- cont_equations) {
      if (eq.typeofeq.contains("pde")) {
        val strf : String = eq.rhs match {
          case ContIntegral(d, b) => b.toString()
          case _                  => eq.rhs.toString
        }
        println("function " + strf)
        DomainKnowledge.function_L1 += new Tuple2(strf, 0)
        val stra : List[String] = eq.lhs match {
          case ContOpapply(a, b)                  => List(a.nam, b.nam)
          case ContIntegral(d, ContOpapply(a, b)) => List(a.nam, b.nam)
          case _                                  => List()
        }
        DomainKnowledge.operator_L1 += new Tuple2(stra(0), List(stra(0)))
        DomainKnowledge.unknown_L1 += new Tuple2(stra(1), 0)
        println("operator " + stra(0))
        println("unknown " + stra(1))
      }

      if (eq.typeofeq.contains("bc")) {
        val str = eq.lhs match {
          case ContIntegral(_, _) => "dn"
          case _                  => "zero"
          //           case ContVariable(s,_) => s.name
        }
        DomainKnowledge.pdebc_L1 = Some(Tuple2(DomainKnowledge.unknown_L1(0)._1, str))
        println("bc " + str + " " + DomainKnowledge.unknown_L1(0)._1)
      }
    }
  }

  case class StencilKnowledge(val domain : String, val distype : String, val order : String, val location : String, val operators : ListBuffer[String])
  case class IntStencilKnowledge(val dim : Int, val order : String, val location : String)
  case class LoopKnowledge(val domain : String, val where : String, val stride : String)

  def rule_idxArray_cpp() : String = {
    if (fragment_L2.get._2.equals("Regular_Square"))
      return "(i0,i1)" //List("i0","i1") //
    else if (fragment_L2.get._2.equals("Regular_Cube"))
      return "(i0,i1,i2)" //List("i0","i1","i2") //
    else
      return "(i0)" //List("i0") // 
  }

  def rule_idxArray_cuda() : String = {
    if (fragment_L2.get._2.equals("Regular_Square"))
      return "[" + IdxKnowledge.mapidxToLinear(ListBuffer("i0", "i1"), ListBuffer("s1", "s2")) + "]"
    else if (fragment_L2.get._2.equals("Regular_Cube"))
      return "[" + IdxKnowledge.mapidxToLinear(ListBuffer("i0", "i1", "i2"), ListBuffer("s1", "s2", "s3")) + "]"
    else
      return "[i0]" //List("i0") // 
  }

  def rule_dim() : Int = {
    println("rule_dim: " + cont_domains(0).subdoms(0).intervals.size)
    return cont_domains(0).subdoms(0).intervals.size
    /*    if (domain_L1.get._2.equals("UnitSquare"))
      return 2
    else if (domain_L1.get._2.equals("UnitCube"))
      return 3
    else
      return 1
      * 
      */
  }

  def rule_mapfineTocoarse(s : String, dim : Int) : String = "(" + s + ")/" + s"${coarsefac(dim)}"
  def rule_mapcoarseTofine(s : String, dim : Int) : String = s"${coarsefac(dim)}*(" + s + ")"

  def rule_addpoints(location : String) : Int = {
    location match {
      case "nodes" => 1
      case "cells" => 2
    }
  }

  //		var CostInfo: collection.mutable.Map[String, Int] = collection.mutable.Map()

  var arraysizes : ListBuffer[ListBuffer[Long]] = ListBuffer()
  var arraysizeslocal : ListBuffer[ListBuffer[Long]] = ListBuffer()
  var coarsefac : ListBuffer[Int] = ListBuffer()

  def initarraysizes = {
    arraysizes.clear
    coarsefac.clear

    coarsefac += xcoarsefac_L2.getOrElse(1)
    coarsefac += ycoarsefac_L2.getOrElse(1)
    coarsefac += zcoarsefac_L2.getOrElse(1)

    var sx : Int = xsize_L2.getOrElse(1);
    var sy : Int = ysize_L2.getOrElse(1);
    var sz : Int = zsize_L2.getOrElse(1);
    var points : Long = sx * sy * sz
    var nlev : Int = nlevels_L2.get

    var clist : ListBuffer[Long] = ListBuffer()
    clist += points
    clist += sx
    clist += sy
    if (rule_dim() == 3)
      clist += sz
    arraysizes += clist

    for (i <- 0 to (nlev - 1)) {
      var clist : ListBuffer[Long] = ListBuffer()
      sx = sx / coarsefac(0);
      sy = sy / coarsefac(1);
      if (rule_dim() == 3) {
        sz = sz / coarsefac(2);
      }

      clist += sx * sy * sz
      clist += sx
      clist += sy
      if (rule_dim() == 3)
        clist += sz
      arraysizes += clist
    }
  }

  var fragments : ListBuffer[Fragment] = ListBuffer()

  def initfragments() : Unit = {
    fragments.clear

    var v : ListBuffer[Vertex] = ListBuffer()
    var e : ListBuffer[Edge] = ListBuffer()
    var f : ListBuffer[Face] = ListBuffer()

    if (cont_domains(0).subdoms(0).intervals.size == 2) { // fragment_L2.get._2.equals("Regular_Square")
      //      v = List(new Vertex(ListBuffer(0.0, 0.0)), new Vertex(ListBuffer(0.0, 1.0)), new Vertex(ListBuffer(1.0, 0.0)), new Vertex(ListBuffer(1.0, 1.0)))
      for (i <- 0 to 1)
        for (j <- 0 to 1)
          v += new Vertex(ListBuffer(cont_domains(0).subdoms(0).intervals(0).values(i), cont_domains(0).subdoms(0).intervals(1).values(j)))

      //      v = List(new Vertex(ListBuffer(0.0, 0.0)), new Vertex(ListBuffer(0.0, 1.0)), new Vertex(ListBuffer(1.0, 0.0)), new Vertex(ListBuffer(1.0, 1.0)))

      e = ListBuffer(new Edge(v(0), v(1)), new Edge(v(0), v(2)), new Edge(v(1), v(3)), new Edge(v(2), v(3)))

      f = ListBuffer(new Face(ListBuffer(e(0), e(1), e(2), e(3)), v))

      fragments += new Fragment(f, e, v)
    }

    if (cont_domains(0).subdoms(0).intervals.size == 3) { // fragment_L2.get._2.equals("Regular_Cube")
      for (i <- 0 to 1)
        for (j <- 0 to 1)
          for (k <- 0 to 1)
            v += new Vertex(ListBuffer(cont_domains(0).subdoms(0).intervals(0).values(i), cont_domains(0).subdoms(0).intervals(1).values(j), cont_domains(0).subdoms(0).intervals(2).values(j)))

      //    v = ListBuffer(new Vertex(ListBuffer(0.0, 0.0, 0.0)), new Vertex(ListBuffer(0.0, 1.0, 0.0)), new Vertex(ListBuffer(1.0, 0.0, 0.0)), new Vertex(ListBuffer(0.0, 0.0, 1.0)),
      //      new Vertex(ListBuffer(1.0, 1.0, 0.0)), new Vertex(ListBuffer(0.0, 1.0, 1.0)), new Vertex(ListBuffer(1.0, 0.0, 1.0)), new Vertex(ListBuffer(1.0, 1.0, 1.0)))

      e = ListBuffer(new Edge(v(0), v(1)), new Edge(v(0), v(2)), new Edge(v(0), v(3)), new Edge(v(1), v(4)), new Edge(v(1), v(5)), new Edge(v(2), v(4)), new Edge(v(2), v(6)),
        new Edge(v(3), v(5)), new Edge(v(3), v(6)), new Edge(v(4), v(7)), new Edge(v(5), v(7)), new Edge(v(6), v(7)))

      f = ListBuffer(new Face(ListBuffer(e(0), e(1), e(3), e(5)), ListBuffer(v(0), v(1), v(2), v(4))), new Face(ListBuffer(e(7), e(8), e(10), e(11)), ListBuffer(v(3), v(5), v(6), v(7))),
        new Face(ListBuffer(e(0), e(2), e(4), e(7)), ListBuffer(v(0), v(1), v(3), v(5))), new Face(ListBuffer(e(5), e(6), e(9), e(11)), ListBuffer(v(2), v(4), v(6), v(7))),
        new Face(ListBuffer(e(3), e(4), e(9), e(10)), ListBuffer(v(1), v(4), v(5), v(7))), new Face(ListBuffer(e(1), e(2), e(6), e(8)), ListBuffer(v(0), v(2), v(3), v(6))))

      fragments += new Fragment(f, e, v)
    }

  }

  def setglobalvariables(optionslist : ListBuffer[String]) = {
    global_variables.clear

    global_variables += new ParameterInfo("xsize", "int", xsize_L2.getOrElse(1).toInt)
    global_variables += new ParameterInfo("ysize", "int", ysize_L2.getOrElse(1).toInt)
    if (rule_dim() == 3)
      global_variables += new ParameterInfo("zsize", "int", zsize_L2.getOrElse(1).toInt)
    if (rule_dim() == 2)
      global_variables += new ParameterInfo("xyzsize", "int", xsize_L2.getOrElse(1).toInt * ysize_L2.getOrElse(1).toInt)
    else if (rule_dim() == 3)
      global_variables += new ParameterInfo("xyzsize", "int", xsize_L2.getOrElse(1).toInt * ysize_L2.getOrElse(1).toInt * zsize_L2.getOrElse(1).toInt)

    //    global_variables += new ParameterInfo("nprae", "int", nprae_L3.getOrElse(1).toInt)
    //    global_variables += new ParameterInfo("npost", "int", npost_L3.getOrElse(1).toInt)
    //    global_variables += new ParameterInfo("ncoarse", "int", ncoarse_L3.getOrElse(1).toInt)
    global_variables += new ParameterInfo("nlevels", "int", nlevels_L2.getOrElse(1).toInt)
    //    global_variables += new ParameterInfo("omega", s"${plaintype}", omega_L3.get.toString.toDouble)
    var h = 1.0 / xsize_L2.get
    var hstr = "{" + h
    for (i <- 1 to nlevels_L2.getOrElse(1).toInt - 1) {
      h *= 2.0
      hstr += "," + h
    }
    hstr += "}"
    global_variables += new ParameterInfo(s"meshsize[${nlevels_L2.getOrElse(1).toInt}]", s"${DomainKnowledge.plaintype}", 1, hstr) // h.toDouble)

    var ostr = "{\"" + optionslist(0) + "\""
    for (i <- 1 to optionslist.size - 1)
      ostr += ",\"" + optionslist(i) + "\""
    ostr += "}"

    global_variables += new ParameterInfo(s"optionstring[${optionslist.size}]", "string", 1, ostr) // h.toDouble)

    if (nodes_HW.get > 1) {
      global_variables += new ParameterInfo("rank", "int", 0)
      global_variables += new ParameterInfo("noprocesses", "int", nodes_HW.get, "", "")
      global_variables += new ParameterInfo("COMM_CART", "MPI_Comm")
      global_variables += new ParameterInfo(s"Pdims[${rule_dim()}]", "int")
      global_variables += new ParameterInfo(s"Pcoords[${rule_dim()}]", "int")
      val neighbors = rule_dim() * 2
      global_variables += new ParameterInfo(s"Pnb[${neighbors}]", "int")

    }

  }

  def clearData() = {
    global_variables.clear
    global_fields.clear
    global_ghost_fields.clear
    global_stencils.clear

    function_L1.clear
    unknown_L1.clear
    operator_L1.clear

    arraysizes.clear
    fragments.clear
  }

  /*  def getglobaldatatype_plain(): (String, Int) = {

   // return "Float" -> 4
    
    if (DomainKnowledge.operator_L1(0)._2(0).equals("ComplexDiffusion")) {
      if (DomainKnowledge.use_gpu)
       return "Float" -> 4
      else
       return "Complex" -> 16
    } else if (accuracy_L1.get > 1)
      if (DomainKnowledge.use_gpu)
       return "Float" -> 4
      else      
       return "Double" -> 8
    else
      return "Float" -> 4
  }
  */
  def transform_datatype_cpp(dt : String) : String = {
    dt match {
      case "Unit"          => return "void"
      case "Double"        => return "double"
      case "Float"         => return "float"
      case "Float2"        => return "float2"
      case "ComplexFloat"  => return "std::complex<float> "
      case "ComplexDouble" => return "std::complex<double> "
      case "Int"           => return "int"
      case "Array"         => return s"${ArrayClassName}<${transform_datatype_cpp(DomainKnowledge.globaldatatype_L2)}>&" // extclasses.get("Array").get.name
      case _               => return "unknown type"
    }
  }

  def transform_datatype_cpp_cuda(dt : String) : String = {
    dt match {
      case "Unit"          => return "void"
      case "Double"        => return "double"
      case "Float"         => return "float"
      case "Float2"        => return "float2"
      case "ComplexDouble" => return "std::complex<double> "
      case "ComplexFloat"  => return "std::complex<float> "
      case "Int"           => return "int"
      case "Array"         => return s"${ArrayClassNameGPU}<${transform_datatype_cpp_cuda(DomainKnowledge.globaldatatype_L2)}>&" // extclasses.get("Array").get.name
      case _               => return "unknown type"
    }
  }

  def plaintype() : String = {
    DomainKnowledge.globaldatatype_L2 match {
      case "Double"        => return "double"
      case "Float"         => return "float"
      case "Float2"        => return "float"
      case "ComplexDouble" => return "double"
      case "ComplexFloat"  => return "float"
    }
  }

  def abstracttype() : String = {
    DomainKnowledge.globaldatatype_L2 match {
      case "ComplexDouble" => return "Double"
      case "ComplexFloat"  => return "Float"
      case _               => return DomainKnowledge.globaldatatype_L2
    }
  }

  def getmaxlevel : Int = {

    var nlev = 1
    var minsize : Int = Math.min(xsize_L2.get, ysize_L2.get) // TODO: 3D!
    while (minsize > 2) {
      minsize /= 2
      nlev += 1
    }
    return nlev
  }

  def transform_datatype_cuda(dt : String) : String = {
    dt match {
      case "Unit"   => return "void"
      case "Double" => return "double"
      case "Float"  => return "float"
      case "Int"    => return "int"
      case "Array"  => return s"${transform_datatype_cuda(DomainKnowledge.globaldatatype_L2)}*"
      case _        => return "unknown type"
    }
  }

  def fornberg(z : Double, x : Array[Double], m : Int) : Array[Array[Double]] = {
    // Array[Array[Double]]

    //    = Array.ofDim[Double](3,3)
    /*
  function c = weights(z,x,m)
% Calculates FD weights. The parameters are:
%  z   location where approximations are to be accurate,
%  x   vector with x-coordinates for grid points,
%  m   highest derivative that we want to find weights for
%  c   array size m+1,lentgh(x) containing (as output) in 
%      successive rows the weights for derivatives 0,1,...,m.
  */
    /*
  var n = x.length 
  var c1=1.0
  var c4= x(0)-z
    var c = Array.ofDim[Double](m+1,n) 
  c(0)(0)=1;
  //return c
  
  for (i <- 1 to n-1) {
   var mn= Math.min(i,m+1)
   var c2=1.0
   var c5=c4
   c4=x(i)-z;
  for (j <- 0 to i-2) {
      var c3=x(i)-x(j)
      c2=c2*c3;
      if (j==i-2) { 
         for (k <- 1 to mn-1)
         c(k)(i)=c1*((k-1)*c(k-1)(i-1)-c5*c(k)(i-1))/c2;
         c(0)(i)= -c1*c5*c(0)(i-1)/c2;
      }
      for (k <- 1 to mn-1)
      c(k)(j)=(c4*c(k)(j)-(k-1)*c(k-1)(j))/c3;
      c(0)(j)=c4*c(0)(j)/c3;
    }
   c1=c2;
  }
  */
    var n = x.length
    var c1 = 1.0
    var c4 = x(0) - z
    var c = Array.ofDim[Double](m + 1 + 1, n + 1)
    var temp = Array.ofDim[Double](m + 1 + 1, n + 1)
    for (i <- 0 to c.length - 1)
      for (j <- 0 to c(i).length - 1)
        c(i)(j) = 0;

    c(1)(1) = 1;
    //return c
    var c2 = 1.0

    for (i <- 2 to n) {
      var mn = Math.min(i, m + 1)
      c2 = 1.0
      var c5 = c4
      c4 = x(i - 1) - z;
      for (j <- 1 to i - 1) {
        var c3 = x(i - 1) - x(j - 1)
        c2 = c2 * c3;
        if (j == (i - 1)) {
          for (k <- 2 to mn)
            temp(k)(i) = c1 * ((k - 1) * c(k - 1)(i - 1) - c5 * c(k)(i - 1)) / c2;
          for (k <- 2 to mn)
            c(k)(i) = temp(k)(i)
          c(1)(i) = -c1 * c5 * c(1)(i - 1) / c2;
        }
        for (k <- 2 to mn)
          temp(k)(j) = (c4 * c(k)(j) - (k - 1) * c(k - 1)(j)) / c3;
        for (k <- 2 to mn)
          c(k)(j) = temp(k)(j)
        c(1)(j) = c4 * c(1)(j) / c3;
      }
      c1 = c2;
    }

    return c

  }

  def tensor_prod[T : Manifest](A : Array[Array[T]], B : Array[Array[T]])(implicit num : Numeric[T]) : Array[Array[T]] = {
    def smm(s : T, m : Array[Array[T]]) = m.map(_.map(x => num.times(x, s)))
    def concat(A : Array[Array[T]], B : Array[Array[T]]) = (A, B).zipped.map(_ ++ _)
    A flatMap (row => row map (s => smm(s, B)) reduce concat)
  }
}
