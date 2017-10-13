package test.parser.l4

import exastencils.parsers.l4.L4_Parser

object TestParserL4 {
  val parser = L4_Parser

  def out(h : String, s : Any) = {
    println("#######################################################")
    println(s"#### $h")
    println("")
    println(s.toString)
    println("")
  }

  def main(args : Array[String]) : Unit = {
    out("@coarser", parser.parse(
      """def t() : Unit {
someFunction @(coarser) (arg1, arg2)
  }"""))

    out("@(1 to 5)", parser.parse(
      """def t @(1 to 5) () : Unit  {
  }"""))

    out("@(current + 1)", parser.parse(
      """def t() : Unit {
      someFunction @(current + 1) (arg1, arg2)
  }"""))

    out("@(not(finest + 3 to finest))", parser.parse(
      """def t @(not(finest+3 to finest)) () : Unit {
  }"""))

  }
}