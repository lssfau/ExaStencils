package test.parser.l4

import scala.util.parsing.combinator.syntactical._

object TestParsing {

  class MyParser extends StandardTokenParsers {
    lexical.reserved += ("foo", "bar")
    lexical.delimiters += ","

    lazy val program = (("foo" ~> integers) <~ "bar")
    lazy val integers = (numericLit <~ ",").* ~ numericLit ^^ { case l ~ n => l ++ n }

    def doParse(s : String) = {
      phrase(program)(new lexical.Scanner(s))
    }
  }

  def main(args : Array[String]) : Unit = {
    val parser = new MyParser
    println(parser.doParse("foo 1,2,3,4 bar"))
  }
}