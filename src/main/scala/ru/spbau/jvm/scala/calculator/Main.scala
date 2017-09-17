package ru.spbau.jvm.scala.calculator

import scala.util.control.Breaks._

import scala.io.StdIn

object Main {

  val unaryOps: Map[Arithmetic, Double => Double] = Map(
    Arithmetic('-') -> { x => -x }
  )

  val outerBinOps: Map[Arithmetic, (Double, Double) => Double] = Map(
    Arithmetic('+') -> { (x, y) => x + y },
    Arithmetic('-') -> { (x, y) => x - y }
  )

  val innerBinOps: Map[Arithmetic, (Double, Double) => Double] = Map(
    Arithmetic('/') -> { (x, y) => x / y },
    Arithmetic('*') -> { (x, y) => x * y }
  )

  val functions: Map[Function, Double => Double] = Map(
    Function("log") -> Math.log,
    Function("sin") -> Math.sin,
    Function("cos") -> Math.cos
  )

  private val lexer = new Lexer(outerBinOps.keySet ++ innerBinOps.keySet ++ unaryOps.keySet, functions.keySet)
  private val interpreter = new Interpreter(unaryOps, List(outerBinOps, innerBinOps), functions)

  def main(args: Array[String]): Unit = {
    breakable {
      while (true) {
        try {
          val line = StdIn.readLine("> ")
          if (line == null) break
          val tokens = lexer.tokenize(line)
          if (tokens.nonEmpty) {
            try {
              val result = interpreter.parse(tokens)
              println(result)
            }
            catch {
              case e: ParserException => println(s"Error while parsing: ${e.msg}")
            }
          }
          else break
        }
        catch {
          case e: LexerException => println(s"Error while lexing: ${e.msg}")
        }
      }
    }
  }
}
