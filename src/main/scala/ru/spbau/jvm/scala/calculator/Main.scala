package ru.spbau.jvm.scala.calculator

import scala.util.control.Breaks._

import scala.io.StdIn

object Main {

  val unaryOps = List(
    '-' -> { (x: Double) => -x }
  )

  val outerBinOps = List(
    '+' -> { (x: Double, y: Double) => x + y },
    '-' -> { (x: Double, y: Double) => x - y }
  )

  val innerBinOps = List(
    '/' -> { (x: Double, y: Double) => x / y },
    '*' -> { (x: Double, y: Double) => x * y }
  )

  val functions = List(
    "log" -> { (x: Double) => Math.log(x) },
    "sin" -> { (x: Double) => Math.sin(x) },
    "cos" -> { (x: Double) => Math.cos(x) }
  )

  val unaryOpsRules: Interpreter.UnaryOpRules = unaryOps.map({ pr => (new Arithmetic(pr._1), pr._2) }).toMap
  val outerBinOpsRules: Interpreter.BinOpRules = binOpsToRules(outerBinOps)
  val innerBinOpsRules: Interpreter.BinOpRules = binOpsToRules(innerBinOps)
  val functionRules: Interpreter.FunctionRules = functions.map({ pr => (new Function(pr._1), pr._2) }).toMap

  private val lexer = new Lexer((outerBinOps ++ innerBinOps).map({ pr => pr._1 }), functions.map({ pr => pr._1 }))

  private def binOpsToRules(binOps: List[(Char, (Double, Double) => Double)]) =
    binOps.map({ pr => (new Arithmetic(pr._1), pr._2) }).toMap

  private val interpreter = new Interpreter(unaryOpsRules, List(outerBinOpsRules, innerBinOpsRules), functionRules)

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
