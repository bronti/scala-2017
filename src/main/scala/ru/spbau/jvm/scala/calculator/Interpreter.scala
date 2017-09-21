package ru.spbau.jvm.scala.calculator

class ParserException(val msg: String) extends Exception

object Interpreter {
  type UnaryOpRules = Map[Arithmetic, (Double) => Double]
  type BinOpRules = Map[Arithmetic, (Double, Double) => Double]
  type FunctionRules = Map[Function, Double => Double]
}

class Interpreter(
                   private val unaryOpRules: Interpreter.UnaryOpRules,
                   private val binOpRules: List[Interpreter.BinOpRules],
                   private val functions: Interpreter.FunctionRules
            ) {

  def parse(tokens: List[Token]): Double = {
    val result = parseBlock(tokens, 0)
    if (result._2.isEmpty) result._1 else throw new ParserException("end of expression expected")
  }

  private def parseBlock(tail: List[Token], level: Int): (Double, List[Token]) = {
    if (tail.isEmpty) throw new ParserException("something expected")
    if (level >= binOpRules.length) return parseBinOpArgument(tail)

    val validBinOpRules = binOpRules(level)

    val (firstArgument, nextTail) = parseBlock(tail, level + 1)

    def isValidBinOp(t: Token): Boolean = validBinOpRules.keys.toList.contains(t)

    def parseRest(acc: Double, tail: List[Token]): (Double, List[Token]) = {
      if (tail.isEmpty || !isValidBinOp(tail.head)) return (acc, tail)
      val binOp = tail.head.asInstanceOf[Arithmetic]

      val (nextArgument, nextTail) = parseBlock(skipToken(tail), level + 1)

      val nextAcc = validBinOpRules(binOp)(acc, nextArgument)
      parseRest(nextAcc, nextTail)
    }

    parseRest(firstArgument, nextTail)
  }

  private def skipToken(tail: List[Token]): List[Token] = tail.tail

  private def parseBinOpArgument(tail: List[Token]): (Double, List[Token]) = {
    if (tail.isEmpty) throw new ParserException("something expected")
    tail.head match {
      case OPEN_PAREN => parseParenthesized(tail)
      case CLOSE_PAREN => throw new ParserException("unexpected closing paren")
      case number: Number => (number.value.toDouble, skipToken(tail))
      case function: Function =>
        val (result, nextTail) = parseParenthesized(skipToken(tail))
        (functions(function)(result), nextTail)

      case ar: Arithmetic if unaryOpRules.keys.toList.contains(ar) =>
        val (result, nextTail) = parseBinOpArgument(skipToken(tail))
        (unaryOpRules(ar)(result), nextTail)

      case _ => throw new IllegalStateException()
    }
  }

  private def parseParenthesized(tail: List[Token]): (Double, List[Token]) = {
    if (tail.isEmpty || tail.head != OPEN_PAREN) throw new ParserException("closing paren expected")
    val (result, intermediateTail) = parseBlock(skipToken(tail), 0)
    val newTail = parseClosingParen(intermediateTail)
    (result, newTail)
  }

  private def parseClosingParen(tail: List[Token]): List[Token] = {
    if (tail.isEmpty || tail.head != CLOSE_PAREN) throw new ParserException("closing paren expected")
    skipToken(tail)
  }
}
