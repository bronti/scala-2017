package ru.spbau.jvm.scala.calculator

class LexerException(val msg: String) extends Exception

class Lexer(private val possibleArithmetics: Set[Arithmetic], private val possibleFunctions: Set[Function]) {

  def tokenize(input: String): List[Token] = tokenizeTail(List(), null, input)

  private[this] sealed abstract class LexerBuffer(protected var value: String) {
    def accepts(c: Char): Boolean
    def extendBy(c: Char): LexerBuffer = {
      value += c
      this
    }
    def toToken: Token
  }

  private[this] class NumberBuffer(c: Char) extends LexerBuffer(c.toString) {
    override def accepts(c: Char): Boolean = c.isDigit
    override def toToken: Token = Number(value.toInt)
  }

  private[this] class FunctionBuffer(c: Char) extends LexerBuffer(c.toString) {
    override def accepts(c: Char): Boolean = c.isLetter
    override def toToken: Token = {
      val function = Function(value)
      if (possibleFunctions.contains(function)) function
      else throw new LexerException("incorrect function name")
    }
  }

  private[this] def tokenizeTail(previousTokens: List[Token],
                                 buffer: LexerBuffer,
                                 tail: String): List[Token] = {
    if (tail.isEmpty) {
      return if (buffer != null) previousTokens ++ List(buffer.toToken)
      else previousTokens
    }
    val nextChar = tail(0)
    val newTail = tail.substring(1)
    val flushedBuffer = if (buffer != null && !buffer.accepts(nextChar)) buffer.toToken else null

    val (newToken, newBuffer) =
      if (buffer != null && buffer.accepts(nextChar)) {
        (null, buffer.extendBy(tail(0)))
      }
      else nextChar match {
        case '(' => (OPEN_PAREN, null)
        case ')' => (CLOSE_PAREN, null)
        case c if possibleArithmetics.contains(Arithmetic(c)) => (Arithmetic(c), null)
        case c if c.isWhitespace => (null, null)
        case c if c.isLetter => (null, new FunctionBuffer(c))
        case c if c.isDigit => (null, new NumberBuffer(c))
        case _ => throw new LexerException("unexpected symbol")
      }

    val additionalTokens = List(flushedBuffer, newToken).filterNot({ t => t == null })
    val newPreviousTokens = previousTokens ++ additionalTokens

    tokenizeTail(newPreviousTokens, newBuffer, newTail)
  }
}
