package ru.spbau.jvm.scala.calculator

class LexerException(val msg: String) extends Exception

class Lexer(private val possibleArithmetics: List[Char], private val possibleFunctions: List[String]) {

  def tokenize(input: String): List[Token] = tokenizeTail(List(), null, input)

  private[this] sealed abstract class LexerBuffer {
    def accepts(c: Char): Boolean

    def extendedBy(c: Char): LexerBuffer

    def toToken: Token
  }

  private[this] class NumberBuffer(val value: String) extends LexerBuffer {
    def this(c: Char) = this(c.toString)

    override def accepts(c: Char): Boolean = c.isDigit

    override def extendedBy(c: Char) = new NumberBuffer(value + c)

    override def toToken: Token = new Number(value.toInt)
  }

  private[this] class FunctionBuffer(val value: String) extends LexerBuffer {
    def this(c: Char) = this(c.toString)

    override def accepts(c: Char): Boolean = c.isLetter

    override def extendedBy(c: Char) = new FunctionBuffer(value + c)

    override def toToken: Token =
      if (possibleFunctions.contains(value)) new Function(value)
      else throw  new LexerException("incorrect function name")
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
        (null, buffer.extendedBy(tail(0)))
      }
      else nextChar match {
        case '(' => (OPEN_PAREN, null)
        case ')' => (CLOSE_PAREN, null)
        case c if c.isWhitespace => (null, null)
        case c if c.isLetter => (null, new FunctionBuffer(c))
        case c if c.isDigit => (null, new NumberBuffer(c))
        case c if possibleArithmetics.contains(c) => (new Arithmetic(c), null)
        case _ => throw new LexerException("unexpected symbol")
      }

    val additionalTokens = List(flushedBuffer, newToken).filterNot({ t => t == null })
    val newPreviousTokens = previousTokens ++ additionalTokens

    tokenizeTail(newPreviousTokens, newBuffer, newTail)
  }
}
