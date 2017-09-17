package ru.spbau.jvm.scala.calculator

import org.junit.{Assert, Test}
import ru.spbau.jvm.scala.calculator.Main._

class LexerTest {

  def getLexer = new Lexer(outerBinOps.keySet ++ innerBinOps.keySet ++ unaryOps.keySet, functions.keySet)

  @Test
  def testSimple(): Unit = {
    Assert.assertTrue(getLexer.tokenize("8 + 8 * log(5 / (12))").length == 12)
  }

  @Test
  def testNumber(): Unit = {
    val result = getLexer.tokenize("8")
    Assert.assertTrue(result.length == 1)
    Assert.assertTrue(result.head == Number(8))
  }

  @Test
  def testFunction(): Unit = {
    val result = getLexer.tokenize("log")
    Assert.assertTrue(result.length == 1)
    Assert.assertTrue(result.head == Function("log"))
  }

  @Test
  def testParens(): Unit = {
    val resultOpen = getLexer.tokenize("(")
    Assert.assertTrue(resultOpen.length == 1)
    Assert.assertTrue(resultOpen.head == OPEN_PAREN)
    val resultClose = getLexer.tokenize(")")
    Assert.assertTrue(resultClose.length == 1)
    Assert.assertTrue(resultClose.head == CLOSE_PAREN)
  }

  @Test
  def testArithmetic(): Unit = {
    val result = getLexer.tokenize("+")
    Assert.assertTrue(result.length == 1)
    Assert.assertTrue(result.head == Arithmetic('+'))
  }

  @Test(expected = classOf[LexerException])
  def testIncorrectArithmetic(): Unit = {
    getLexer.tokenize("&")
  }

  @Test(expected = classOf[LexerException])
  def testIncorrectFunction(): Unit = {
    getLexer.tokenize("foo")
  }

  @Test
  def testNumberBuffer(): Unit = {
    val result = getLexer.tokenize("123")
    Assert.assertTrue(result.length == 1)
    Assert.assertTrue(result.head == Number(123))
  }

  @Test
  def testBufferBetweenTokens(): Unit = {
    val result = getLexer.tokenize("+123-")
    Assert.assertTrue(result.length == 3)
    Assert.assertTrue(result.head == Arithmetic('+'))
    Assert.assertTrue(result(1) == Number(123))
    Assert.assertTrue(result(2) == Arithmetic('-'))
  }

  @Test
  def testWhitespaces(): Unit = {
    val result = getLexer.tokenize(" +      123" +
      "  - ")
    Assert.assertTrue(result.length == 3)
    Assert.assertTrue(result.head == Arithmetic('+'))
    Assert.assertTrue(result(1) == Number(123))
    Assert.assertTrue(result(2) == Arithmetic('-'))
  }
}
