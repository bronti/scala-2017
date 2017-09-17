package ru.spbau.jvm.scala.calculator

import org.junit.{Assert, Test}
import ru.spbau.jvm.scala.calculator.Main._

class InterpreterTest {

  def getInterpreter = new Interpreter(unaryOps, List(outerBinOps, innerBinOps), functions)

  @Test
  def testSimple(): Unit = {
    Assert.assertEquals(8.0, getInterpreter.parse(List(
      Number(8),
      Arithmetic('+'),
      Number(9),
      Arithmetic('*'),
      Function("log"),
      OPEN_PAREN,
      Number(1),
      CLOSE_PAREN
    )), 1e-5)
  }

  @Test
  def testParens(): Unit = {
    Assert.assertEquals(1.0, getInterpreter.parse(List(
      OPEN_PAREN,
      Number(1),
      CLOSE_PAREN
    )), 1e-5)
  }

  @Test
  def testPriority(): Unit = {
    Assert.assertEquals(118.0 + 2.0 * 15.0, getInterpreter.parse(List(
      Number(118),
      Arithmetic('+'),
      Number(2),
      Arithmetic('*'),
      Number(15),
    )), 1e-5)
    Assert.assertEquals(118.0 * 2.0 + 15.0, getInterpreter.parse(List(
      Number(118),
      Arithmetic('*'),
      Number(2),
      Arithmetic('+'),
      Number(15),
    )), 1e-5)
  }

  @Test
  def testAssoc(): Unit = {
    Assert.assertEquals(1.0 - 17.0 - 154.0, getInterpreter.parse(List(
      Number(1),
      Arithmetic('-'),
      Number(17),
      Arithmetic('-'),
      Number(154),
    )), 1e-5)
  }

  @Test
  def testUnary(): Unit = {
    Assert.assertEquals(1.0 - -17.0 - (- (- (- 154.0))), getInterpreter.parse(List(
      Number(1),
      Arithmetic('-'),
      Arithmetic('-'),
      Number(17),
      Arithmetic('-'),
      Arithmetic('-'),
      Arithmetic('-'),
      Arithmetic('-'),
      Number(154),
    )), 1e-5)
  }

  @Test
  def testFunction(): Unit = {
    Assert.assertEquals(1.0 - Math.log(17.0 - 154.0), getInterpreter.parse(List(
      Number(1),
      Arithmetic('-'),
      Function("log"),
      OPEN_PAREN,
      Number(17),
      Arithmetic('-'),
      Number(154),
      CLOSE_PAREN
    )), 1e-5)
  }

  @Test(expected = classOf[ParserException])
  def testIncorrectFunction(): Unit = {
    getInterpreter.parse(List(
      Function("log"),
      Number(17),
    ))
  }
}
