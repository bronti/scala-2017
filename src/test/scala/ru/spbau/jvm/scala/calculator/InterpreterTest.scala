package ru.spbau.jvm.scala.calculator

import org.junit.{Assert, Test}
import ru.spbau.jvm.scala.calculator.Main._

class InterpreterTest {

  def getInterpreter = new Interpreter(unaryOpsRules, List(outerBinOpsRules, innerBinOpsRules), functionRules)

  @Test
  def testSimple(): Unit = {
    Assert.assertEquals(8.0, getInterpreter.parse(List(
      new Number(8),
      new Arithmetic('+'),
      new Number(9),
      new Arithmetic('*'),
      new Function("log"),
      OPEN_PAREN,
      new Number(1),
      CLOSE_PAREN
    )), 1e-5)
  }

  @Test
  def testParens(): Unit = {
    Assert.assertEquals(1.0, getInterpreter.parse(List(
      OPEN_PAREN,
      new Number(1),
      CLOSE_PAREN
    )), 1e-5)
  }

  @Test
  def testPriority(): Unit = {
    Assert.assertEquals(118.0 + 2.0 * 15.0, getInterpreter.parse(List(
      new Number(118),
      new Arithmetic('+'),
      new Number(2),
      new Arithmetic('*'),
      new Number(15),
    )), 1e-5)
    Assert.assertEquals(118.0 * 2.0 + 15.0, getInterpreter.parse(List(
      new Number(118),
      new Arithmetic('*'),
      new Number(2),
      new Arithmetic('+'),
      new Number(15),
    )), 1e-5)
  }

  @Test
  def testAssoc(): Unit = {
    Assert.assertEquals(1.0 - 17.0 - 154.0, getInterpreter.parse(List(
      new Number(1),
      new Arithmetic('-'),
      new Number(17),
      new Arithmetic('-'),
      new Number(154),
    )), 1e-5)
  }

  @Test
  def testUnary(): Unit = {
    Assert.assertEquals(1.0 - -17.0 - (- (- (- 154.0))), getInterpreter.parse(List(
      new Number(1),
      new Arithmetic('-'),
      new Arithmetic('-'),
      new Number(17),
      new Arithmetic('-'),
      new Arithmetic('-'),
      new Arithmetic('-'),
      new Arithmetic('-'),
      new Number(154),
    )), 1e-5)
  }

  @Test
  def testFunction(): Unit = {
    Assert.assertEquals(1.0 - Math.log(17.0 - 154.0), getInterpreter.parse(List(
      new Number(1),
      new Arithmetic('-'),
      new Function("log"),
      OPEN_PAREN,
      new Number(17),
      new Arithmetic('-'),
      new Number(154),
      CLOSE_PAREN
    )), 1e-5)
  }

  @Test(expected = classOf[ParserException])
  def testIncorrectFunction(): Unit = {
    getInterpreter.parse(List(
      new Function("log"),
      new Number(17),
    ))
  }
}
