package ru.spbau.jvm.scala.calculator

import org.junit.{Assert, Test}
import ru.spbau.jvm.scala.calculator.Main.{unaryOpsRules, functionRules, innerBinOpsRules, outerBinOpsRules}

class ArithmeticsAndFunctionTest {

  def getInterpreter = new Interpreter(unaryOpsRules, List(outerBinOpsRules, innerBinOpsRules), functionRules)

  @Test
  def testSum(): Unit = {
    Assert.assertEquals(8 + 9, getInterpreter.parse(List(
      new Number(8),
      new Arithmetic('+'),
      new Number(9)
    )), 1e-5)
  }

  @Test
  def testDiff(): Unit = {
    Assert.assertEquals(8 - 9, getInterpreter.parse(List(
      new Number(8),
      new Arithmetic('-'),
      new Number(9)
    )), 1e-5)
  }

  @Test
  def testMult(): Unit = {
    Assert.assertEquals(8 * 9, getInterpreter.parse(List(
      new Number(8),
      new Arithmetic('*'),
      new Number(9)
    )), 1e-5)
  }

  @Test
  def testDiv(): Unit = {
    Assert.assertEquals(8.0 / 9.0, getInterpreter.parse(List(
      new Number(8),
      new Arithmetic('/'),
      new Number(9)
    )), 1e-5)
  }

  @Test
  def testUnaryMinus(): Unit = {
    Assert.assertEquals(- 9.0, getInterpreter.parse(List(
      new Arithmetic('-'),
      new Number(9)
    )), 1e-5)
  }

  @Test
  def testLog(): Unit = {
    Assert.assertEquals(Math.log(8.0), getInterpreter.parse(List(
      new Function("log"),
      OPEN_PAREN,
      new Number(8),
      CLOSE_PAREN
    )), 1e-5)
  }

  @Test
  def testSin(): Unit = {
    Assert.assertEquals(Math.sin(8), getInterpreter.parse(List(
      new Function("sin"),
      OPEN_PAREN,
      new Number(8),
      CLOSE_PAREN
    )), 1e-5)
  }

  @Test
  def testCos(): Unit = {
    Assert.assertEquals(Math.cos(8.0), getInterpreter.parse(List(
      new Function("cos"),
      OPEN_PAREN,
      new Number(8),
      CLOSE_PAREN
    )), 1e-5)
  }

}
