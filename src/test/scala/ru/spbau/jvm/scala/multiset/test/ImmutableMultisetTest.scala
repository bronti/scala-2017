package ru.spbau.jvm.scala.multiset.test

import org.junit.{Assert, Test}
import ru.spbau.jvm.scala.multiset.ImmutableMultiSet

import scala.collection.mutable

class ImmutableMultisetTest {

  @Test
  def findTest(): Unit = {
    Assert.assertTrue(ImmutableMultiSet(1, 2, 3).find(1).isDefined)
    Assert.assertTrue(ImmutableMultiSet(1, 2, 2).find(2).isDefined)
    Assert.assertFalse(ImmutableMultiSet(1, 2, 3).find(0).isDefined)
    Assert.assertFalse(ImmutableMultiSet().find(1).isDefined)
  }

  @Test
  def filterTest(): Unit = {
    Assert.assertTrue(ImmutableMultiSet(1, 2, 2).filter { _ == 1 }.find(1).isDefined)
    Assert.assertEquals(1, ImmutableMultiSet(1, 2, 2).filter { _ == 1 }.size)
  }

  @Test
  def mapTest(): Unit = {
    Assert.assertTrue(ImmutableMultiSet(1, 2, 3).map { _ + 1}.find(4).isDefined)
    Assert.assertTrue(ImmutableMultiSet(1, 2, 3).map { _ + 1}.find(1).isEmpty)
    Assert.assertEquals(3, ImmutableMultiSet(1, 2, 3).map { _ + 1}.size)
  }

  @Test
  def forTest(): Unit = {
    val res = mutable.Set[Int]()
    for (e <- ImmutableMultiSet(1, 2, 3)) {
      res.add(e)
    }
    Assert.assertEquals(mutable.Set(1, 2, 3), res)
  }

  @Test
  def matchTest(): Unit = {
    ImmutableMultiSet(1, 2, 3) match {
      case ImmutableMultiSet() => Assert.fail()
      case ImmutableMultiSet(1, 2) => Assert.fail()
      case ImmutableMultiSet(1, 2, 3, 4) => Assert.fail()
      case ImmutableMultiSet(1, 2, 3) =>
    }
  }

}
