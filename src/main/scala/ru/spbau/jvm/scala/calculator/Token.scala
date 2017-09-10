package ru.spbau.jvm.scala.calculator

sealed abstract class Token

case object OPEN_PAREN extends Token
case object CLOSE_PAREN extends Token

class Number(val value: Int) extends Token {
  override def equals(o: scala.Any): Boolean = o != null && o.isInstanceOf[Number] && (o.asInstanceOf[Number].value == value)
}

class Function(val value: String) extends Token {
  override def equals(o: scala.Any): Boolean = o != null && o.isInstanceOf[Function] && (o.asInstanceOf[Function].value == value)
}

class Arithmetic(val value: Char) extends Token {
  override def equals(o: scala.Any): Boolean = o != null && o.isInstanceOf[Arithmetic] && (o.asInstanceOf[Arithmetic].value == value)
}