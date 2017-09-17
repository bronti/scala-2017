package ru.spbau.jvm.scala.calculator

sealed abstract class Token

case object OPEN_PAREN extends Token
case object CLOSE_PAREN extends Token

case class Number(value: Int) extends Token
case class Function(value: String) extends Token
case class Arithmetic(value: Char) extends Token