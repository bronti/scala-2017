package ru.spbau.jvm.scala.trackbot.parser.messages

/**
  * @author bronti
  */
trait UserMessage

case class AddTrackNumber(track: String) extends UserMessage

case object WrongMessage extends UserMessage
