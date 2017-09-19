package ru.spbau.jvm.scala.trackbot.parser

import ru.spbau.jvm.scala.trackbot.parser.messages._
import ru.spbau.jvm.scala.trackbot.parser.messages.{UserMessage, WrongMessage}
import scala.util.parsing.combinator.RegexParsers

/**
  * @author bronti
  */
class MessageParser extends RegexParsers {
  override def skipWhitespace = true

  private val wordParser: Parser[String] = raw"\S+".r

  private val addTrackNumber: Parser[AddTrackNumber] = "[Оо]тслеживай".r ~> wordParser ^^ AddTrackNumber

  val userMessage: Parser[UserMessage] = addTrackNumber
}

object MessageParser extends MessageParser {
  def parse(text: String): UserMessage = {
    parse(userMessage, text) match {
      case Success(message, _) => message
      case _ => WrongMessage
    }
  }
}
