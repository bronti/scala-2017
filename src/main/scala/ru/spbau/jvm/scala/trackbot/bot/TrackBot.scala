package ru.spbau.jvm.scala.trackbot.bot

import akka.actor.ActorRef
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import ru.spbau.jvm.scala.trackbot.parser.MessageParser
import ru.spbau.jvm.scala.trackbot.parser.messages._

/**
  * @author bronti
  */
class TrackBot(val token: String, val database: ActorRef) extends TelegramBot with Polling with Commands {

  onMessage {
    implicit message =>
      message.text.foreach { text =>
        MessageParser.parse(text) match {
          case AddTrackNumber(track) =>
            reply("Трекинговый номер не добавлен...")
          case WrongMessage =>
            reply("Неверная команда :(")
        }
      }
  }
}
