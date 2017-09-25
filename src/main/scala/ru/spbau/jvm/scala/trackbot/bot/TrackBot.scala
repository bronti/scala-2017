package ru.spbau.jvm.scala.trackbot.bot

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import info.mukel.telegrambot4s.methods.{ParseMode, SendMessage}
import info.mukel.telegrambot4s.models.ChatId.Chat
import info.mukel.telegrambot4s.models.Message
import ru.spbau.jvm.scala.trackbot.database.DatabaseActor._
import ru.spbau.jvm.scala.trackbot.parser.MessageParser
import ru.spbau.jvm.scala.trackbot.parser.messages._
import ru.spbau.jvm.scala.trackbot.tracker.Track24.{JSONResponse, RequestFailed, ResponseInvalid}
import ru.spbau.jvm.scala.trackbot.tracker._

import scala.concurrent.duration.DurationInt
import scala.util.Success

/**
  * @author bronti
  */
class TrackBot(val token: String, val database: ActorRef) extends TelegramBot with Polling with Commands {

  implicit val timeout: Timeout = Timeout(15.second)

  def checkForUpdates(): Unit = doForAllStoredTracksWithUpdatedData { (id, track, lastEventTime, data) =>
    val isDelivered = data.deliveredStatus == 1

    lastEventTime match {
      case Some(time) => if (time == data.lastPoint.eventDateTime) return
      case _ =>
    }

    reportTrackStatus(Chat(id), track, data)
    if (!isDelivered && lastEventTime.isDefined) database ! SaveTrack(id, track, data.lastPoint.eventDateTime)
    else if (isDelivered) database ! SaveDeliveredTrack(id, track)
    else database ! SaveUnidentifiedTrack(id, track)
  }

  def cleanDatabase(): Unit = { database ! CleanTracks }

  onMessage {
    implicit message =>
      message.text.foreach { text =>
        MessageParser.parse(text) match {
          case AddTrackNumber(track) => handleTrackAddition(track, verboseDatabase = true)
          case ShowAllTracks =>
            (database ? GetAllTracks(message.chat.id)).onComplete {
              case Success(AllTracks(tracks)) =>
                tracks.foreach { track => handleTrackAddition(track, verboseDatabase = false) }
              case _ => replyWithFormatting("Ошибка базы данных :(")
            }
          case WrongMessage => replyWithFormatting("Неверная команда :(")
        }
      }
  }

  private def handleTrackAddition(track: String, verboseDatabase: Boolean)(implicit message: Message) = {
    Track24.getTrackInfo(track) match {
      case ResponseInvalid => replyWithFormatting(s"Некорректный ответ сайта для трека *$track* Оо")
      case RequestFailed(code, reason) => replyWithFormatting(s"Не достучаться до сайта для трека *$track*. Код: $code, причина: $reason")
      case JSONResponse(content) => checkedAddTrackReportStatusAndReply(track, content, verboseDatabase)
    }
  }

  private def checkedAddTrackReportStatusAndReply(track: String, content: TrackResponse, verboseDatabase: Boolean)(implicit message: Message) = {
    content match {
      case ErrorMessageResponse(status, msg) =>
        replyWithFormatting(s"Сайт ругается на *$track*. Статус: $status, сообщение: $msg.")
        if (verboseDatabase) replyTrackNotAdded(track)
      case TrackDataResponse(data) =>
        reportTrackStatus(message.source, track, data)
        if (data.deliveredStatus == 0) {
          checkedAddTrackAndReply(track, Some(data), verboseDatabase)
        }
        else if (verboseDatabase) replyTrackNotAdded(track)
      case EmptyResponse =>
        replyWithFormatting(s"Сайт не нашел трекингового номера *$track*. Может посылка еще не отправлена?")
        checkedAddTrackAndReply(track, None, verboseDatabase)
    }
  }

  private def checkedAddTrackAndReply(track: String, trackData: Option[ResponseData], verboseDatabase: Boolean)(implicit message: Message): Unit = {
    (database ? CheckTrackIsStored(message.chat.id, track)).onComplete {
      case Success(TrackIsStored(true)) => if (verboseDatabase) replyTrackAlreadyStored(track)
      case Success(TrackIsStored(false)) =>
        trackData match {
          case None => database ! SaveUnidentifiedTrack(message.chat.id, track)
          case Some(data) => database ! SaveTrack(message.chat.id, track, data.lastPoint.eventDateTime)
        }
        if (verboseDatabase) replyTrackAdded(track)
      case _ => if (verboseDatabase) replyDatabaseError(track)
    }
  }

  private def reportTrackStatus(chat: Chat, track: String, data: ResponseData): Unit = {
    val location = data.lastPoint
    data.deliveredStatus match {
      case 0 =>
        sendMessageWithFormatting(chat, s"Последний раз посылка *$track* была замечена в ${location.operationPlaceName}.\n" +
          s"Время: ${location.eventDateTime}. Атрибут: ${location.operationAttribute}.")
      case 1 =>
        sendMessageWithFormatting(chat, s"Посылка *$track* уже доставлена в ${location.operationPlaceName}." +
          s"Время: ${location.eventDateTime}. Атрибут: ${location.operationAttribute}.")
      case _ =>
        sendMessageWithFormatting(chat, s"Неизвестный deliveryStatus трека *$track*...")
    }
  }

  private def doForAllStoredTracksWithUpdatedData(handleTrack: (Long, String, Option[String], ResponseData) => Unit): Unit
  = doForAllStoredTracks { (id, track) =>
    Track24.getTrackInfo(track) match {
      case JSONResponse(content) =>
        content match {
          case TrackDataResponse(data) =>
            (database ? GetTrackLastEventTime(id, track)).onComplete {
              case Success(TrackLastEventTime(time)) => handleTrack(id, track, time, data)
              case _ =>
            }
          case _ =>
        }
      case _ =>
    }
  }

  private def doForAllStoredTracks(handleTrack: (Long, String) => Unit): Unit = doForAllIds { id =>
    (database ? GetAllTracks(id)).onComplete {
      case Success(AllTracks(tracks)) => tracks.foreach { track => handleTrack(id, track) }
      case _ =>
    }
  }

  private def doForAllIds(handleId: (Long) => Unit): Unit = {
    (database ? GetAllIds).onComplete {
      case Success(AllIds(ids)) => ids.foreach(handleId)
      case _ =>
    }
  }

  private def replyTrackAdded(track: String)(implicit message: Message) = replyWithFormatting(s"Трекинговый номер *$track* добавлен в базу.")

  private def replyTrackAlreadyStored(track: String)(implicit message: Message) = replyWithFormatting(s"Трекинговый номер *$track* уже есть в базе.")

  private def replyTrackNotAdded(track: String)(implicit message: Message) = replyWithFormatting(s"Трекинговый номер *$track* не добавлен в базу.")

  private def replyDatabaseError(track: String)(implicit message: Message) = replyWithFormatting(s"При обработке номера *$track* произошла ошибка базы данных, сорян :( ")

  private def sendMessageWithFormatting(chat: Chat, text: String): Unit = {
    request(SendMessage(chat, text, Some(ParseMode.Markdown), None, None, None, None))
  }

  private def replyWithFormatting(text: String)(implicit message: Message) = reply(text, Some(ParseMode.Markdown))

}
