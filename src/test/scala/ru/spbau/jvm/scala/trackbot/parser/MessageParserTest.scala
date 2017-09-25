package ru.spbau.jvm.scala.trackbot.parser

import org.scalatest.FunSuite
import ru.spbau.jvm.scala.trackbot.parser.messages.{AddTrackNumber, ShowAllTracks, WrongMessage}

class MessageParserTest  extends FunSuite {

  test("Добавить трек") {
    assertResult(AddTrackNumber("239")) {
      MessageParser.parse("Отслеживай 239")
    }
  }

  test("Добавить трек c маленькой буквы") {
    assertResult(AddTrackNumber("239")) {
      MessageParser.parse("отслеживай 239")
    }
  }

  test("Покажи мои треки") {
    assertResult(ShowAllTracks) {
      MessageParser.parse("Покажи мои треки")
    }
  }

  test("покажи мои треки") {
    assertResult(ShowAllTracks) {
      MessageParser.parse("покажи мои треки")
    }
  }

  test("Ошибка") {
    assertResult(WrongMessage) {
      MessageParser.parse("ОТСлежива 666")
    }
  }
}

