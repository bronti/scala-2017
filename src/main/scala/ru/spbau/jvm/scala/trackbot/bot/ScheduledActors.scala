package ru.spbau.jvm.scala.trackbot.bot

import akka.actor.Actor

class CheckForUpdatesActor(bot: TrackBot) extends Actor {
  override def receive: Actor.Receive = {
    case _ => bot.checkForUpdates()
  }
}

class CleanDatabaseActor(bot: TrackBot) extends Actor {
  override def receive: Actor.Receive = {
    case _ => bot.cleanDatabase()
  }
}
