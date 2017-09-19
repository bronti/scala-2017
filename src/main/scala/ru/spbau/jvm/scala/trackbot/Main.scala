package ru.spbau.jvm.scala.trackbot

import akka.actor.{ActorSystem, Props}
import com.typesafe.akka.extension.quartz.QuartzSchedulerExtension
import ru.spbau.jvm.scala.trackbot.bot.TrackBot
import ru.spbau.jvm.scala.trackbot.database.TrackActor

import scala.io.Source

object Main extends App {
  lazy val token = Source.fromFile("bot.token").getLines().mkString

  val system = ActorSystem()
  val scheduler = QuartzSchedulerExtension(system)
  val database = system.actorOf(Props(classOf[TrackActor]))

  private val bot = new TrackBot(token, database)

  bot.run()
}
