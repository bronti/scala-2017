package ru.spbau.jvm.scala.trackbot

import akka.actor.{ActorSystem, Props}
import com.typesafe.akka.extension.quartz.QuartzSchedulerExtension
import ru.spbau.jvm.scala.trackbot.bot.{CheckForUpdatesActor, CleanDatabaseActor, TrackBot}
import ru.spbau.jvm.scala.trackbot.database.DatabaseActor

import scala.io.Source

object Main extends App {
  // todo: from sbt props?
  lazy val token = Source.fromFile("bot.token").getLines().mkString

  val system = ActorSystem()
  val scheduler = QuartzSchedulerExtension(system)
  val database = system.actorOf(Props(classOf[DatabaseActor]))

  private val bot = new TrackBot(token, database)

  val checkForUpdatesActor = system.actorOf(Props(classOf[CheckForUpdatesActor], bot))
  val cleanDatabaseActor = system.actorOf(Props(classOf[CleanDatabaseActor], bot))

  // todo: set time
  // todo: timezone
  val checkForUpdatesTime = "0 0 14" // utc!!
  val checkForUpdatesName = "check for updates"

  scheduler.createSchedule(checkForUpdatesName, None, s"$checkForUpdatesTime ? * *")
  scheduler.schedule(checkForUpdatesName, checkForUpdatesActor, checkForUpdatesName)

  val cleanDatabaseNameTime = "0 0 11"
  val cleanDatabaseNameName = "clean database"

  scheduler.createSchedule(cleanDatabaseNameName, None, s"$cleanDatabaseNameTime ? * 1")
  scheduler.schedule(cleanDatabaseNameName, cleanDatabaseActor, cleanDatabaseNameName)

  bot.run()
}
