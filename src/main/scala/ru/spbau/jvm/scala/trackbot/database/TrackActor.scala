package ru.spbau.jvm.scala.trackbot.database

import akka.persistence.PersistentActor

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * @author bronti
  */
class TrackActor extends PersistentActor {
  import TrackActor._

  val map: mutable.HashMap[Long, ArrayBuffer[String]] = mutable.HashMap.empty

  def receiveEvent(event: Event): Unit = {
    event match {
      case SaveTrackNumber(id, track) => map.getOrElseUpdate(id, ArrayBuffer.empty) += track
    }
  }

  override def receiveRecover: Receive = {
    case evt: Event => receiveEvent(evt)
  }

  override def receiveCommand: Receive = {
    case evt: Event => persist(evt)(receiveEvent)
  }

  override def persistenceId = "trackbot-database"
}

object TrackActor {

  //events
  trait Event

  case class SaveTrackNumber(id: Long, track: String) extends Event

  //queries

}
