package ru.spbau.jvm.scala.trackbot.database

import akka.persistence.PersistentActor

import scala.collection.mutable

/**
  * @author bronti
  */
class DatabaseActor extends PersistentActor {
  import DatabaseActor._

  val activeTracksMap: mutable.HashMap[Long, mutable.HashMap[String, Option[String]]] = mutable.HashMap.empty
  val toBeDeletedTracksMap: mutable.HashMap[Long, mutable.HashMap[String, Boolean]] = mutable.HashMap.empty

  private def activeTracks(id: Long) = activeTracksMap.getOrElseUpdate(id, new mutable.HashMap)
  private def toBeDeletedTracks(id: Long) = toBeDeletedTracksMap.getOrElseUpdate(id, new mutable.HashMap)

  def receiveEvent(event: Event): Unit = {
    event match {
      case SaveTrack(id, track, time) => activeTracks(id)(track) = Some(time)
      case SaveUnidentifiedTrack(id, track) =>
        activeTracks(id) += track -> None
        toBeDeletedTracks(id)(track) = false
      case SaveDeliveredTrack(id, track) =>
        if (!activeTracks(id).contains(track)) {
          activeTracks(id)(track) = None
        }
        toBeDeletedTracks(id)(track) = false
      case CleanTracks =>
        for (id <- toBeDeletedTracksMap.keys) {
          val toDelete = toBeDeletedTracks(id).filter { _._2 }.keys.toList
          // delete marked tracks
          toDelete.foreach(activeTracks(id).remove)
          toDelete.foreach(toBeDeletedTracks(id).remove)
          // mark rest to be deleted on next cleanup
          toBeDeletedTracks(id).keys.foreach { toBeDeletedTracks(id)(_) = true }
          // clean ids without tracks
          if (activeTracks(id).isEmpty) activeTracksMap.remove(id)
          if (toBeDeletedTracks(id).isEmpty) toBeDeletedTracksMap.remove(id)
        }
    }
  }

  def receiveQuery(query: Query): QueryResult = {
    query match {
      case GetAllTracks(id: Long) => AllTracks(activeTracks(id).keys.toList)
      case GetTrackLastEventTime(id: Long, track: String) => TrackLastEventTime(activeTracks(id).getOrElse(track, None))
      case CheckTrackIsStored(id: Long, track: String) => TrackIsStored(activeTracks(id).contains(track))
      case GetAllIds => AllIds(activeTracksMap.keys.toList)
    }
  }

  override def receiveRecover: Receive = {
    case evt: Event => receiveEvent(evt)
  }

  override def receiveCommand: Receive = {
//    case evt: Event => persist(evt)(receiveEvent) //todo:
    case evt: Event => receiveEvent(evt)
    case que: Query => sender ! receiveQuery(que)
  }

  override def persistenceId = "track-bot-database"
}

object DatabaseActor {

  trait Event

  case class SaveTrack(id: Long, track: String, lastEventTime: String) extends Event
  case class SaveUnidentifiedTrack(id: Long, track: String) extends Event
  case class SaveDeliveredTrack(id: Long, track: String) extends Event

  case object CleanTracks extends Event

  trait Query

  case class GetTrackLastEventTime(id: Long, track: String) extends Query
  case class GetAllTracks(id: Long) extends Query
  case object GetAllIds extends Query
  case class CheckTrackIsStored(id: Long, track: String) extends Query

  trait QueryResult

  case class TrackLastEventTime(time: Option[String]) extends QueryResult
  case class AllTracks(tracks: List[String]) extends QueryResult
  case class AllIds(ids: List[Long]) extends QueryResult
  case class TrackIsStored(result: Boolean) extends QueryResult
}
