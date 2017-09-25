package ru.spbau.jvm.scala.trackbot.tracker

import java.lang.reflect.Type

import com.google.gson.{Gson, GsonBuilder}
import org.apache.http.HttpResponse
import org.apache.http.client.fluent.Request
import org.apache.http.client.utils.URIBuilder
import org.apache.http.entity.ContentType

import scala.io.Source

object Track24 {

  private val gsonBuilder = new GsonBuilder
  gsonBuilder.registerTypeAdapter(classOf[TrackResponse], TrackResponse)
  private val gson = gsonBuilder.create

  sealed abstract class TrackingResult

  case class RequestFailed(statusCode: Int, reason: String) extends TrackingResult

  case object ResponseInvalid extends TrackingResult

  case class JSONResponse(content: TrackResponse) extends TrackingResult

  val apiKey: String = Source.fromFile("track.api.key").getLines().mkString

  def getTrackInfo(code: String): TrackingResult = {
    val uri = new URIBuilder("https://api.track24.ru/tracking.json.php")
      .addParameter("apiKey", apiKey)
      .addParameter("domain", "bronti.net")
      .addParameter("code", code)
      .build()
    Request.Get(uri).execute().handleResponse((response: HttpResponse) => {
        val statusLine = response.getStatusLine
        val entity = response.getEntity
        if (statusLine.getStatusCode >= 300) RequestFailed(statusLine.getStatusCode, statusLine.getReasonPhrase)
        else if (entity == null) ResponseInvalid
        else try {
          val contentType = ContentType.getOrDefault(entity)
          if (contentType.getMimeType != ContentType.APPLICATION_JSON.getMimeType) ResponseInvalid
          else {
            val responseString = scala.io.Source.fromInputStream(entity.getContent).getLines().mkString
            val response = gson.fromJson[TrackResponse](responseString, classOf[TrackResponse])
            JSONResponse(response)
          }
        } catch {
          case _ : Exception => ResponseInvalid
        }
    })
  }
}
