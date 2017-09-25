package ru.spbau.jvm.scala.trackbot.tracker

import java.lang.reflect.Type

import com.google.gson._

case class Location(
                   id : Int,
                   eventDateTime : String,
                   operationPlaceName : String,
                   operationAttribute : String,
                   operationType : String
                   )

case class ResponseData(
                         trackUpdateDateTime : String,
                         deliveredStatus : Int,
                         lastPoint : Location
                       )

trait TrackResponse
case class TrackDataResponse(data : ResponseData) extends TrackResponse
case class ErrorMessageResponse(status : String, message : String) extends TrackResponse
case object EmptyResponse extends TrackResponse

object TrackResponse extends JsonDeserializer[TrackResponse] {
  @throws[JsonParseException]
  def deserialize (json: JsonElement, jsonType: Type, context: JsonDeserializationContext): TrackResponse = {
    val jobject = json.getAsJsonObject
    val status = jobject.get("status").getAsString
    if (status != "ok") ErrorMessageResponse(status, jobject.get("message").getAsString)
    else {
      val data = jobject.get("data").getAsJsonObject
      val lastPoint = data.get("lastPoint")
      if (lastPoint.isJsonPrimitive && lastPoint.getAsString == "")
        EmptyResponse
      else {
        val lastPointObject = lastPoint.getAsJsonObject
        val locationData = Location(
          lastPointObject.get("id").getAsInt,
          lastPointObject.get("eventDateTime").getAsString,
          lastPointObject.get("operationPlaceName").getAsString,
          lastPointObject.get("operationAttribute").getAsString,
          lastPointObject.get("operationType").getAsString
        )
        val responseData = ResponseData(
          data.get("trackUpdateDateTime").getAsString: String,
          data.get("deliveredStatus").getAsInt: Int,
          locationData
        )
        TrackDataResponse(responseData)
      }
    }
  }
}