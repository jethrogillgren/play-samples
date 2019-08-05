package models

import java.util.NoSuchElementException

import models.MatchFilterType.MatchFilterType
import play.api.data.FormError
import play.api.libs.json._
import play.api.data.format.Formats._
import play.api.data.format.Formatter

object MatchFilterType extends Enumeration {
  type MatchFilterType = Value
  val ONE /*1*/ , TWO /*2*/ , ETC /*n*/ = Value

  implicit object MatchFilterTypeFormatter extends Formatter[MatchFilterType.Value] {
    override val format = Some(("format.enum", Nil))
    override def bind(key: String, data: Map[String, String]) = {
      try {
        Right(MatchFilterType.withName(data.get(key).head))
      } catch {
        case e:NoSuchElementException =>  Left(Seq(play.api.data.FormError(key, "Invalid MatchFilterType Enumeration")))
      }
    }
    override def unbind(key: String, value: MatchFilterType.Value)              = {
      Map(key -> value.toString)
    }
  }
  implicit val matchFilterTypeFormat = new Format[MatchFilterType.MatchFilterType] {
    def reads(json: JsValue) = JsSuccess(MatchFilterType.withName(json.as[String]))
    def writes(myEnum: MatchFilterType.MatchFilterType) = JsString(myEnum.toString)
  }
}

case class SearchRequest(mft: MatchFilterType, queryText: String, locations: List[String])


object SearchRequest {
  implicit val searchRequestFormat = Json.format[SearchRequest]
}