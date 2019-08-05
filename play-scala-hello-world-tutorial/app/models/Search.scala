package models

import models.MatchFilterType.{MatchFilterType, MatchFilterTypeFormatter}
import play.api.data.FormError
import play.api.libs.json._
import play.api.data.format.Formats._
import play.api.data.format.Formatter
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._ // Combinator syntax

object MatchFilterType extends Enumeration {
  type MatchFilterType = Value
  val ONE /*1*/ , TWO /*2*/ , ETC /*n*/ = Value

  implicit def reads[E <: Enumeration](enum: E): Reads[E#Value] = new Reads[E#Value] {
    def reads(json: JsValue): JsResult[E#Value] = json match {
      case JsNumber(s) =>
        try {
          JsSuccess(enum.apply(s.toInt))
        } catch {
          case _: NoSuchElementException => JsError(s"Enumeration expected of type: '${enum.getClass}', but it does not appear to contain the value: '$s'")
        }
      case _ => JsError("Number value expected")
    }
  }

  implicit def writes[E <: Enumeration]: Writes[E#Value] = new Writes[E#Value] {
    def writes(v: E#Value): JsValue = JsNumber(v.id)
  }

  implicit def formatID[E <: Enumeration](enum: E): Format[E#Value] =
    Format(reads(enum), writes)


  def readsString[E <: Enumeration](enum: E): Reads[E#Value] = new Reads[E#Value] {
    def reads(json: JsValue): JsResult[E#Value] = json match {
      case JsString(s) => {
        try {
          JsSuccess(enum.withName(s))
        } catch {
          case _: NoSuchElementException => JsError(s"Enumeration expected of type: '${enum.getClass}', but it does not appear to contain the value: '$s'")
        }
      }
      case _ => JsError("String value expected")
    }
  }

  implicit def writesString[E <: Enumeration]: Writes[E#Value] = new Writes[E#Value] {
    def writes(v: E#Value): JsValue = JsString(v.toString)
  }

  implicit def formatString[E <: Enumeration](enum: E): Format[E#Value] =
    Format(readsString(enum), writesString)




  implicit object MatchFilterTypeFormatter extends Formatter[MatchFilterType.Value] {
    override val format = Some(("format.enum", Nil))
    override def bind(key: String, data: Map[String, String]) = {
      //      parsing(new MatchFilterType(_), "error.enum", Nil)(key, data)
      Right(MatchFilterType.withName(data.values.head))
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

//  implicit val searchWrites = new Writes[SearchRequest] {
//    def writes(search: SearchRequest) = Json.obj(
//      "mft" -> search.mft.toString,
//      "queryText" -> search.queryText,
//      "locations" -> search.locations
//    )
//  }

//  implicit val searchFormat: Format[SearchRequest] = (
//    (JsPath \ "mft").format[MatchFilterType] and
//      (JsPath \ "queryText").format[String]
//    )(SearchRequest.apply, unlift(SearchRequest.unapply))

   implicit val searchRequestFormat = Json.format[SearchRequest]

     //  implicit object SearchRequestFormatter extends Formatter[SearchRequest] {
   //    override val format = Some(("format.enum", Nil))
     //    override def bind(key: String, data: Map[String, String]) = {
   //      //      parsing(new MatchFilterType(_), "error.enum", Nil)(key, data)
     //      Right(SearchRequest(MatchFilterType.eq, "test", List("rteasr")))
   //    }
     //    override def unbind(key: String, value: SearchRequest)              = {
   //      Map(key -> value.toString)
     //    }
   //  }

   //  implicit val searchRequestFormat: Format[SearchRequest] = enums.formatID(SearchRequest)

}

