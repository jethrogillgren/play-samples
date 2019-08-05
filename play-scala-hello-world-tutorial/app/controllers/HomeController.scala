package controllers

import javax.inject._
import models.MatchFilterType.MatchFilterType
import models.MatchFilterType.matchFilterTypeFormat
import models.{MatchFilterType, SearchRequest}
import play.api.data.{Form, Forms}
import play.api.data.Forms.{list, mapping, nonEmptyText, text}
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.Future

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {


  val searchForm: Form[SearchRequest] = Form(
    mapping(
      "mft" -> Forms.of[MatchFilterType],
      "queryText" -> nonEmptyText,
      "locations" -> list(text)
    )(SearchRequest.apply)(SearchRequest.unapply)
  )

  def getSearch = Action.async {
    Future.successful( Ok(Json.toJson(SearchRequest(MatchFilterType.ONE, "test", List[String]() ))) )
  }
  def doSearch = Action.async { implicit request =>
    searchForm.bindFromRequest.fold(
      searchForm => {
        Future.successful(BadRequest("nooo"))
      },
      search => {
        Future.successful(Ok(Json.toJson(search)))
      }
    )
  }

  
}
