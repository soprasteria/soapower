package controllers

import models.{Criterias, LiveRoom}
import play.api.libs.json.{JsValue, Json}
import play.api.mvc._

object Live extends Controller {

  case class Criteria(key: String, value: String)

  /**
    * Handles the websocket.
    */
  def socket(group: String, environment: String, serviceaction: String, code: String) = WebSocket.async[JsValue] {
    implicit request =>
      // Create the client criterias based on the URL
      val clientCriterias = Criterias(group, environment, serviceaction, code, search = "", request = true, response = true)
      LiveRoom.join(request.remoteAddress, clientCriterias)
  }

  implicit val format = Json.format[Criteria]

  /**
    * Change the criteria of a user in the LiveRoom
    *
    * @return
    */
  def changeCriteria() = Action(parse.json) {
    implicit request =>
      val json: JsValue = Json.parse(request.body.toString)
      json.validate(format)
      // We parse the incoming data in JSON
      json.validate(format).map {
        criteria =>
          LiveRoom.changeCriterias(request.remoteAddress, (criteria.key, criteria.value))
          Ok("Criterias changed")
      }.getOrElse {
        BadRequest("Wrong criterias format")
      }
  }
}
