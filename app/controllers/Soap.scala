package controllers

import play.Logger
import play.api.mvc._
import play.api.libs.iteratee._
import models._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import reactivemongo.bson.BSONObjectID
import org.jboss.netty.handler.codec.http.HttpMethod

object Soap extends Controller {

  def index(environment: String, localTarget: String) = Action.async(parse.xml) {
    implicit request =>

      Logger.debug("Request on environment:" + environment + " localTarget:" + localTarget)

      val requestContentType = request.contentType.get
      val sender = request.remoteAddress
      val content = request.body.toString()

      val headers = request.headers.toSimpleMap
      forwardRequest(environment, localTarget, sender, content, headers, requestContentType)
  }

  /**
   * Automatically detect new services. If the given parameters interpolates an existing service, then nothing is created otherwise a new service is created.
   * The new service takes the given parameters and theses defaults parameters :
   * <ul>
   * <li>record Xml Data set to false</li>
   * <li>record All Data set to false</li>
   * <li>timeoutms set to default (60000ms)</li>
   * </ul>
   * After this the equivalent of  {@link Soap#index} is made.
   *
   * @param group The group of soap request. It is a logical separation between environments.
   * @param environment The environment group of the soap request. It is a logical separation between services.
   * @param remoteTarget The remote target to be call. The underlying soap request is forwarded to this remote target.
   */
  def autoIndex(group: String, environment: String, remoteTarget: String) = Action(parse.xml) {
    implicit request =>
      ???

      // TODO
      /*
        val requestContentType = request.contentType.get
        Logger.info("Automatic service detection request on group: " + group + " environment:" + environment + " remoteTarget: " + remoteTarget)

      // Extract local target from the remote target
      val localTarget = UtilExtract.extractPathFromURL(remoteTarget)

        if (!localTarget.isDefined) {
          val err = "Invalid remoteTarget:" + remoteTarget
          Logger.error(err)
          BadRequest(err)
        }


      // Search the corresponding service
      val optionService = Service.findByLocalTargetAndEnvironmentName(Service.SOAP, localTarget.get, environment)

      var service: Service = null.asInstanceOf[Service]

      optionService match {
        case Some(realService) =>
          // If the service exists
          service = realService
        case None => {
          // If the service doesn't exits {
          val id = -1
          val description = "This service was automatically generated by soapower"
          val timeoutms = 60000
          val recordXmlData = false
          val recordData = false
          val useMockGroup = false
          val typeRequest = Service.SOAP
          val httpMethod = Service.POST

          val environmentOption = Environment.findByGroupAndByName(group, environment)
          // Check that the environment exists for the given group
          environmentOption.map {
            environmentReal =>
            // The environment exists so the service creation can be performed
              service = new Service(id,
                typeRequest,
                httpMethod,
                description,
                localTarget.get,
                remoteTarget,
                timeoutms,
                recordXmlData,
                recordData,
                useMockGroup,
                environmentReal.id,
                MockGroup.ID_DEFAULT_NO_MOCK_GROUP)
              // Persist environment to database
              Service.insert(service)
          }.getOrElse {
            val err = "environment " + environment + " with group " + group + " unknown"
            Logger.error(err)
            BadRequest(err)
          }
        }

        // Now the service exists then we have to forward the request
        val sender = request.remoteAddress
        val content = request.body.toString()
        val headers = request.headers.toSimpleMap

        forwardRequest(environment, localTarget.get, sender, content, headers, requestContentType)
        */
      BadRequest("TODO")
  }


  /**
   * Replay a given request.
   */
  def replay(requestId: String) = Action {
    //TODO
    ???
    BadRequest("TODO")
    /*
    implicit request =>
      val requestData = RequestData.load(requestId)

      val environmentTuple = Environment.options.find {
        case (k, v) => k == requestData.environmentId.toString
      }

      if (!environmentTuple.isDefined) {
        val err = "environment with id " + requestData.environmentId + " unknown"
        Logger.error(err)
        BadRequest(err)

      } else {
        val sender = requestData.sender
        val content = request.body.asXml.get.toString()
        Logger.debug("Content:" + content)
        val headers = requestData.requestHeaders
        val environmentName = environmentTuple.get._2
        if (requestData.serviceId > 0) {
          val service = Service.findById(requestData.serviceId).get
          forwardRequest(environmentName, service.localTarget, sender, content, headers, requestData.contentType)
          
        } else {
          val err = "service with id " + requestData.serviceId + " unknown"
          Logger.error(err)
          BadRequest(err)
        }
      }
      */
  }

  private def forwardRequest(environmentName: String, localTarget: String, sender: String, content: String, headers: Map[String, String], requestContentType: String): Future[SimpleResult] = {
    val service = Service.findByLocalTargetAndEnvironmentName(Service.SOAP, localTarget, environmentName)
    service.map {
      svc => {
        if (svc.isDefined && svc.get != null) {
          val client = new Client(svc.get, sender, content, headers, Service.SOAP, requestContentType)
          if (svc.get.useMockGroup && svc.get.mockGroupId.isDefined) {
            val mock = Mock.findByMockGroupAndContent(BSONObjectID(svc.get.mockGroupId.get), content)
            client.workWithMock(mock)
            val sr = new Results.Status(mock.httpStatus).stream(Enumerator(mock.response.getBytes()).andThen(Enumerator.eof[Array[Byte]]))
              .withHeaders("ProxyVia" -> "soapower")
              .withHeaders(UtilConvert.headersFromString(mock.httpHeaders).toArray: _*)
              .as(XML)

            val timeoutFuture = play.api.libs.concurrent.Promise.timeout(sr, mock.timeoutms.milliseconds)
            Await.result(timeoutFuture, 10.second) // 10 seconds (10000 ms) is the maximum allowed.
          } else {
            client.sendSoapRequestAndWaitForResponse
            // forward the response to the client
            new Results.Status(client.response.status).chunked(Enumerator(client.response.bodyBytes).andThen(Enumerator.eof[Array[Byte]]))
              .withHeaders("ProxyVia" -> "soapower")
              .withHeaders(client.response.headers.toArray: _*).as(XML)
          }
        } else {
          val err = "environment " + environmentName + " with localTarget " + localTarget + " unknown"
          Logger.error(err)
          BadRequest(err)
        }

      }
    }
  }

  /*
      private def printRequest(implicit r: play.api.mvc.RequestHeader)
      {
        Logger.info("method:" + r)
        Logger.info("headers:" + r.headers)
        Logger.info("path:" + r.path)
        Logger.info("uri:" + r.uri)
        Logger.info("host:" + r.host)
        Logger.info("rawQueryString:" + r.rawQueryString)
      }
      */
}
