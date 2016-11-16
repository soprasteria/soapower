package models

import play.api.Play.current
import play.api.cache.Cache
import reactivemongo.bson._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.modules.reactivemongo.json.BSONFormats._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import play.api.libs.json.Json
import reactivemongo.bson.BSONBoolean
import reactivemongo.bson.BSONString
import reactivemongo.bson.BSONInteger
import org.jboss.netty.handler.codec.http.HttpMethod
import play.api.Logger

case class Service(_id: Option[BSONObjectID],
                   description: String,
                   typeRequest: String,
                   httpMethod: String,
                   localTarget: String,
                   remoteTarget: String,
                   timeoutms: Int,
                   recordContentData: Boolean,
                   recordData: Boolean,
                   useMockGroup: Boolean,
                   mockGroupId: Option[String],
                   environmentName: Option[String]) {

  def this(serviceDoc: BSONDocument, environmentName: Option[String]) =
    this(
      serviceDoc.getAs[BSONObjectID]("_id"),
      serviceDoc.getAs[String]("description").get,
      serviceDoc.getAs[String]("typeRequest").get,
      serviceDoc.getAs[String]("httpMethod").get,
      serviceDoc.getAs[String]("localTarget").get,
      serviceDoc.getAs[String]("remoteTarget").get,
      serviceDoc.getAs[Int]("timeoutms").get,
      serviceDoc.getAs[Boolean]("recordContentData").get,
      serviceDoc.getAs[Boolean]("recordData").get,
      serviceDoc.getAs[Boolean]("useMockGroup").get,
      serviceDoc.getAs[String]("mockGroupId"),
      environmentName)

  /**
    * Converts an service to a CSV line
    *
    * @return
    */
  val toCSV: String = {

    val columns = Service.CsvTitle.map {
      case Service.CsvKey => Service.CsvKey
      case "id" => if (_id.isEmpty) "" else _id.get.stringify
      case "description" => description.replaceAll(",", "")
      case "typeRequest" => typeRequest
      case "httpMethod" => httpMethod
      case "localTarget" => localTarget
      case "remoteTarget" => remoteTarget
      case "timeoutms" => timeoutms.toString
      case "recordContentData" => recordContentData.toString
      case "recordData" => recordData.toString
      case "useMockGroup" => useMockGroup.toString
      case "mockGroupId" => mockGroupId.getOrElse("")
      case "environmentName" => environmentName.getOrElse("")
    }
    columns.mkString(",")
  }
}


case class Services(services: List[Service])

object Service {

  implicit val serviceFormat = Json.format[Service]
  implicit val servicesFormat = Json.format[Services]

  private val keyCacheRequest = "cacheServiceRequest-"

  def clearCache() = Cache.remove(keyCacheRequest)

  implicit object ServicesBSONReader extends BSONDocumentReader[Services] {
    def read(doc: BSONDocument): Services = {
      if (doc.getAs[List[BSONDocument]]("services").isDefined) {
        val list = doc.getAs[List[BSONDocument]]("services").get.map(
          s => new Service(s, doc.getAs[String]("name"))
        )
        Services(list)
      } else {
        Services(List())
      }
    }
  }


  /**
    * Identity key for CSV file
    */
  val CsvKey = "service"

  /**
    * Header of csvFile. Defines the column name and order.
    */
  val CsvTitle = List(
    CsvKey, "id", "description", "environmentName", "typeRequest", "httpMethod",
    "localTarget", "remoteTarget", "useMockGroup", "mockGroupId",
    "timeoutms", "recordContentData", "recordData"
  )

  /**
    * Get all services, csv format.
    *
    * @return List of services, csv format
    */
  def fetchCsv(): List[String] =
    Await.result(findAll.map(service => service.map(_.toCSV)), 5.seconds)

  /**
    * Get the header as a csv string
    *
    * @return
    */
  val CsvHeader = "#" + Service.CsvTitle.mkString(",")

  /**
    * Upload a csvLine => insert service & potential environment.
    *
    * @param csvLine line in csv file
    * @return nothing
    */
  def uploadCSV(csvLine: String): Either[ErrorUploadCsv, Boolean] = {
    val dataCsv = csvLine.split(",")

    try {
      if (dataCsv.size != CsvTitle.size) {
        Left(ErrorUploadCsv("Please check csvFile, " + CsvTitle.size + " fields required"))
      } else if (dataCsv.head == CsvKey) {
        val uploadFuture = uploadEnvironment(dataCsv).flatMap(_ => uploadService(dataCsv))
        Right(Await.result(uploadFuture, 10.seconds))
      } else {
        Left(ErrorUploadCsv(s"First column ${dataCsv.head} is not recognized as $CsvKey"))
      }
    } catch {
      case e : Exception => Left(ErrorUploadCsv(e.getMessage))
    }

  }

  /**
    * Upload a new Environment from the service
    *
    * @param dataCsv
    */
  private def uploadEnvironment(dataCsv: Array[String]): Future[Boolean] = {

    val envS = dataCsv(CsvTitle.indexOf("environmentName")).trim
    val localTarget = dataCsv(CsvTitle.indexOf("localTarget"))
    if (envS.isEmpty) {
      throw new Exception(s"environmentName is mandatory when uploading the service $localTarget.")
    }

    def insertEnvironment(env: Environment, localTarget: String): Future[Boolean] = {
      Environment.insert(env).map { _ =>
        Logger.info(s"Created new default environment ${env.name} for service $localTarget")
        true
      }
    }

    // Search the environment by its name
    val potentialEnvironmentF = Environment.findByName(envS, cached = false)
    potentialEnvironmentF.flatMap {
      case Some(e) =>
        // Environment exists, so we do nothing because we don't have any other info to update
        Logger.info(s"Environment ${e.name} exists for service $localTarget")
        Future.successful(true)
      case None =>
        // Create a default environment with name
        Logger.info(s"Environment $envS does not exist for service $localTarget")
        insertEnvironment(Environment(_id = None, name = envS, groups = List()), localTarget)
    }
  }


  /**
    * Check if service already exist (with localTarget and Environment). Insert or update if exist.
    *
    * @param dataCsv line in csv file
    * @return service (new or not)
    */
  private def uploadService(dataCsv: Array[String]): Future[Boolean] = {

    val id = dataCsv(CsvTitle.indexOf("id"))
    val typeRequest = dataCsv(CsvTitle.indexOf("typeRequest"))
    val localTarget = dataCsv(CsvTitle.indexOf("localTarget"))
    val environmentName = dataCsv(CsvTitle.indexOf("environmentName"))
    val httpMethod = dataCsv(CsvTitle.indexOf("httpMethod"))
    val potentialServiceF = if (id.isEmpty) {
      findByLocalTargetAndEnvironmentName(typeRequest, localTarget, environmentName, HttpMethod.valueOf(httpMethod))
    } else {
      findById(dataCsv(CsvTitle.indexOf("environmentName")), id)
    }

    def insertService(): Future[Boolean] = {
      // When no id is given, generate one
      // If id is provided keep it to insert service
      val serviceFromCSV = getServiceFromCSV(dataCsv)
      val service = serviceFromCSV._id match {
        case Some(_) => serviceFromCSV
        case None => serviceFromCSV.copy(_id = Some(BSONObjectID.generate))
      }
      Service.insert(service).map {res =>
        Logger.info(s"Created new service $localTarget for $environmentName and $typeRequest/$httpMethod")
        true
      }
    }
    def updateService(serviceToUpdate: Service): Future[Boolean] = {
      val service = getServiceFromCSV(dataCsv).copy(_id = serviceToUpdate._id)
      Service.update(service).map { res =>
        Logger.info(s"Updated existing service $localTarget for $environmentName and $typeRequest/$httpMethod")
        true

      }
    }

    potentialServiceF.flatMap {
      case Some(null) | None => insertService()
      case Some(e) ⇒ updateService(e)
    }
  }


  /**
    * Get a service object from a csv structured line.
    * Check syntax and semantic of each column
    *
    * @param dataCsv
    * @return
    */
  private def getServiceFromCSV(dataCsv: Array[String]) = {
    val idRaw = dataCsv(CsvTitle.indexOf("id"))
    val id = if (idRaw.trim.isEmpty) None else Some(BSONObjectID(idRaw))

    val descriptionRaw = dataCsv(CsvTitle.indexOf("description"))
    val description = descriptionRaw.trim

    val environmentNameRaw = dataCsv(CsvTitle.indexOf("environmentName"))
    val environmentName = if (environmentNameRaw.trim.isEmpty) throw new Exception("environmentName is required")
    else Some(environmentNameRaw.trim)

    val typeRequestRaw = dataCsv(CsvTitle.indexOf("typeRequest"))
    val typeRequest = if (Set(REST, SOAP).contains(typeRequestRaw.trim)) typeRequestRaw.trim
    else throw new Exception(s"typeRequest should be either ${REST} or ${SOAP}")

    val httpMethod = if (typeRequest == SOAP) {
        // When SOAP webservices, typeRequest has to be POST
        "POST"
      } else {
        val httpMethodRaw = dataCsv(CsvTitle.indexOf("httpMethod"))
        if (httpMethodRaw.trim.nonEmpty)
          httpMethodRaw.trim
        else
          throw new Exception("httpMethod is required")
      }

    val localTargetRaw = dataCsv(CsvTitle.indexOf("localTarget"))
    val localTarget = if (localTargetRaw.trim.isEmpty) throw new Exception("localTarget is required")
    else localTargetRaw.trim

    val remoteTargetRaw = dataCsv(CsvTitle.indexOf("remoteTarget"))
    val remoteTarget = if (remoteTargetRaw.trim.isEmpty) throw new Exception("remoteTarget is required")
    else remoteTargetRaw.trim

    val useMockGroupRaw = dataCsv(CsvTitle.indexOf("useMockGroup"))
    val useMockGroup = useMockGroupRaw.trim == "true"

    val mockGroupIdRaw = dataCsv(CsvTitle.indexOf("mockGroupId"))
    val mockGroupId = if (mockGroupIdRaw.trim.isEmpty) None else Some(mockGroupIdRaw.trim)

    val timeoutmsRaw = dataCsv(CsvTitle.indexOf("timeoutms"))
    val timeoutms = UtilNumbers.toInt(timeoutmsRaw.trim).getOrElse(5000)

    val recordContentDataRaw = dataCsv(CsvTitle.indexOf("recordContentData"))
    val recordContentData = recordContentDataRaw.trim == "true"

    val recordDataRaw = dataCsv(CsvTitle.indexOf("recordData"))
    val recordData = recordDataRaw.trim == "true"

    Service(id, description, typeRequest, httpMethod, localTarget, remoteTarget, timeoutms, recordContentData, recordData, useMockGroup, mockGroupId, environmentName)
  }


  /**
    * Services
    */
  val REST = "REST"
  val SOAP = "SOAP"

  implicit object ServiceBSONReader extends BSONDocumentReader[Service] {
    def read(doc: BSONDocument): Service = {
      if (doc.getAs[List[BSONDocument]]("services").isDefined) {
        val s = doc.getAs[List[BSONDocument]]("services").get.head
        new Service(s, doc.getAs[String]("name"))
      } else {
        null
      }
    }
  }

  implicit object ServiceBSONWriter extends BSONDocumentWriter[Service] {
    def write(service: Service): BSONDocument =
      BSONDocument(
        "_id" -> service._id,
        "description" -> BSONString(service.description),
        "typeRequest" -> BSONString(service.typeRequest),
        "httpMethod" -> BSONString(service.httpMethod),
        "localTarget" -> BSONString(checkLocalTarget(service.localTarget)),
        "remoteTarget" -> BSONString(service.remoteTarget),
        "timeoutms" -> BSONInteger(service.timeoutms),
        "recordContentData" -> BSONBoolean(service.recordContentData),
        "recordData" -> BSONBoolean(service.recordData),
        "useMockGroup" -> BSONBoolean(service.useMockGroup),
        "mockGroupId" -> service.mockGroupId)
  }

  /**
    * Retrieve a Service.
    *
    * @param environmentName Name of environement
    * @param serviceId       ObjectID of service
    * @return Option of service
    */
  def findById(environmentName: String, serviceId: String): Future[Option[Service]] = {
    val query = BSONDocument("name" -> environmentName)
    val projection = BSONDocument("name" -> 1, "groups" -> 1, "services" -> BSONDocument(
      "$elemMatch" -> BSONDocument("_id" -> BSONObjectID(serviceId))))
    Environment.collection.find(query, projection).cursor[Service].headOption
  }

  /**
    * Retrieve a Soap Service from localTarget / environmentName
    *
    * @param localTarget     localTarget
    * @param environmentName Name of environment
    * @return service
    */
  def findByLocalTargetAndEnvironmentName(typeRequest: String, localTarget: String, environmentName: String, httpMethod: HttpMethod): Future[Option[Service]] = {
    Cache.getOrElse(keyCacheRequest + typeRequest + localTarget + environmentName + httpMethod.toString, 15) {
      val query = BSONDocument("name" -> environmentName)
      val projection = BSONDocument("name" -> 1, "groups" -> 1, "services" -> BSONDocument(
        "$elemMatch" -> BSONDocument(
          "localTarget" -> BSONString(localTarget),
          "httpMethod" -> BSONString(httpMethod.toString),
          "typeRequest" -> BSONString(typeRequest))))
      Environment.collection.find(query, projection).cursor[Service].headOption
    }
  }

  /**
    * Insert a new service.
    *
    * @param service The service values
    */
  def insert(service: Service) = {
    val selectorEnv = BSONDocument("name" -> service.environmentName)
    val insert = BSONDocument("$push" -> BSONDocument("services" -> service))
    Environment.collection.update(selectorEnv, insert)
  }

  /**
    * Update a service.
    *
    * @param service The service values.
    */
  def update(service: Service) = {
    val selector = BSONDocument(
      "name" -> service.environmentName,
      "services._id" -> service._id
    )
    val update = BSONDocument("$set" -> BSONDocument("services.$" -> service))
    Environment.collection.update(selector, update)
  }

  /**
    * Delete a service.
    *
    * @param environmentName environment name wich contains the service
    * @param serviceId       id of the service to delete
    * @return
    */
  def delete(environmentName: String, serviceId: String) = {
    val selector = BSONDocument("name" -> environmentName)
    val update = BSONDocument("$pull" -> BSONDocument("services" -> BSONDocument("_id" -> BSONObjectID(serviceId))))
    Environment.collection.update(selector, update)
  }

  def findAll(environmentName: String): Future[Option[Services]] = {
    val query = BSONDocument("name" -> environmentName)
    Environment.collection.find(query).cursor[Services].headOption
  }

  /**
    * Return a list of all services.
    */
  def findAll: Future[List[Service]] = {
    val query = BSONDocument()
    Environment.collection.find(query).cursor[Services].collect[List]().map(l => l.flatMap(s => s.services))
  }

  /**
    * Remove first / in localTarget.
    */
  private def checkLocalTarget(localTarget: String) = {
    if (localTarget.startsWith("/")) localTarget.substring(1) else localTarget
  }

}
