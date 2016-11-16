package controllers.admin

import play.api.mvc._
import models.{ErrorUploadCsv, _}
import play.api.Logger
import play.api.libs.iteratee._
import play.api.http._
import play.api.libs.Files.TemporaryFile
import play.api.libs.json.Json
import play.api.mvc.MultipartFormData.FilePart

import scala.io.Source

/**
  * BulkConfiguration controller is the endpoint of routes where admin can configure Soapower application by bulk batches
  */
object BulkConfiguration extends Controller {
  /**
    * Convert an message object to a Json Object
    *
    * @param code the code of the message
    * @param text the message to send
    * @param data the metadata for the message
    * @return A Json representation of the message
    */
  def toJson(code: String, text: String, data: List[String] = List()) = Json.obj("code" -> code, "text" -> text, "data" -> data)

  /**
    * Upload the configuration from a file (body field fileUploaded)
    */
  def uploadConfiguration = Action(parse.multipartFormData) { request =>
    // Clear caches, as we will create new services/environments and update potentially all services/environments
    Service.clearCache()
    Environment.clearCache()
    // Get the file
    request.body.file("fileUploaded").map(uploadFile).getOrElse {
      Ok(toJson("danger", "Error ! Uploading configuration is not available right now. See with an administrator.")).as(JSON)
    }
  }

  /**
    * Upload the configuration from a file
    */
  private def uploadFile(fileUploaded: FilePart[TemporaryFile]): Result = {
    // Parse the file, line by line
    val lines = Source.fromFile(fileUploaded.ref.file).getLines().toList

    val uploadResults = lines.map {
      case line if line.startsWith(Service.CsvKey) =>
        Logger.info("Uploading service: " + line)
        Some(Service.uploadCSV(line))
      case line if line.startsWith(Environment.CsvKey) =>
        Logger.info("Uploading environment: " + line)
        Some(Environment.uploadCSV(line))
      case line =>
        Logger.debug(s"Ignoring not recognized line $line")
        None
    }.collect {
      case Some(result) => result
    }

    val uploadErrors = uploadResults.zipWithIndex.map {
      case (Left(error), index) =>
        Logger.warn(s"Failed upload of line ${index + 1}: ${error.msg}")
        Some(ErrorUploadCsv(s"Line ${index + 1}: ${error.msg}"))
      case (Right(result), index) =>
        Logger.debug(s"Uploaded line ${index + 1}")
        None
    }

    if (uploadErrors.exists(_.nonEmpty)) {
      // Number of lines successfully uploaded
      val linesUploaded = uploadErrors.count(_.isEmpty)
      // Number of effective lines in file (i.e. comments are not processed)
      val effectiveLines = uploadResults.size
      val errorMessages = uploadErrors.collect { case Some(error) => error.msg }
      Ok(toJson(
        "warning",
        s"Warning ! Configuration uploaded partially ($linesUploaded/$effectiveLines lines uploaded). Fix the errors and upload the file again.",
        errorMessages
      )).as(JSON)
    } else {
      Ok(toJson("success", "Success ! Every line of configuration is uploaded.")).as(JSON)
    }
  }

  /**
    * Download the configuration as a file. File exported can be uploaded afterwards
    *
    * @return
    */
  def downloadConfiguration = Action {
    val header = List(Environment.CsvHeader, Service.CsvHeader)
    val content = header ++ Environment.fetchCsv() ++ Service.fetchCsv()
    // result as a file
    val body = Enumerator(content.mkString("\n").getBytes())
    Result(ResponseHeader(play.api.http.Status.OK), body)
      .withHeaders((HeaderNames.CONTENT_DISPOSITION, "attachment; filename=configuration.csv")).as(BINARY)
  }

  /**
    * Print a sample code of file configuration explaining columns
    * @return
    */
  def configurationExample = Action {
    val sampleEnvironment =  Environment(None, "env1", List("group1", "newGroup2"))
    val sampleService = Service(None, "A simple desc", "REST", "GET", "localTarget", "http://target:port/remote",
      timeoutms = 1000, recordContentData = true, recordData = true, useMockGroup = false, None, Some("env1"))
    val example = List(
      s"#Example for key '${Environment.CsvKey}'",
      Environment.CsvHeader,
      sampleEnvironment.toCSV,
      s"#Example for key '${Service.CsvKey}'",
      Service.CsvHeader,
      sampleService.toCSV
    )
    Ok(example.mkString("\n"))
  }


}
