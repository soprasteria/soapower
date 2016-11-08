package controllers.admin

import play.api.mvc._
import models.{ErrorUploadCsv, _}
import play.api.Logger
import play.api.libs.iteratee._
import play.api.http._
import play.api.libs.json.Json

import scala.collection.mutable.ListBuffer

/**
  *BulkConfiguration controller is the endpoint of routes where admin can configure Soapower application by bulk batches
  */
object BulkConfiguration extends Controller {
  /**
    * Convert an message object to a Json Object
    * @param code the code of the message
    * @param text the message to send
    * @param data the metadata for the message
    * @return A Json representation of the message
    */
  def toJson(code: String, text: String, data: List[String] = List()) = Json.obj("code" -> code, "text" -> text, "data" -> data)

  /**
    * Upload the configuration from a file (body field fileUploaded)
    * @return
    */
  def uploadConfiguration = Action(parse.multipartFormData) {
    request =>
      // Clear caches, as we will create new services/environments and update potentially all services/environments
      Service.clearCache
      Environment.clearCache
      // Get the file
      request.body.file("fileUploaded").map {
        fileUploaded =>
          import scala.io._
          var errors = new ListBuffer[ErrorUploadCsv]() // Errors that may happen on line parsing are appended
          var linesNumber: Int = 1 // Counter of lines in file
          var effectiveLines: Int = 0 // Counter of effective lines in file (i.e. comments are not processed)
          var linesUploaded: Int = 0 // Counter of lines successfully uploaded

          // Parse the file, line by line
          for (line <- Source.fromFile(fileUploaded.ref.file).getLines()) {
            try {
              val res = if (line.startsWith(Service.csvKey)) {
                Logger.info("Uploading service: " + line)
                effectiveLines += 1
                Some(Service.uploadCSV(line))
              } else if (line.startsWith(Environment.csvKey)) {
                Logger.info("Uploading environment: " + line)
                effectiveLines += 1
                Some(Environment.uploadCSV(line))
              } else {
                None
              }

              res match {
                case Some(Left(error)) => {
                  Logger.warn(s"Failed upload of line ${linesNumber}: ${error.msg}")
                  errors += ErrorUploadCsv(s"Line ${linesNumber}: ${error.msg}")
                }
                case Some(Right(result)) => {
                  linesUploaded += 1
                  Logger.info("Uploaded : " + line)
                }
                case None => {
                  Logger.debug(s"Ignoring not recognized line ${line}")
                }
              }
            } catch {
              case e: Exception => {
                Logger.warn(s"Failed upload line ${linesNumber}: ${e.getMessage}")
                errors += ErrorUploadCsv(s"Line ${linesNumber}: ${e.getMessage}")
              }
            } finally {
              linesNumber += 1
            }
          }

          if (!errors.isEmpty) {
            Ok(toJson(
              "warning",
              s"Warning ! Configuration uploaded partially (${linesUploaded}/${effectiveLines} lines uploaded). Fix the errors and reupload the file.",
              errors.map(e => e.msg).toList
            )).as(JSON)
          } else {
            Ok(toJson("success", "Success ! Every line of configuration is uploaded.")).as(JSON)
          }
      }.getOrElse {
        Ok(toJson("danger", "Error ! Uploading configuration is not available right now. See with an administrator.")).as(JSON)
      }
  }

  /**
    * Download the configuration as a file. File exported can be uploaded afterwards
    * @return
    */
  def downloadConfiguration = Action {
    // data
    var content = ""
    content += Environment.fetchCsvHeader() + "\n"
    content += Service.fetchCsvHeader() + "\n"
    Environment.fetchCsv().foreach { s => content += s + "\n"}
    Service.fetchCsv().foreach { s => content += s + "\n"}

    // result as a file
    val fileContent = Enumerator(content.getBytes())
    Result(
      header = ResponseHeader(play.api.http.Status.OK),
      body = fileContent
    ).withHeaders((HeaderNames.CONTENT_DISPOSITION, "attachment; filename=configuration.csv")).as(BINARY)
  }

  /**
    * Print a sample code of file configuration
    * Explains different columns
    * @return
    */
  def configurationExample = Action {
    var content = "#Example for key \"" + Environment.csvKey + "\"\n#"
    content += Environment.fetchCsvHeader() + "\n"
    content += Environment(None, "env1", List("group1", "newGroup2")).toCSV() + "\n"
    content += "#Example for key \"" + Service.csvKey + "\"\n#"
    content += Service.fetchCsvHeader() + "\n"
    content += Service(None, "A simple desc", "REST", "GET", "localTarget", "http://target:port/remote", 1000, true, true, false, None, Some("env1")).toCSV() + "\n"
    Ok(content)
  }



}
