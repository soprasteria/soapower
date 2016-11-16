package models

/**
  * Error when uploading CSV
  * @param msg the error message
  */
final case class ErrorUploadCsv(msg: String) extends AnyVal
