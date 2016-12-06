package models

object UtilConvert {

  private val lineSeparator = "\n"
  private val keyValueSeparator = " -> "

  def headersToString(headers: Option[Map[String, String]]): String = headers match {
    case Some(null) | None => ""
    case Some(actualHeaders) => actualHeaders.map { case (k, v) => k + keyValueSeparator + v }.mkString(lineSeparator)
  }

  def headersFromString(headersAsStr: String): Map[String, String] =
    headersAsStr.split(lineSeparator).map { kv =>
      val splitKv = kv.split(keyValueSeparator, 2)
      splitKv.head -> splitKv.tail.head
    }.toMap

}
