package models

/**
  * Contains util functions around numbers.
  */
object UtilNumbers {

  /**
    * Convert string to optional int.
    * If s cannot be converted to Int, this method returns None
    * @param s the string to convert
    * @return
    */
  def toInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case e: Exception => None
    }
  }
}
