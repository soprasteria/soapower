package models

import java.text.{ParseException, SimpleDateFormat}
import java.util.{Calendar, Date, GregorianCalendar, TimeZone}

import org.joda.time.DateTime
import play.api.libs.concurrent.Execution.Implicits._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success}

// TODO consider using scala date classes instead of Gregorian Calendar

object UtilDate {

  val v23h59min59s = ((24 * 60 * 60) - 1) * 1000
  val v1d = 24 * 60 * 60 * 1000
  val pattern = "today-([0-9]+)".r
  val longFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
  val shortFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm")
  val defaultGCal = new GregorianCalendar()

  def getDate(sDate: String, addInMillis: Long = 0, isMax: Boolean = false): GregorianCalendar = {
    val gCal = new GregorianCalendar()

    gCal.set(Calendar.HOUR_OF_DAY, 0)
    gCal.set(Calendar.MINUTE, 0)
    gCal.set(Calendar.SECOND, 0)

    sDate match {
      case "all" | "today" if isMax =>
        // the date is set to today date at 23 hour 59 minutes 59s
        gCal.setTimeInMillis(gCal.getTimeInMillis + v23h59min59s)
      case "all" =>
        // the date is set to the start time of the oldest requestData
        val findMin = RequestData.getMinRequestData.andThen {
          case Success(request) if request.isDefined => gCal.setTimeInMillis(request.get.startTime.getMillis)
          case Success(_) | Failure(_) => gCal.setTime(new Date) // FIXME log failure?
        }
        Await.result(findMin, 5000 millis) // FIXME Blocking future is evil
      case "yesterday" =>
        // the date is set to yesterdays date
        gCal.add(Calendar.DATE, -1)
        if (isMax) {
          gCal.setTimeInMillis(gCal.getTimeInMillis + v23h59min59s)
        }
      case pattern(days) => gCal.add(Calendar.DATE, -days.toInt)
      case _ =>
        try {
          gCal.setTime(shortFormat.parse(sDate))
        }
        catch {
          case e: ParseException =>
            if (isMax) {
              gCal.setTimeInMillis(gCal.getTimeInMillis + v23h59min59s)
            }
            else {
              gCal.add(Calendar.DATE, -1)
            }
        }
    }
    gCal

  }

  def formatedDate(gCal: GregorianCalendar): String = gCal.get(Calendar.YEAR) + "-" +
    addZero(gCal.get(Calendar.MONTH) + 1) + "-" +
    addZero(gCal.get(Calendar.DATE)) + " " +
    addZero(gCal.get(Calendar.HOUR_OF_DAY)) + ":" +
    addZero(gCal.get(Calendar.MINUTE)) + ":" +
    addZero(gCal.get(Calendar.SECOND))


  def formatedDate(date: Date): String = {
    defaultGCal.setTime(date).toString
    defaultGCal.get(Calendar.YEAR) + "-" +
      addZero(defaultGCal.get(Calendar.MONTH) + 1) + "-" +
      addZero(defaultGCal.get(Calendar.DATE)) + " " +
      addZero(defaultGCal.get(Calendar.HOUR_OF_DAY)) + ":" +
      addZero(defaultGCal.get(Calendar.MINUTE)) + ":" +
      addZero(defaultGCal.get(Calendar.SECOND)) + "." +
      addZero(defaultGCal.get(Calendar.MILLISECOND))
  }

  def parse(date: String): Date = longFormat.parse(date)

  def addZero(f: Int): String = "%02d".format(f)

  def getGMTDateTime(date: Date): DateTime = {
    TimeZone.setDefault(TimeZone.getTimeZone("GMT"))
    new DateTime(date)
  }
}
