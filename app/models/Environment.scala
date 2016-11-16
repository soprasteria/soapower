package models

import play.api.Play.current
import play.api.cache._
import java.util.{Calendar, GregorianCalendar}

import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.libs.json._
import reactivemongo.api.indexes.{Index, IndexType}
import reactivemongo.bson._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import reactivemongo.core.commands.RawCommand
import play.api.Logger
import reactivemongo.api.collections.default.BSONCollection
import play.modules.reactivemongo.json.BSONFormats._

case class Environment(_id: Option[BSONObjectID],
                       name: String,
                       groups: List[String],
                       hourRecordContentDataMin: Int = 8,
                       hourRecordContentDataMax: Int = 22,
                       nbDayKeepContentData: Int = 2,
                       nbDayKeepAllData: Int = 5,
                       recordContentData: Boolean = true,
                       recordData: Boolean = true) {
  /**
    * Converts an environment to a CSV line
    *
    * @return
    */
  def toCSV: String = {
    val columns = Environment.csvTitle.collect {
      case Environment.CsvKey => Environment.CsvKey
      case "id" => if (_id.isEmpty) "" else _id.get.stringify
      case "name" => name
      case "groupsName" => groups.mkString(";")
      case "hourRecordContentDataMin" => hourRecordContentDataMin.toString
      case "hourRecordContentDataMax" => hourRecordContentDataMax.toString
      case "nbDayKeepContentData" => nbDayKeepContentData.toString
      case "nbDayKeepAllData" => nbDayKeepAllData.toString
      case "recordData" => recordData.toString
      case "recordContentData" => recordContentData.toString
    }
    columns.mkString(",")
  }
}

object ModePurge extends Enumeration {
  type ModePurge = Value
  val CONTENT, ALL = Value
}


object Environment {

  /*
   * Collection MongoDB
   */
  def collection: BSONCollection = ReactiveMongoPlugin.db.collection[BSONCollection]("environments")

  def ensureIndexes() {
    Logger.info("Collection environments, ensure index")
    collection.indexesManager.ensure(Index(Seq("groups" -> IndexType.Ascending, "name" -> IndexType.Ascending)))
    collection.indexesManager.ensure(Index(Seq("name" -> IndexType.Ascending)))
  }

  implicit val environmentFormat = Json.format[Environment]

  implicit object EnvironmentBSONReader extends BSONDocumentReader[Environment] {
    def read(doc: BSONDocument): Environment = {
      Environment(
        doc.getAs[BSONObjectID]("_id"),
        doc.getAs[String]("name").get,
        doc.getAs[List[String]]("groups").toList.flatten,
        doc.getAs[Int]("hourRecordContentDataMin").get,
        doc.getAs[Int]("hourRecordContentDataMax").get,
        doc.getAs[Int]("nbDayKeepContentData").get,
        doc.getAs[Int]("nbDayKeepAllData").get,
        doc.getAs[Boolean]("recordContentData").get,
        doc.getAs[Boolean]("recordData").get
      )
    }
  }

  implicit object EnvironmentBSONWriter extends BSONDocumentWriter[Environment] {
    def write(environment: Environment): BSONDocument =
      BSONDocument(
        "_id" -> environment._id,
        "name" -> BSONString(environment.name),
        "hourRecordContentDataMin" -> BSONInteger(environment.hourRecordContentDataMin),
        "hourRecordContentDataMax" -> BSONInteger(environment.hourRecordContentDataMax),
        "nbDayKeepContentData" -> BSONInteger(environment.nbDayKeepContentData),
        "nbDayKeepAllData" -> BSONInteger(environment.nbDayKeepAllData),
        "recordContentData" -> BSONBoolean(environment.recordContentData),
        "recordData" -> BSONBoolean(environment.recordData),
        "groups" -> environment.groups)
  }

  private val keyCacheAllOptions = "environment-options"
  private val keyCacheByName = "environment-name-"
  private val ENVIRONMENT_NAME_PATTERN = "[a-zA-Z0-9]{1,200}"

  /**
    * Identity key for CSV file
    */
  val CsvKey = "environment"

  /**
    * Header of csvFile. Defines the column name and order.
    */
  val csvTitle = List(
    CsvKey, "id", "groupsName", "name", "hourRecordContentDataMin",
    "hourRecordContentDataMax", "nbDayKeepContentData", "nbDayKeepAllData",
    "recordContentData", "recordData"
  )

  val CsvHeader = "#" + Environment.csvTitle.mkString(",")

  /**
    * Get All environements, csv format.
    *
    * @return List of Environements, csv format
    */
  def fetchCsv(): List[String] =
  Await.result(findAll().map(environments => environments.map(_.toCSV)), 5.seconds)

  /**
    * Upload a csvLine => insert environment.
    *
    * @param csvLine line in csv file
    * @return nothing
    */
  def uploadCSV(csvLine: String): Either[ErrorUploadCsv, Boolean] = {
    val dataCsv = csvLine.split(",")
    try {
      if (dataCsv.size != csvTitle.size) {
        Left(ErrorUploadCsv("Please check csvFile, " + csvTitle.size + " fields required"))
      } else if (dataCsv.head == CsvKey) {
        val uploadFuture = uploadEnvironment(dataCsv)
        Right(Await.result(uploadFuture, 10.seconds))
      } else {
        Left(ErrorUploadCsv(s"First column ${dataCsv.head} is not recognized as $CsvKey"))
      }
    } catch {
      case e : Exception => Left(ErrorUploadCsv(e.getMessage))
    }
  }

  /**
    * Check if environment already exist (with same name). Insert or do nothing if exist.
    *
    * @param dataCsv line in csv file
    * @return environment (new or not)
    */
  private def uploadEnvironment(dataCsv: Array[String]): Future[Boolean] = {

    def insertEnvironment() = {
      // When no id is given, generate one
      // If id is provided keep it to insert environment
      val envFromCSV = getEnvironmentFromCSV(dataCsv)
      val env = envFromCSV._id match {
        case Some(_) => envFromCSV
        case None => envFromCSV.copy(_id = Some(BSONObjectID.generate))
      }
      Environment.insert(env).map { res =>
        Logger.info(s"Created new environment ${env.name}")
        true
      }
    }
    def updateEnvironment(envToUpdate: Environment) = {
      val env = getEnvironmentFromCSV(dataCsv).copy(_id = envToUpdate._id)
      Environment.update(env).map { res =>
        Logger.info(s"Updated existing environment ${env.name} (id=${env._id})")
        true
      }
    }

    // Find the environment from id if present
    // Search by name if not
    val id = dataCsv(csvTitle.indexOf("id"))
    val potentialEnvironmentF = if (id.isEmpty) {
      findByName(dataCsv(csvTitle.indexOf("name")), cached = false)
    } else {
      findById(BSONObjectID(id))
    }

    potentialEnvironmentF.flatMap {
      case Some(null) | None => insertEnvironment()
      case Some(e) => updateEnvironment(e)
    }

  }

  /**
    * Create an Environment object model from a csvLine
    * Send exception when semantic and/or syntax are not valid
    *
    * @param dataCsv
    * @return
    */
  private def getEnvironmentFromCSV(dataCsv: Array[String]) = {
    val idRaw = dataCsv(csvTitle.indexOf("id"))
    val id = if (idRaw.trim.isEmpty) None else Some(BSONObjectID(idRaw))

    val nameRaw = dataCsv(csvTitle.indexOf("name"))
    val name = if (nameRaw.trim.isEmpty) throw new Exception("name is required") else nameRaw.trim

    val groupsNameRaw = dataCsv(csvTitle.indexOf("groupsName"))
    // Groups without duplicates (.toSet)
    val groups = groupsNameRaw.split(";").map(group => group.trim).toSet.toList

    val hourRecordContentDataMinRaw = dataCsv(csvTitle.indexOf("hourRecordContentDataMin"))
    val hourRecordContentDataMin = UtilNumbers.toInt(hourRecordContentDataMinRaw.trim).getOrElse(8)

    val hourRecordContentDataMaxRaw = dataCsv(csvTitle.indexOf("hourRecordContentDataMax"))
    val hourRecordContentDataMax = UtilNumbers.toInt(hourRecordContentDataMaxRaw.trim).getOrElse(22)

    val nbDayKeepContentDataRaw = dataCsv(csvTitle.indexOf("nbDayKeepContentData"))
    val nbDayKeepContentData = UtilNumbers.toInt(nbDayKeepContentDataRaw.trim).getOrElse(2)

    val nbDayKeepAllDataRaw = dataCsv(csvTitle.indexOf("nbDayKeepAllData"))
    val nbDayKeepAllData = UtilNumbers.toInt(nbDayKeepAllDataRaw.trim).getOrElse(5)

    val recordContentDataRaw = dataCsv(csvTitle.indexOf("recordContentData"))
    val recordContentData = recordContentDataRaw.trim == "true"

    val recordDataRaw = dataCsv(csvTitle.indexOf("recordData"))
    val recordData = recordDataRaw.trim == "true"

    Environment(id, name, groups, hourRecordContentDataMin, hourRecordContentDataMax, nbDayKeepContentData, nbDayKeepAllData, recordContentData, recordData)
  }

  /**
    * Sort the given env option seq
    */
  private def sortEnvs(envs: Seq[(String, String)]): Seq[(String, String)] = {
    val sortedEnvs = envs.sortWith {
      (a, b) =>
        val pattern = """^(.+?)([0-9]+)$""".r

        val matchA = pattern.findAllIn(a._2)
        val matchB = pattern.findAllIn(b._2)

        if (matchA.hasNext && matchB.hasNext) {
          // both names match the regex: compare name then number
          val nameA = matchA.group(1)
          val numberA = matchA.group(2)
          val nameB = matchB.group(1)
          val numberB = matchB.group(2)
          if (nameA != nameB) {
            nameA < nameB
          } else {
            numberA.toInt <= numberB.toInt
          }

        } else if (matchA.hasNext) {
          val nameA = matchA.group(1)
          // only a matches the regex
          nameA < b._2

        } else if (matchB.hasNext) {
          val nameB = matchB.group(1)
          // only b matches the regex
          a._2 < nameB

        } else {
          // none matches the regex
          a._2 < b._2
        }
    }

    sortedEnvs
  }

  /**
    * Retrieve an Environment from id.
    */
  def findById(objectId: BSONObjectID): Future[Option[Environment]] = {
    val query = BSONDocument("_id" -> objectId)
    collection.find(query).one[Environment]
  }

  /**
    * Retrieve an Environment from name.
    */
  def findByName(name: String, cached: Boolean = true): Future[Option[Environment]] = {
    def find(): Future[Option[Environment]] = {
      val query = BSONDocument("name" -> name)
      collection.find(query).one[Environment]
    }
    if (cached) {
      Cache.getOrElse(keyCacheByName + name) {
        find()
      }
    } else {
      find()
    }

  }

  /**
    * Insert a new environment.
    *
    * @param environment The environment values.
    */
  def insert(environment: Environment) = {
    if (!environment.name.trim.matches(ENVIRONMENT_NAME_PATTERN)) {
      throw new Exception("Environment name invalid:" + environment.name.trim)
    }
    if (options.exists {
      e => e._2.equals(environment.name.trim)
    }) {
      throw new Exception("Environment with name " + environment.name.trim + " already exist")
    }
    clearCache()
    collection.insert(environment)
  }

  /**
    * Update a environment.
    *
    * @param environment The environment values.
    */
  def update(environment: Environment) = {
    if (!environment.name.trim.matches(ENVIRONMENT_NAME_PATTERN)) {
      throw new Exception("Environment name invalid:" + environment.name.trim)
    }

    if (options.exists {
      e => e._2.equals(environment.name.trim) && e._1 != environment._id.get.stringify
    }) {
      throw new Exception("Environment with name " + environment.name.trim + " already exist")
    }
    val selector = BSONDocument("_id" -> environment._id)

    val modifier = BSONDocument(
      "$set" -> BSONDocument(
        "name" -> environment.name,
        "hourRecordContentDataMin" -> environment.hourRecordContentDataMin,
        "hourRecordContentDataMax" -> environment.hourRecordContentDataMax,
        "nbDayKeepContentData" -> environment.nbDayKeepContentData,
        "nbDayKeepAllData" -> environment.nbDayKeepAllData,
        "recordContentData" -> environment.recordContentData,
        "recordData" -> environment.recordData,
        "groups" -> environment.groups)
    )
    clearCache()
    collection.update(selector, modifier)
  }

  /**
    * Delete a environment.
    *
    * @param id Id of the environment to delete.
    */
  def delete(id: String) = {
    val objectId = BSONObjectID.apply(id)
    clearCache()
    collection.remove(BSONDocument("_id" -> objectId))
  }

  def clearCache() {
    this.options.map(e => Cache.remove(keyCacheByName + e._2))
    Cache.remove(keyCacheAllOptions)
  }

  /**
    * Return a list of all environments.
    */
  def findAll(): Future[List[Environment]] = {
    collection.
      find(BSONDocument()).
      sort(BSONDocument("name" -> 1)).
      cursor[Environment].
      collect[List]()
  }

  /**
    * Return a list of all environments in some groups.
    */
  def findInGroups(groups: String): Future[List[Environment]] = {
    if ("all".equals(groups)) {
      return findAll()
    }
    val find = BSONDocument("groups" -> BSONDocument("$in" -> groups.split(',')))
    collection.
      find(find).
      sort(BSONDocument("name" -> 1)).
      cursor[Environment].
      collect[List]()
  }

  /**
    * Construct the Map[String,String] needed to fill a select options set.
    */
  def options = {
    Cache.getOrElse(keyCacheAllOptions) {
      val f = findAll().map(environments => environments.map(e => (e._id.get.stringify, e.name)))
      sortEnvs(Await result(f, 5.seconds))
    }

  }

  /**
    * Construct the Map[String,String] needed to fill a select options set for selected groups.
    */
  def optionsInGroups(groups: String) = {
    if ("all".equals(groups)) {
      options
    } else {
      val f = findInGroups(groups).map(environments => environments.map(e => (e._id.get.stringify, e.name)))
      sortEnvs(Await result(f, 5.seconds))
    }
  }

  /**
    * Find all distinct groups in environments collections.
    *
    * @return all distinct groups
    */
  def findAllGroups(): Future[BSONDocument] = {
    val command = RawCommand(BSONDocument("distinct" -> "environments", "key" -> "groups"))
    collection.db.command(command) // result is Future[BSONDocument]
  }

  /**
    * Find an environment using his name and retrieve it if the groups in parameters match the environment groups
    *
    * @param name   name of environment
    * @param groups groups, separated by ',', example group1,group2...
    * @return
    */
  def findByNameAndGroups(name: String, groups: String): Future[Option[Environment]] = {
    val find = BSONDocument("name" -> name, "groups" -> BSONDocument("$in" -> groups.split(',')))
    collection.find(find).one[Environment]
  }

  /**
    * Foreach environment, retrieve his name and his groups
    *
    * @return
    */
  def findNamesAndGroups(): List[(String, List[String])] = {
    val query = collection.find(BSONDocument()).cursor[Environment].collect[List]().map {
      list => list.map { envir => (envir.name, envir.groups) }
    }
    Await.result(query, 1.second)
  }

  import ModePurge._

  def purgeContentData() {
    purgeData(ModePurge.CONTENT)
  }

  def purgeAllData() {
    purgeData(ModePurge.ALL)
  }

  private def purgeData(mode: ModePurge) {

    Logger.info("Purging " + mode + " data...")

    val minDate = UtilDate.getDate("all").getTime
    var purgedRequests = 0

    val gcal = new GregorianCalendar
    val today = new GregorianCalendar(gcal.get(Calendar.YEAR), gcal.get(Calendar.MONTH), gcal.get(Calendar.DATE))

    Environment.findAll().map(environments => environments.map(
      env => {
        var nbDay = 100
        val maxDate = new GregorianCalendar
        if (mode == ModePurge.CONTENT)
          nbDay = env.nbDayKeepContentData
        else
          nbDay = env.nbDayKeepAllData

        maxDate.setTimeInMillis(today.getTimeInMillis - UtilDate.v1d * nbDay)
        Logger.debug("Purge env: " + env.name + " NbDaysKeep: " + nbDay + " MinDate:" + minDate + " MaxDate:" + maxDate.getTime)
        val user = "Soapower Akka Scheduler (keep " + mode + " data for " + nbDay + " days for this env " + env.name + ")"
        if (mode == ModePurge.CONTENT)
          purgedRequests += RequestData.deleteRequestResponse(env.name, minDate, maxDate.getTime, user)
        else
          purgedRequests += RequestData.delete(env.name, minDate, maxDate.getTime)
      }
    ))
    Logger.info("Purging " + mode + " data: done (" + purgedRequests + " requests purged)")
  }

}
