package models

import java.util.Date

import org.joda.time.DateTime
import play.api.Play.current
import play.Logger
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.Json
import play.modules.reactivemongo.{MongoController, ReactiveMongoApi, ReactiveMongoComponents}
import play.modules.reactivemongo.json._
import reactivemongo.api.Cursor._
import reactivemongo.api.{DefaultDB, FailoverStrategy}
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.commands.WriteResult
import reactivemongo.bson.{BSONString, _}
import reactivemongo.core.commands.RawCommand

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}
import util.FutureOps._

case class Stat(_id: Option[BSONObjectID],
                groups: List[String],
                environmentName: String,
                serviceAction: String,
                avgInMillis: Long,
                nbOfRequestData: Long,
                atDate: DateTime) {

  def this(groups: List[String], environmentName: String, serviceAction: String, avgInMillis: Long, nbOfRequestData: Long, atDate: DateTime) =
    this(Some(BSONObjectID.generate), groups, environmentName, serviceAction, avgInMillis, nbOfRequestData, atDate)

}

object Stat extends MongoController with ReactiveMongoComponents {
  lazy val reactiveMongoApi = current.injector.instanceOf[ReactiveMongoApi]

  /*
   * Collection MongoDB
   *
   */
  def collection: Future[BSONCollection] = database.map(_.collection[BSONCollection]("statistics"))

  implicit val statsFormat = Json.format[Stat]

  implicit object StatsBSONReader extends BSONDocumentReader[Stat] {
    def read(doc: BSONDocument): Stat = {
      Stat(
        doc.getAs[BSONObjectID]("_id"),
        doc.getAs[List[String]]("groups").toList.flatten,
        doc.getAs[String]("environmentName").get,
        doc.getAs[String]("serviceAction").get,
        doc.getAs[Long]("avgInMillis").get,
        doc.getAs[Long]("nbOfRequestData").get,
        new DateTime(doc.getAs[BSONDateTime]("atDate").get.value)
      )
    }
  }

  implicit object StatsBSONWriter extends BSONDocumentWriter[Stat] {
    def write(stat: Stat): BSONDocument =
      BSONDocument(
        "_id" -> stat._id,
        "groups" -> stat.groups,
        "environmentName" -> BSONString(stat.environmentName),
        "serviceAction" -> BSONString(stat.serviceAction),
        "avgInMillis" -> BSONLong(stat.avgInMillis),
        "nbOfRequestData" -> BSONLong(stat.nbOfRequestData),
        "atDate" -> BSONDateTime(stat.atDate.getMillis))
  }

  /**
    * Insert stat
    */
  def insert(stat: Stat): Future[Option[WriteResult]] = {
    findByGroupsEnvirServiceDate(stat).flatMap {
      case None =>
        Logger.debug("New statistics for env : " + stat.environmentName + ", in groups : " + stat.groups.mkString(", ") + " and for serviceAction : " + stat.serviceAction + " at " + stat.atDate.toDate)
        collection.flatMap(_.insert(stat)).toFutureOption
      case Some(_) =>
        Logger.debug("This statistic already exists")
        Future.successful(None)
    }
  }

  def findAll(): Future[List[Stat]] = {
    val errorHandler: ErrorHandler[List[Stat]] = FailOnError[List[Stat]]()
    collection.flatMap(
      _.find(BSONDocument())
        .sort(BSONDocument("groups" -> 1, "environmentName" -> 1, "serviceAction" -> 1))
        .cursor[Stat]()
        .collect(maxDocs = -1, errorHandler)
    )
  }

  /**
    * Find a stat using groups, environmentName and serviceaction
    */
  def findByGroupsEnvirServiceDate(stat: Stat): Future[Option[Stat]] = {
    val find = BSONDocument("groups" -> stat.groups, "environmentName" -> stat.environmentName, "serviceAction" -> stat.serviceAction, "atDate" -> BSONDocument("$eq" -> BSONDateTime(stat.atDate.toDate.getTime)))
    collection.flatMap(_.find(find).one[Stat])
  }

  case class PageStat(groups: List[String], environmentName: String, serviceAction: String, avgInMillis: Long, treshold: Long)

  /**
    * Return a page of stats
    *
    * @return a list of (groups, environmentName, serviceaction, avgTime, Treshold)
    */
  def find(groups: String, environmentName: String, minDate: Date, maxDate: Date): Future[List[PageStat]] = {

    // First, we retrieve a map of all serviceactions and their treshold, organized by name and groups
    val serviceActions = Await.result(ServiceAction.findAll, 1.second).map {
      sa =>
        ((sa.name, sa.groups), sa.thresholdms)
    }.toMap

    // We remove 1000 millisecond to minDate to avoid issue with last two milliseconds being random
    // when mindate is set to yesterday
    var matchQuery = BSONDocument("atDate" -> BSONDocument(
      "$gt" -> BSONDateTime(minDate.getTime - 1000),
      "$lte" -> BSONDateTime(maxDate.getTime))
    )

    if (groups != "all") {
      matchQuery = matchQuery ++ ("groups" -> BSONDocument("$in" -> groups.split(',')))
    }
    if (environmentName != "all") {
      matchQuery = matchQuery ++ ("environmentName" -> environmentName)
    }

    val command = collection.map { c =>
        BSONDocument(
          "aggregate" -> c.name, // we aggregate on collection
          "pipeline" -> BSONArray(
            BSONDocument(
              "$match" -> matchQuery
            ),
            BSONDocument(
              "$project" -> BSONDocument(
                "groups" -> "$groups",
                "environmentName" -> "$environmentName",
                "serviceAction" -> "$serviceAction",
                "atDate" -> "$atDate",
                "nbOfRequestData" -> "$nbOfRequestData",
                "ponderateAvg" -> BSONDocument("$multiply" -> BSONArray("$nbOfRequestData", "$avgInMillis"))
              )
            ),
            BSONDocument(
              "$group" -> BSONDocument(
                "_id" -> BSONDocument("groups" -> "$groups",
                  "environmentName" -> "$environmentName",
                  "serviceAction" -> "$serviceAction"
                ),
                "sumOfPonderate" -> BSONDocument(
                  "$sum" -> "$ponderateAvg"
                ),
                "totalRequest" -> BSONDocument(
                  "$sum" -> "$nbOfRequestData"
                )
              )
            ),
            BSONDocument(
              "$project" -> BSONDocument(
                "groups" -> "$groups",
                "serviceAction" -> "$serviceAction",
                "environment" -> "$environmentName",
                "totalAvg" -> BSONDocument(
                  "$divide" -> BSONArray(
                    "$sumOfPonderate",
                    "$totalRequest"
                  )
                )
              )
            )
          )
        )
    }

    // Perform the query
    val query = command.flatMap(c => db.command(RawCommand(c)))
    query.map { list =>
        // Decode the result
        var listRes = ListBuffer.empty[PageStat]
        list.elements.foreach {
          document =>
            if (document.name == "result") {
              // We retrieve the result element (contain all the results)
              document.value match {
                case array: BSONArray =>
                  // Each result is a BSONArray containing a list of BSONDocuments
                  array.values.foreach {
                    result =>
                      var sa = ""
                      var groups = ListBuffer.empty[String]
                      var realEnvironmentName = ""
                      var avg = 0.toLong
                      result match {
                        case document: BSONDocument =>
                          // Each BSONDocument is a statistics composed of the id (groups and service action) and the avg time
                          document.elements.foreach {
                            stat =>
                              if (stat.name == "_id") {
                                stat.value.asInstanceOf[BSONDocument].elements.foreach {
                                  groupsOrService =>
                                    if (groupsOrService._1 == "groups") {
                                      // For each element we retrieve its groups
                                      groupsOrService._2.asInstanceOf[BSONArray].values.foreach {
                                        group =>
                                          groups += group.asInstanceOf[BSONString].value
                                      }
                                    } else if (groupsOrService._1 == "serviceAction") {
                                      sa = groupsOrService._2.asInstanceOf[BSONString].value
                                    } else if (groupsOrService._1 == "environmentName") {
                                      realEnvironmentName = groupsOrService._2.asInstanceOf[BSONString].value
                                    }
                                }
                              } else if (stat._1 == "totalAvg") {
                                // We retrieve the average
                                avg = stat._2.asInstanceOf[BSONDouble].value.toLong
                              }
                          }
                        case _ =>
                      }
                      val pageStat = new PageStat(groups.toList, realEnvironmentName, sa, avg, serviceActions.apply((sa, groups.toList)))
                      listRes += pageStat
                  }
                case _ =>
              }
            }
        }
        listRes.toList
    }
  }

  case class AnalysisEntity(groups: List[String], environment: String, serviceAction: String, dateAndAvg: List[(Long, Long)])

  /**
    * Response times using mongoDB aggregation framework
    *
    * @param groups
    * @param environmentName
    * @param serviceAction
    * @param minDate
    * @param maxDate
    * @return
    */
  def findResponseTimes(groups: String, environmentName: String, serviceAction: String, minDate: Date, maxDate: Date): Future[List[AnalysisEntity]] = {
    var matchQuery = BSONDocument()
    var groupQuery = BSONDocument()

    if (groups != "all") {
      matchQuery = matchQuery ++ ("groups" -> BSONDocument("$in" -> groups.split(',')))
    }
    if (environmentName != "all") {
      matchQuery = matchQuery ++ ("environmentName" -> environmentName)
    }
    if (serviceAction != "all") {
      matchQuery = matchQuery ++ ("serviceAction" -> serviceAction)
    }

    groupQuery = groupQuery ++ ("groups" -> "$groups", "environmentName" -> "$environmentName", "serviceAction" -> "$serviceAction")

    // We remove 1000 millisecond to minDate to avoid issue with last two milliseconds being random
    // when mindate is set to yesterday
    matchQuery = matchQuery ++ ("atDate" -> BSONDocument(
      "$gte" -> BSONDateTime(minDate.getTime - 1000),
      "$lt" -> BSONDateTime(maxDate.getTime))
      )

    val command = collection.map { c =>
      BSONDocument(
        "aggregate" -> c.name, // we aggregate on collection
        "pipeline" -> BSONArray(
          BSONDocument(
            "$match" -> matchQuery
          ),
          BSONDocument(
            "$sort" -> BSONDocument(
              "groups" -> 1,
              "environmentName" -> 1,
              "serviceAction" -> 1,
              "atDate" -> 1
            )
          ),
          BSONDocument(
            "$group" -> BSONDocument(
              "_id" -> groupQuery,
              "dates" -> BSONDocument(
                "$push" -> "$atDate"
              ),
              "avgs" -> BSONDocument(
                "$push" -> "$avgInMillis"
              )
            )
          )
        )
      )
    }

    val query = command.flatMap(c => db.command(RawCommand(c)))
    query.map {
      list =>
        val resList = ListBuffer.empty[AnalysisEntity]
        list.elements.foreach {
          results =>
            if (results._1 == "result") {
              results._2.asInstanceOf[BSONArray].values.foreach {
                listOfStats =>
                  var groups = ListBuffer.empty[String]
                  var environment = ""
                  var serviceAction = ""
                  var dateAndAvg = ListBuffer.empty[(Long, Long)]
                  var dates = List.empty[Long]
                  var avgs = List.empty[Long]

                  listOfStats.asInstanceOf[BSONDocument].elements.foreach {
                    stat =>
                      if (stat._1 == "_id") {
                        stat._2.asInstanceOf[BSONDocument].elements.foreach {
                          id =>
                            if (id._1 == "groups") {
                              id._2.asInstanceOf[BSONArray].values.foreach {
                                group =>
                                  groups += group.asInstanceOf[BSONString].value
                              }
                            } else if (id._1 == "serviceAction") {
                              serviceAction = id._2.asInstanceOf[BSONString].value
                            } else if (id._1 == "environmentName") {
                              environment = id._2.asInstanceOf[BSONString].value
                            }
                        }
                      } else if (stat._1 == "dates") {
                        dates = stat._2.asInstanceOf[BSONArray].values.toList.map {
                          date =>
                            date.asInstanceOf[BSONDateTime].value
                        }
                      } else if (stat._1 == "avgs") {
                        avgs = stat._2.asInstanceOf[BSONArray].values.toList.map {
                          avg =>
                            avg.asInstanceOf[BSONLong].value
                        }
                      }
                  }
                  val size = avgs.size
                  for (i <- 0 to (size - 1)) {
                    dateAndAvg += ((dates.apply(i), avgs.apply(i)))
                  }
                  resList += new AnalysisEntity(groups.toList, environment, serviceAction, dateAndAvg.toList)
              }
            }
        }
        resList.toList
    }
  }

}

