package util

import scala.concurrent.{ExecutionContext, Future}

object FutureOps {

  /**
    * Pimp My Library class for Future
    */
  implicit class RichFuture[T](val future: Future[T]) extends AnyVal {

    /**
      * If future is successful, will return Future(Some(value))
      */
    def toFutureOption(implicit context: ExecutionContext): Future[Option[T]] = future.map(result => Some(result))

  }

}
