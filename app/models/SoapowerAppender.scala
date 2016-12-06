package models

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.UnsynchronizedAppenderBase
import controllers.admin.Streams

/**
  * Soapower logging appender.
  * Inspired of https://www.devbliss.com/en/comet-style-log-tailing-in-play-2-0/
  */
class SoapowerAppender extends UnsynchronizedAppenderBase[ILoggingEvent] {
  protected def append(event: ILoggingEvent): Unit = Streams.pushLog(event)
}