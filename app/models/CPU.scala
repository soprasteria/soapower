package models

import java.lang.management._

import com.sun.management.OperatingSystemMXBean

/*
* Code from https://github.com/playframework/Play20/tree/master/samples/scala/comet-live-monitoring
*/
class CPU {
  private val operatingSystemMXBean = ManagementFactory.getOperatingSystemMXBean
  private val availableProcessors = operatingSystemMXBean.getAvailableProcessors
  private var lastSystemTime = 0L
  private var lastProcessCpuTime = 0L

  def cpuUsage(): Double =
    if (lastSystemTime == 0L) {
      baselineCounters()
      0
    } else {
      val systemTime = System.nanoTime
      val cpuTime = processCpuTime().getOrElse(0L)
      val cpuUsage = (cpuTime - lastProcessCpuTime).toDouble / (systemTime - lastSystemTime)
      lastSystemTime = systemTime
      lastProcessCpuTime = cpuTime
      cpuUsage / availableProcessors
    }

  private def baselineCounters(): Unit = {
    lastSystemTime = System.nanoTime
    processCpuTime().foreach(cpuTime => lastProcessCpuTime = cpuTime)
  }

  private def processCpuTime(): Option[Long] =
    operatingSystemMXBean match {
      case bean: OperatingSystemMXBean => Some(bean.getProcessCpuTime)
      case _ => None
    }
}