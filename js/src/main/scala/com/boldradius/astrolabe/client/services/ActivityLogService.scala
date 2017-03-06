package com.boldradius.astrolabe.client.services

import com.boldradius.astrolabe.client.ukko.Actor
import com.boldradius.astrolabe.http.{ ClusterEvent, ClusterProtocol }
import com.boldradius.astrolabe.client.services.Logger._
import rx._

object ActivityLogService extends ActivityLogService {

  def init = {
    println("init ActivityLogService")
    MainDispatcher.register(this)
  }

}

trait ActivityLogService extends Actor {

  private val logItems = Var(Seq.empty[ClusterEvent])

  def activities: Rx[Seq[ClusterEvent]] = logItems

  def name = "ActivityLogService"

  def receive: Receive = {
    case ac: ClusterEvent =>
      println(s"ActivityLogService: $ac")
      logItems() = ac +: logItems()

    case other =>
  }
}
