package com.boldradius.astrolabe.client.services

import com.boldradius.astrolabe.http.ClusterProtocol
import org.scalajs.dom
import org.scalajs.dom.raw._
import upickle.default._
import com.boldradius.astrolabe.http.Json._

object WebSocketClient {

  var open: Boolean = false

  lazy val websocket = new WebSocket(getWebsocketUri(dom.document))

  websocket.onopen = { (event: Event) =>
    println(s"Websocket connection established $event")
    ClusterService.findDiscoveringClusters()
    ClusterService.findDiscoveredClusters()
    event
  }
  websocket.onerror = { (event: ErrorEvent) => }

  websocket.onmessage = { (event: MessageEvent) =>
    println(s"Websocket message received $event")
    val msg: ClusterProtocol = read[ClusterProtocol](event.data.toString)
    MainDispatcher.dispatch(msg)
    event
  }

  websocket.onclose = { (event: Event) => }

  def getWebsocketUri(document: Document): String = {
    val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"

    s"$wsProtocol://${dom.document.location.host}/events"
  }

  def send(msg: ClusterProtocol): Unit = {
    websocket.send(write(msg))
  }

}
