package com.boldradius.astrolabe.client

import com.boldradius.astrolabe.client.modules.{ ClusterMap, Dashboard, MainMenu }
import com.boldradius.astrolabe.client.services.Logger._
import com.boldradius.astrolabe.client.services._
import com.boldradius.astrolabe.client.style.GlobalStyles
import japgolly.scalajs.react.{ React, ReactDOM }
import japgolly.scalajs.react.extra.router.RouterConfigDsl
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.all._
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scalacss.Defaults._
import scalacss.ScalaCssReact._

@JSExport("ClusterConsoleApp")
object ClusterConsoleApp extends js.JSApp {

  // Define the locations (pages) used in this application
  sealed trait Loc

  case object DashboardLoc extends Loc
  case object ClusterMapLoc extends Loc

  val cs = ClusterService

  // configure the router
  val routerConfig = RouterConfigDsl[Loc].buildConfig { dsl =>
    import dsl._

    (staticRoute(root, DashboardLoc) ~> renderR(ctl => Dashboard.component(ctl))
      | staticRoute("#clustermap", ClusterMapLoc) ~> renderR(ctl => ClusterMap(cs)(ctl))
    ).notFound(redirectToPage(DashboardLoc)(Redirect.Replace))
  }.renderWith(layout)

  // base layout for all pages
  def layout(c: RouterCtl[Loc], r: Resolution[Loc]) = {
    div(
      // here we use plain Bootstrap class names as these are specific to the top level layout defined here
      nav(className := "navbar navbar-inverse navbar-fixed-top")(
        div(className := "container-fluid")(
          div(className := "navbar-header")(span(className := "navbar-brand")("Cluster Console")),
          div(className := "collapse navbar-collapse")(
            MainMenu(MainMenu.Props(c, r.page))
          )
        )
      ),
      // currently active module is shown in this container
      div(className := "container-fluid")(r.render())
    )
  }

  @JSExport
  def main(): Unit = {

    log.info(s"Application starting ${ReactDOM}")

    ActivityLogService.init

    // create stylesheet
    GlobalStyles.addToDocument()

    // create the router
    val router = Router(BaseUrl(dom.window.location.href.takeWhile(_ != '#')), routerConfig)
    // tell React to render the router in the document body
    ReactDOM.render(router(), dom.document.body)

    WebSocketClient.websocket

  }

}
