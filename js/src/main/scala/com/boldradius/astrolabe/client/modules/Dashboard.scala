package com.boldradius.astrolabe.client.modules

import com.boldradius.astrolabe.client.ClusterConsoleApp.Loc
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.all._
import japgolly.scalajs.react.{ ReactComponentB, _ }

object Dashboard {
  // create the React component for Dashboard
  val component = ReactComponentB[RouterCtl[Loc]]("Dashboard")
    .render(router => {
      div(cls := "container", paddingTop := "6px")(
        div(cls := "col-md-12")(
          span(color := "white", fontSize := "15px")(
            b(
              "Dashboard. Needs some content."
            )
          )
        )
      )
    }).build
}
