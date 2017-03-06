package com.boldradius.astrolabe.client.modules

import com.boldradius.astrolabe.client.ClusterConsoleApp.{ ClusterMapLoc, DashboardLoc, Loc }
import com.boldradius.astrolabe.client.style.Icon.Icon
import com.boldradius.astrolabe.client.style.{ GlobalStyles, Icon }
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.OnUnmount
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.all._

import scalacss.ScalaCssReact._

object MainMenu {
  // shorthand for styles
  @inline private def bss = GlobalStyles.bootstrapStyles

  case class Props(ctl: RouterCtl[Loc], currentLoc: Loc)

  case class MenuItem(label: (Props) => ReactNode, icon: Icon, location: Loc)

  case class Backend(t: BackendScope[Props, _]) extends OnUnmount {
    def mounted(): Callback = Callback {
    }
  }

  private val menuItems = Seq(
    MenuItem(_ => "Dashboard", Icon.dashboard, DashboardLoc),
    MenuItem(_ => "ClusterMap", Icon.circle, ClusterMapLoc)
  )

  private val MainMenu = ReactComponentB[Props]("MainMenu")
    .stateless
    .backend(Backend)
    .render(scope => {
      ul(bss.navbar)(
        // build a list of menu items
        for (item <- menuItems) yield {
          li((scope.props.currentLoc == item.location) ?= (className := "active"),
            scope.props.ctl.link(item.location)(item.icon, " ", item.label(scope.props))
          )
        }
      )
    })
    .componentDidMount(_.backend.mounted())
    .build

  def apply(props: Props) = MainMenu(props)
}
