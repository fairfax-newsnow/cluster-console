package com.boldradius.astrolabe.client.modules

import com.boldradius.astrolabe.client.ClusterConsoleApp.Loc
import com.boldradius.astrolabe.client.components._
import com.boldradius.astrolabe.client.services.Logger._
import com.boldradius.astrolabe.client.services.{ ActivityLogService, ClusterService }
import com.boldradius.astrolabe.client.style.{ GlobalStyles, Icon }
import com.boldradius.astrolabe.http.{ ClusterForm, DiscoveredCluster, HostPortUtil }
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.OnUnmount
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.all._

import scalacss.ScalaCssReact._

sealed trait Mode {
  val name: String
  def isActive(m: Mode): Boolean = this == m

}
object Mode {
  def fromString(s: String) =
    s match {
      case Members.name => Members
      case Roles.name => Roles
      case Nodes.name => Nodes
      case _ => Members
    }
}
case object Members extends Mode {
  val name = "Members"
}
case object Roles extends Mode {
  val name = "Roles"
}
case object Nodes extends Mode {
  val name = "Nodes"
}

object ClusterMap {

  @inline private def globalStyles = GlobalStyles

  case class Props(store: ClusterService, router: RouterCtl[Loc])

  case class State(showClusterForm: Boolean, mode: Mode)

  case class Backend(t: BackendScope[Props, State]) extends RxObserver(t) {
    def mounted(): Callback =
      Callback { ClusterService.getDiscoveredClusters() }

    def editCluster(cForm: ClusterForm): Callback = Callback {
      ClusterService.subscribeToCluster(cForm.name, cForm.selfHost, cForm.seeds.map(HostPortUtil.apply))
    }

    def selectCluster(name: String) = Callback {
      ClusterService.selectCluster(name)
    }

    def showClusterForm(show: Boolean): Callback =
      Callback.log("called showClusterForm")
        .flatMap(_ ⇒ t.modState(_.copy(showClusterForm = show)))
        .void

    def closeClusterForm: Callback = showClusterForm(false)

    def changeMode(e: ReactMouseEvent) = {
      e.preventDefault()
      val element = e.currentTarget

      t.modState(_.copy(mode = Mode.fromString(
        element.childNodes.item(0).childNodes.item(0).childNodes.item(0).nodeValue),
        showClusterForm = false
      ))
    }

  }

  // create the React component for Clusters mgmt
  val component = ReactComponentB[Props]("Clusters")
    .initialState(State(false, Members)) // initial state from TodoStore
    .backend(Backend)
    .render(scope => {
      log.debug("*** showClusterForm  " + scope.state.showClusterForm)

      val toolBar: ReactElement =
        div(cls := "row", globalStyles.mainHeaders)(
          div(cls := "col-md-8")(h3("Clusters")),
          div(cls := "col-md-4")(button(cls := "pull-right btn-lg", marginTop := "9px", name := "showClusterForm",
            tpe := "button", onClick --> scope.backend.showClusterForm(true))(Icon.plus)))

      val modal: Seq[ReactElement] = if (scope.state.showClusterForm) Seq(ClusterFormComponent(scope.props.store,
        scope.backend.editCluster,
        scope.backend.closeClusterForm))
      else Seq.empty[ReactElement]

      val leftNav: Seq[ReactElement] = toolBar +: (Seq(DiscoveringClusterComponent(scope.props.store.getDiscoveringClusters),
        DiscoveredClusterComponent(scope.props.store.getDiscoveredClusters, scope.props.store.getSelectedCluster, scope.state.mode),
        ActivityLogComponent(ActivityLogService)
      ) ++ modal)

      def renderModeButton(m: Mode) =
        if (scope.state.mode.isActive(m)) {
          li(cls := "active", onClick ==> scope.backend.changeMode,
            globalStyles.mainHeaders, borderTop := "1px solid white")(
              a(href := "", globalStyles.mainHeaders, backgroundColor := globalStyles.mapBackground)(
                span(color := globalStyles.textColor)(fontSize := "24px")(m.toString)
              )
            )

        } else {
          li(onClick ==> scope.backend.changeMode, globalStyles.mainHeaders)(
            a(href := "", globalStyles.mainHeaders)(
              span(fontSize := "24px")(m.toString)
            )
          )

        }
      val clusterMapToolBar =
        div(cls := "row", globalStyles.mainHeaders)(
          ul(cls := "nav nav-tabs")(
            renderModeButton(Members),
            renderModeButton(Roles),
            renderModeButton(Nodes)
          )
        )

      div(cls := "row")(
        div(cls := "col-md-2", globalStyles.leftNav, height := "100%")(
          leftNav
        ),
        div(cls := "col-md-10")(
          clusterMapToolBar,
          div(cls := "row")(
            div(cls := "col-md-12")(
              ClusterNodeGraphComponent(scope.props.store, scope.state.mode)
            )
          )
        )
      )
    })
    .componentDidMount(_.backend.mounted())
    .configure(OnUnmount.install)
    .build

  /** Returns a function with router location system while using our own props */
  def apply(store: ClusterService) = (router: RouterCtl[Loc]) => {
    component(Props(store, router))
  }

}
