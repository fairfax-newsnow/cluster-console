package com.boldradius.astrolabe.client.components

import com.boldradius.astrolabe.client.modules.{ Roles, Mode, RxObserver }
import com.boldradius.astrolabe.client.services.ClusterService
import com.boldradius.astrolabe.client.style.GlobalStyles
import com.boldradius.astrolabe.http.DiscoveredCluster
import japgolly.scalajs.react.extra.OnUnmount
import japgolly.scalajs.react.vdom.all._
import japgolly.scalajs.react._
import com.boldradius.astrolabe.client.services.Logger._
import rx._

object DiscoveredClusterComponent {

  @inline private def globalStyles = GlobalStyles

  case class Props(discovered: Rx[Map[String, DiscoveredCluster]], selected: Rx[Option[DiscoveredCluster]], mode: Mode)

  case class State(rolesOpen: Option[String])

  class Backend(t: BackendScope[Props, State]) extends RxObserver(t) {

    def mounted(): Callback = {
      t.props.map { props ⇒
        observe(props.discovered)
        observe(props.selected)
      }
        .void
    }

    def selectCluster(e: ReactMouseEvent): Callback = Callback {
      ClusterService.selectCluster(e.currentTarget.firstChild.nodeValue)
      e.preventDefault()
    }

    def roles(system: String): Callback =
      Callback.log("open roles " + system).flatMap(_ ⇒
        t.modState(_.copy(rolesOpen = Some(system)))
      )

    def closeRolesForm(): Callback =
      Callback.log("closeRolesForm").flatMap(_ ⇒
        t.modState(_.copy(rolesOpen = None))
      )

  }

  val component = ReactComponentB[Props]("DiscoveredClusterComponent")
    .initialState(State(None)) // initial state
    .backend(new Backend(_))
    .render(scope => {
      log.debug("************* S.rolesOpen " + scope.state.rolesOpen)

      div(paddingTop := "30px")(
        if (scope.props.discovered().isEmpty) {
          span("")
        } else {
          div(cls := "row", height := "200px")(
            scope.state.rolesOpen.flatMap(role =>
              scope.props.selected().map(cluster =>
                RolesFormComponent(cluster, scope.backend.closeRolesForm())
              )
            ).getOrElse[ReactElement](span("")),
            div(cls := "col-md-12")(
              div(cls := "row", borderBottom := "1px solid white")(
                div(cls := "col-md-12")(
                  span(fontSize := "20px", color := globalStyles.textColor)("Discovered"))),
              div(cls := "row")(
                scope.props.discovered().values.map(e =>

                  if (isSelected(scope.props, e.system) && scope.props.mode == Roles) {
                    div(cls := "col-md-12", paddingTop := "10px", paddingBottom := "10px",
                      backgroundColor := selectedBackground(scope.props, e.system))(
                        a(href := "", key := e.system, fontSize := "18px")(
                          span(onClick ==> scope.backend.selectCluster,
                            color := selectedColor(scope.props, e.system))(e.system)
                        ), span(cls := "pull-right")(button(cls := "btn btn-small", onClick --> scope.backend.roles(e.system))("Roles"))
                      )

                  } else {
                    div(cls := "col-md-12", paddingTop := "10px", paddingBottom := "10px",
                      backgroundColor := selectedBackground(scope.props, e.system))(
                        a(href := "", key := e.system, fontSize := "18px")(
                          span(onClick ==> scope.backend.selectCluster,
                            color := selectedColor(scope.props, e.system))(e.system)
                        )
                      )

                  }
                )
              )
            )
          )
        }
      )

    }
    ).componentDidMount(_.backend.mounted())
    .configure(OnUnmount.install)
    .build

  def selectedColor(props: Props, system: String) =
    if (isSelected(props, system)) {
      globalStyles.textColor
    } else {
      globalStyles.navUnselectedTextColor
    }

  def selectedBackground(props: Props, system: String) =
    if (isSelected(props, system)) {
      "#6A777B"
    } else {
      ""
    }

  def isSelected(props: Props, system: String): Boolean =
    props.selected().exists(_.system == system)

  def apply(discovered: Rx[Map[String, DiscoveredCluster]],
    selected: Rx[Option[DiscoveredCluster]],
    mode: Mode) = component(Props(discovered, selected, mode))

}
