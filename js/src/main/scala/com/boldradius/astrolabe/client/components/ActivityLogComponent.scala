package com.boldradius.astrolabe.client.components

import com.boldradius.astrolabe.client.modules.RxObserver
import com.boldradius.astrolabe.client.services.ActivityLogService
import com.boldradius.astrolabe.client.style.GlobalStyles
import com.boldradius.astrolabe.http._
import japgolly.scalajs.react.extra.OnUnmount
import japgolly.scalajs.react.vdom.all._
import japgolly.scalajs.react.{ ReactMouseEvent, BackendScope, ReactComponentB }
import rx._
import japgolly.scalajs.react._

object ActivityLogComponent {

  @inline private def globalStyles = GlobalStyles

  case class Props(activities: Rx[Seq[ClusterEvent]])

  case class State(logItems: Seq[ClusterEvent] = Seq.empty)

  def apply(service: ActivityLogService) = {
    component(Props(service.activities))
  }

  class Backend(t: BackendScope[Props, State]) extends RxObserver(t) {
    def mounted(): Callback =
      t.props.map(props ⇒
        observe(props.activities)
      )
      // MainDispatcher.dispatch(RefreshClusterMembers)

    def select(e: ReactMouseEvent) = Callback {
      e.preventDefault()
    }

  }

  val component = ReactComponentB[Props]("ActivityLog")
    .initialState(State())
    .backend(new Backend(_))
    .render(scope => {
      div(paddingTop := "30px")(
        div(cls := "row", height := "200px")(
          div(cls := "col-md-12", maxHeight := "600px", overflowY := "auto")(
            div(cls := "row", borderBottom := "1px solid white")(
              div(cls := "col-md-12")(
                span(fontSize := "20px", color := globalStyles.textColor)("Events"))
            ),
            div(
              scope.props.activities().map { e =>

                val (bg, tcolor) = e match {
                  case ev: ClusterMemberUp => (globalStyles.nodeUpColor, "white")
                  case ev: ClusterMemberUnreachable => (globalStyles.nodeUnreachableColor, "white")
                  case ev: ClusterMemberRemoved => (globalStyles.nodeRemovedColor, "white")
                  case ev: ClusterMemberExited => (globalStyles.nodeRemovedColor, "white")
                  case ev: ClusterMetricCPU => (globalStyles.metricsColor, "white")
                  case ev: ClusterMetricMemory => (globalStyles.metricsColor, "white")
                }
                div(cls := "row", borderTop := "1px solid white", borderBottom := "1px solid white")(
                  div(cls := "col-md-12", paddingTop := "10px", paddingBottom := "10px",
                    backgroundColor := bg, color := tcolor)(
                      span(color := tcolor, fontSize := "15px")(
                        b(
                          ClusterEventUtil.label(e)
                        )
                      )
                    )
                )
              }
            )
          )
        )
      )
    })
    .componentDidMount(scope ⇒ scope.backend.mounted())
    .configure(OnUnmount.install)
    .build

}

