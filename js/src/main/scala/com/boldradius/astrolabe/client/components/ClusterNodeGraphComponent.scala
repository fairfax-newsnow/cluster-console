package com.boldradius.astrolabe.client.components

import com.boldradius.astrolabe.client.components.graph.Graph
import com.boldradius.astrolabe.client.modules.{ Mode, RxObserver }
import com.boldradius.astrolabe.client.services.ClusterService
import com.boldradius.astrolabe.http.DiscoveredCluster
import japgolly.scalajs.react.extra.OnUnmount
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom._
import japgolly.scalajs.react.vdom.all._

object ClusterNodeGraphComponent {

  case class Props(store: ClusterService, mode: Mode)

  case class State(cluster: Option[DiscoveredCluster])

  class Backend(t: BackendScope[Props, State]) extends RxObserver(t) {
    def mounted(): Callback = {
      t.props.map(props ⇒
        react(props.store.getSelectedCluster, (value: Option[DiscoveredCluster]) ⇒ updateCluster(value))
      )
    }

    def updateCluster(maybeCluster: Option[DiscoveredCluster]): CallbackTo[CallbackTo.MapGuard[Unit]#Out] = {
      t.state map { state ⇒
        val current = state.cluster

        current match {
          case None => maybeCluster.fold[Unit]({})(c => t.modState(_.copy(cluster = maybeCluster)))
          case Some(c) => maybeCluster.fold[Unit](t.modState(_.copy(cluster = None)))(newC =>
            if (c.system != newC.system) {
              t.modState(_.copy(cluster = maybeCluster))
            })
        }
      }
    }

  }

  def component = ReactComponentB[Props]("ClusterNodeGraph")
    .initialState_P(props ⇒ State(props.store.getSelectedCluster()))
    .backend(new Backend(_))
    .render(scope =>
      scope.state.cluster.fold(div(""))(cluster =>
        div(
          Graph(cluster.system, scope.props.mode, 1400, 900, scope.props.store, fixedMap = false)
        )
      )
    )
    .componentDidMount(_.backend.mounted())
    .configure(OnUnmount.install)
    .build

  def apply(store: ClusterService, mode: Mode): ReactComponentU[Props, State, Backend, TopNode] = component(Props(store, mode))

}
