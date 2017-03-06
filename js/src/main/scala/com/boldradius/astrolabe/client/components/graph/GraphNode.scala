package com.boldradius.astrolabe.client.components.graph

import com.boldradius.astrolabe.client.d3.Layout.ForceLayout
import com.boldradius.astrolabe.client.domain.ClusterGraphNode
import com.boldradius.astrolabe.client.modules.{ Mode, Nodes }
import com.boldradius.astrolabe.client.style.GlobalStyles
import japgolly.scalajs.react.vdom.Attrs
import japgolly.scalajs.react.vdom.all.svg._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{ ReactComponentB, ReactNode, _ }

object GraphNode {

  case class Props(node: ClusterGraphNode, force: ForceLayout, mode: Mode)

  case class State(selected: Boolean)

  case class Backend(t: BackendScope[Props, State]) {
    def select: Callback = t.state.flatMap(state ⇒ t.modState(_.copy(selected = !state.selected))).void
  }

  val component = ReactComponentB[Props]("GraphNode")
    .initialState(State(false))
    .backend(Backend)
    .render { scope =>
      g(
        circle(Attrs.cls := "node", Attrs.id := scope.props.node.index, r := getRadius(scope.props.mode, scope.props.node),
          cx := scope.props.node.x, cy := scope.props.node.y,
          fill := {

            if (scope.state.selected) {
              "#EEE"
            } else {
              scope.props.node.status match {
                case "Up" => GlobalStyles.nodeUpColor
                case "Unreachable" => GlobalStyles.nodeUnreachableColor
                case "Removed" => GlobalStyles.nodeRemovedColor
                case "Exited" => GlobalStyles.nodeRemovedColor
                case _ => GlobalStyles.nodeUpColor
              }
            }
          }, stroke := "#fff", strokeWidth := "1.px5"
        //        , {
        //            import japgolly.scalajs.react.vdom.all._
        //            onClick --> B.select
        //
        //          }
        ),
        getTextNodes(scope.props.mode, scope.props.node)
      )

    }.build

  def getRadius(mode: Mode, n: ClusterGraphNode): String = mode match {
    case Nodes =>
      if (n.port == 0) {
        "50"
      } else {
        "20"
      }
    case _ => "30"
  }

  def getTextNodes(mode: Mode, n: ClusterGraphNode): ReactNode = mode match {
    case Nodes =>
      if (n.port == 0) {
        g(
          text(x := n.x - 40, y := n.y - 55, fill := "white", fontSize := "18px")(n.host)
        )

      } else {
        g(
          text(x := n.x - 30, y := n.y - 55, fill := "white", fontSize := "18px")(n.port),
          text(x := n.x - 30, y := n.y - 35, fill := "#D5EFD5", fontSize := "18px")(n.roles)
        )
      }
    case _ => g(
      text(x := n.x - 30, y := n.y - 55, fill := "white", fontSize := "18px")(n.host + ":" + n.port),
      text(x := n.x - 30, y := n.y - 35, fill := "#D5EFD5", fontSize := "18px")(n.roles)
    )
  }

  def apply(node: ClusterGraphNode, force: ForceLayout, mode: Mode) = component(Props(node, force, mode))
}