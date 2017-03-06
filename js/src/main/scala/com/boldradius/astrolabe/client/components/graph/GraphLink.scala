package com.boldradius.astrolabe.client.components.graph

import com.boldradius.astrolabe.client.domain.{ ClusterGraphLink, ClusterGraphRoleLink }
import com.boldradius.astrolabe.client.modules.{ Members, Mode, Nodes, Roles }
import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.vdom.Attrs
import japgolly.scalajs.react.vdom.all.svg._
import japgolly.scalajs.react.vdom.prefix_<^._

object GraphLink {

  case class Props(link: ClusterGraphLink, key: Int, mode: Mode)

  val component = ReactComponentB[Props]("GraphLink")
    .render { scope =>

      scope.props.mode match {
        case Members =>
          line(
            Attrs.cls := "link",
            x1 := scope.props.link.source.x,
            y1 := scope.props.link.source.y,
            x2 := scope.props.link.target.x,
            y2 := scope.props.link.target.y,
            stroke := "#999",
            strokeOpacity := ".6",
            strokeWidth := "1",
            strokeDasharray := "5,5")
        case Roles =>

          val roleLink = scope.props.link.asInstanceOf[ClusterGraphRoleLink]

          line(
            Attrs.cls := "link",
            x1 := scope.props.link.source.x,
            y1 := scope.props.link.source.y,
            x2 := scope.props.link.target.x,
            y2 := scope.props.link.target.y,
            stroke := LegendColors.colors(roleLink.index % 5),
            strokeOpacity := "1",
            strokeWidth := "5")
        case Nodes =>
          line(
            Attrs.cls := "link",
            x1 := scope.props.link.source.x,
            y1 := scope.props.link.source.y,
            x2 := scope.props.link.target.x,
            y2 := scope.props.link.target.y,
            stroke := "#999",
            strokeOpacity := "1",
            strokeWidth := "1")

      }

    }.build

  def apply(link: ClusterGraphLink, key: Int, mode: Mode) = component(Props(link, key, mode))
}