package com.boldradius.astrolabe.client.components.graph

import com.boldradius.astrolabe.client.style.GlobalStyles
import com.boldradius.astrolabe.http.RoleDependency
import japgolly.scalajs.react.vdom.all.svg._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{ ReactComponentB, _ }

object ClusterDependencyLegend {

  case class Props(dep: RoleDependency, index: Int, selected: Boolean, selectDep: (RoleDependency, Boolean) => Callback)

  case class State(selected: Boolean)

  case class Backend(t: BackendScope[Props, State]) {
    def select: Callback = {
      (t.state flatMap { state ⇒
        t.props flatMap { props ⇒
          t.modState(_.copy(selected = !state.selected))
          props.selectDep(props.dep, !state.selected)
        }
      })
        .void
    }
  }

  val component = ReactComponentB[Props]("ClusterDependencyLegend")
    .initialState_P(props => State(props.selected))
    .backend(Backend)
    .render { componentScope =>
      val label =
        s"${componentScope.props.dep.tpe.name}:  ${componentScope.props.dep.tpe.typeName}. " +
          s"${componentScope.props.dep.roles.mkString(",")} " +
          s"--> ${componentScope.props.dep.dependsOn.mkString(",")}"

      val rectwidth = (label.length * 9) + 20

      g({
        import japgolly.scalajs.react.vdom.all._
        onClick --> componentScope.backend.select
      })(rect(width := rectwidth.toString, height := "40", fill := {
        if (componentScope.state.selected) {
          LegendColors.colors(componentScope.props.index % 5)
        } else {
          GlobalStyles.leftNavBackgrounColor
        }
      }, x := 0, y := (componentScope.props.index * 45) + 5, stroke := GlobalStyles.textColor),

        text(x := 10, y := (componentScope.props.index * 45) + 30,
          fill := GlobalStyles.textColor, fontSize := "15px", fontFamily := "Courier")(label)
      )
    }.build

  def apply(dep: RoleDependency, index: Int, selected: Boolean, selectDep: (RoleDependency, Boolean) => Callback) =
    component(Props(dep, index, selected, selectDep))
}
