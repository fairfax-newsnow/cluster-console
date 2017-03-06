package com.boldradius.astrolabe.client.components

import com.boldradius.astrolabe.client.services.{ ClusterService, Logger }
import com.boldradius.astrolabe.client.style.Bootstrap.{ Button, Modal }
import com.boldradius.astrolabe.client.style.Icon
import com.boldradius.astrolabe.http._
import japgolly.scalajs.react.vdom.all._
import japgolly.scalajs.react._
import Logger._
import org.scalajs.dom.raw.{ HTMLOptionElement, HTMLSelectElement }

object RolesFormComponent {

  case class Props(cluster: DiscoveredCluster, closeForm: Callback)

  case class State(
    dependencies: Seq[RoleDependency],
    roles: Seq[String],
    selectedRoles: Seq[String],
    dependsOnRoles: Seq[String],
    dependencyName: Option[String],
    dependencyType: ClusterDependency)

  class Backend(t: BackendScope[Props, State]) {

    def hide(): Callback =
      for {
        _ ← Callback.log("RolesFormComponent hide()")
        props ← t.props
        _ ← props.closeForm
      } yield {}

    def selectRole(e: ReactMouseEvent): Callback = {
      val select = e.currentTarget.asInstanceOf[HTMLSelectElement]
      t.state flatMap { state ⇒
        val options: Seq[HTMLOptionElement] = state.roles.indices.map(i => select.childNodes(i).asInstanceOf[HTMLOptionElement])

        val selectedRoles = options.filter(_.selected).flatMap(r => state.roles.find(e => e == r.value))

        t.modState(_.copy(selectedRoles = selectedRoles))
      }
    }

    def dependsOnRole(e: ReactMouseEvent): CallbackTo[Unit] = {
      val select = e.currentTarget.asInstanceOf[HTMLSelectElement]

      t.state.flatMap { state ⇒
        log.debug("dependsOnRole select: " + select)

        val options: Seq[HTMLOptionElement] = state.roles.filter(e =>
          !state.selectedRoles.contains(e)).indices.map(i =>
          select.childNodes(i).asInstanceOf[HTMLOptionElement])

        val dependsOnRoles = options.filter(_.selected).flatMap(r => state.roles.find(e => e == r.value))

        t.modState(_.copy(dependsOnRoles = dependsOnRoles))
      }
    }

    def selectType(e: ReactMouseEvent): CallbackTo[Unit] = {
      val select = e.currentTarget.asInstanceOf[HTMLSelectElement]

      t.state.flatMap { state ⇒
        t.modState(_.copy(dependencyType = {
          select.selectedIndex match {
            case 0 => DistributedRouter(state.dependencyName.getOrElse(""))
            case 1 => ClusterSharded(state.dependencyName.getOrElse(""))
            case 2 => Manual(state.dependencyName.getOrElse(""))
            case _ => Manual(state.dependencyName.getOrElse(""))
          }

        }))
      }
    }

    def updateDepName(e: ReactEventI): Callback = {
      val depName = e.currentTarget.value

      t.modState(_.copy(dependencyName = {
        if (depName.length > 0) {
          log.debug("--- " + depName)
          Some(depName)
        } else {
          None
        }
      }))
    }

    def canSubmit: CallbackTo[Boolean] =
      t.state.map(state ⇒
        state.selectedRoles.nonEmpty &&
          state.dependsOnRoles.nonEmpty &&
          state.dependencyName.isDefined
      )

    def addDep(e: ReactMouseEvent): Callback = {
      e.preventDefault()

      val result =
        t.state.flatMap(state ⇒
          state.dependencyName match {
            case Some(depName) ⇒
              val dependency =
                RoleDependency(state.selectedRoles, state.dependsOnRoles, state.dependencyType.updateName(depName))

              t.modState(_.copy(dependencies = state.dependencies :+ dependency))
            case None ⇒
              Callback {}
          }
        )

      result.void
    }

    def submitForm(): Callback = {
      val callback = t.props.flatMap { props ⇒
        t.state.flatMap { state ⇒
          ClusterService.updateClusterDependencies(props.cluster.copy(dependencies = state.dependencies))
          props.closeForm
        }
      }

      callback.void
    }
  }

  val component = ReactComponentB[Props]("DiscoveringClusterComponent")
    .initialState_P(props =>
      State(props.cluster.dependencies, props.cluster.getRoles, Seq.empty[String], Seq.empty[String], None, DistributedRouter(""))
    )
    .backend(new Backend(_))
    .render(scope =>
      Modal(Modal.Props(
        header = be => span(button(tpe := "button", cls := "pull-right", onClick --> be.hide(), Icon.close), h4(color := "black")("Describe Dependencies")),
        footer = be => span(Button(Button.Props(
          scope.backend.submitForm().>>(be.hide())
        ), "OK")),
        closed = scope.props.closeForm),

        if (scope.state.dependencies.nonEmpty) {
          div(cls := "row")(
            div(cls := "col-md-12")(
              div(cls := "panel panel-primary")(
                div(cls := "panel-heading")("Existing Dependencies"),
                div(cls := "panel-body")(
                  scope.state.dependencies.map(d =>
                    div(
                      span(d.tpe.name + ": "),
                      span(d.tpe.typeName + ": "),
                      span(d.roles.mkString(",")),
                      span("-->"),
                      span(d.dependsOn.mkString(",")))
                  )
                )
              )
            )
          )
        } else {
          span("")
        },
        div(cls := "row")(
          div(cls := "col-md-12")(
            div(cls := "panel panel-primary")(
              div(cls := "panel-heading")("Add Dependencies"),
              div(cls := "panel-body")(
                form(
                  div(cls := "row")(
                    div(cls := "form-group col-md-4")(
                      label("Roles(s)"),
                      select(name := "selectRole", multiple := "multiple", cls := "form-control", height := {
                        (scope.state.roles.length * 20) + "px"
                      }, onChange ==> scope.backend.selectRole)(
                        scope.state.roles.map(r => option(value := r)(r))
                      )
                    ),
                    div(cls := "form-group col-md-3")(
                      label("Depend(s) On")
                    ),

                    scope.state.selectedRoles.headOption.map(selectedRole =>
                      div(cls := "form-group col-md-4")(
                        label("Role(s)"),
                        select(name := "dependsOnRole", multiple := "multiple", cls := "form-control", height := {
                          (scope.state.roles.length * 20) + "px"
                        }, onChange ==> scope.backend.dependsOnRole)(
                          scope.state.roles.filter(e => !scope.state.selectedRoles.contains(e)).map(r => option(value := r)(r))
                        )
                      )
                    ).getOrElse(span(""))
                  ),
                  div(cls := "row", paddingTop := "20px")(
                    div(cls := "col-md-12")(
                      div(cls := "row")(
                        scope.state.selectedRoles.headOption.map(selectedRole =>
                          div(cls := "col-md-7")(
                            div(cls := "col-md-8")(scope.state.selectedRoles.mkString(",")),
                            div(cls := "col-md-4")("-->")
                          )
                        ).getOrElse(EmptyTag),
                        scope.state.dependsOnRoles.headOption.map(dependsOnRole =>
                          div(cls := "col-md-5")(
                            span(scope.state.dependsOnRoles.mkString(",")))
                        ).getOrElse(EmptyTag)
                      )
                    )
                  ),
                  div(cls := "row", paddingTop := "20px")(
                    div(cls := "form-group col-md-9")(
                      label("Dependency Name"),
                      input(tpe := "text", cls := "form-control", onChange ==> scope.backend.updateDepName)
                    ),
                    div(cls := "form-group col-md-4")(
                      label("Dependency Type"),
                      select(onChange ==> scope.backend.selectType)(
                        option("DistributedRouter"),
                        option("ClusterSharded"),
                        option("Manual")
                      )
                    )
                  ),
                  div(cls := "row")(
                    div(cls := "col-md-12")(
                      button(cls := "btn btn-submit", onClick ==> scope.backend.addDep, disabled := scope.backend.canSubmit.runNow())(
                        "Add dependency")
                    )
                  )
                )
              )
            )
          )
        )
      )
    ).build

  def apply(cluster: DiscoveredCluster, closeForm: Callback): ReactComponentU[Props, State, Backend, TopNode] =
    component(Props(cluster, closeForm))

}
