package com.boldradius.astrolabe.client.components

import com.boldradius.astrolabe.client.services.ClusterService
import com.boldradius.astrolabe.client.style.Bootstrap.{ Button, Modal }
import com.boldradius.astrolabe.client.style.{ GlobalStyles, Icon }
import com.boldradius.astrolabe.http.ClusterForm
import japgolly.scalajs.react.vdom.all._
import japgolly.scalajs.react._

object ClusterFormComponent {

  // shorthand for styles
  @inline private def bss = GlobalStyles.bootstrapStyles

  case class EditClusterProps(clusterForm: ClusterForm, editHandler: ClusterForm => Callback, closeForm: Callback)

  case class State(clusterForm: ClusterForm, seeds: Int, portValid: Boolean, submitEnabled: Boolean)

  case class Backend(t: BackendScope[EditClusterProps, State]) {

    def updateClusterForm(newForm: ClusterForm) = {
      ClusterService.updateClusterForm(newForm)
    }

    def updateClusterName(e: ReactEventI): Callback = {
      val nameValue = e.currentTarget.value

      t.modState { s =>
        val newState = s.copy(clusterForm = ClusterForm(nameValue, s.clusterForm.selfHost, s.clusterForm.seeds))
        updateClusterForm(newState.clusterForm)
        newState.copy(submitEnabled = getSubmitEnabled(newState))
      }
    }

    def updateClusterSelfHost(e: ReactEventI): Callback = {
      val hostValue = e.currentTarget.value

      t.modState { s =>
        val newState = s.copy(clusterForm = ClusterForm(s.clusterForm.name, hostValue, s.clusterForm.seeds))
        updateClusterForm(newState.clusterForm)
        newState.copy(submitEnabled = getSubmitEnabled(newState))
      }
    }

    def updateClusterSeedHost(index: Int)(e: ReactEventI): Callback = {
      val hostValue = e.currentTarget.value

      t.modState { s =>
        val newState =
          s.copy(clusterForm = ClusterForm(s.clusterForm.name, s.clusterForm.selfHost, seeds = {
            s.clusterForm.seeds.zipWithIndex.map {
              case (seed, i) =>
                if (index == i) {
                  (seed.copy(host = hostValue), i)
                } else {
                  (seed, i)
                }
            }.map(_._1)
          }))

        updateClusterForm(newState.clusterForm)
        newState.copy(submitEnabled = getSubmitEnabled(newState))
      }
    }

    def setPortValue(form: ClusterForm, v: String, index: Int): ClusterForm =
      ClusterForm(form.name, form.selfHost, seeds = {
        form.seeds.zipWithIndex.map {
          case (seed, i) =>
            if (index == i) {
              (seed.copy(port = v), i)
            } else {
              (seed, i)
            }
        }.map(_._1)
      })

    def updateClusterSeedPort(index: Int)(e: ReactEventI): Callback = {
      val portValue = e.currentTarget.value

      if (e.currentTarget.value.length > 0) {
        try {
          val portIntValue = portValue.toInt
          t.modState { s =>
            val newState = s.copy(clusterForm = setPortValue(s.clusterForm, portValue, index))
            updateClusterForm(newState.clusterForm)
            newState.copy(portValid = portIntValue <= 65535, submitEnabled = getSubmitEnabled(newState))
          }

        } catch {
          case ex: Throwable =>
            t.modState(s =>
              s.copy(portValid = false, clusterForm = setPortValue(s.clusterForm, portValue, index),
                submitEnabled = getSubmitEnabled(s)))
        }
      } else {
        t.modState { s =>
          val newState = s.copy(portValid = true, clusterForm = setPortValue(s.clusterForm, portValue, index))
          updateClusterForm(newState.clusterForm)
          newState.copy(submitEnabled = getSubmitEnabled(newState))
        }

      }
    }

    def addSeedNodeToForm(): Callback = {
      t.modState(s => s.copy(seeds = s.seeds + 1))
    }

    def getSubmitEnabled(s: State): Boolean = {
      s.clusterForm.name.length > 0 && s.clusterForm.seeds.forall(hp =>
        hp.host.length > 0 && hp.port != "0" && hp.port.toString.length > 0)
    }

    def hide(): Callback =
      t.props.flatMap { props ⇒
        t.modState(_.copy(clusterForm = ClusterForm.initial))
        props.closeForm
      }
        .void

  }

  def component = ReactComponentB[EditClusterProps]("ClusterForm")
    .initialState_P(props ⇒ State(props.clusterForm, 0, portValid = true, submitEnabled = false)) // initial state
    .backend(Backend)
    .render(scope =>
      Modal(Modal.Props(
        be =>
          span(button(tpe := "button", cls := "pull-right", onClick --> be.hide, Icon.close),
            h4(color := "black")("Discover Cluster")),
        be =>
          span(Button(Button.Props(
            scope.props.editHandler(scope.state.clusterForm)
              .flatMap(_ ⇒ be.hide())
          ), "OK")),
        scope.backend.hide()
      ),
        form(
          div(cls := "form-group col-md-8")(
            label()("Cluster Name"),
            input(tpe := "text", cls := "form-control",
              value := scope.state.clusterForm.name,
              onChange ==> scope.backend.updateClusterName)
          ),
          div(cls := "col-md-12 form-group") {

            scope.props.clusterForm.seeds.zipWithIndex.map {
              case (eachSeed, index) =>
                div(cls := "row", key := s"$index")(
                  div(cls := "form-group col-md-4")(
                    label()("App host"),
                    input(tpe := "text", cls := "form-control", value := scope.state.clusterForm.selfHost,
                      onChange ==> scope.backend.updateClusterSelfHost)
                  ),
                  div(cls := "form-group col-md-4")(
                    label()("Seed Host"),
                    input(tpe := "text", cls := "form-control",
                      value := scope.state.clusterForm.seeds.zipWithIndex.find {
                        case (x, i) =>
                          i == index
                      }.map(_._1.host).getOrElse(""),
                      onChange ==> scope.backend.updateClusterSeedHost(index))
                  ),

                  div(cls := s"form-group col-md-2 ${if (!scope.state.portValid) "has-error" else ""}")(
                    label()("Port"),
                    input(tpe := "text", cls := "form-control",
                      value := scope.state.clusterForm.seeds.zipWithIndex.find { case (x, i) => i == index }.map(_._1.port.toString).getOrElse(""),
                      onChange ==> scope.backend.updateClusterSeedPort(index))
                  )
                )
            }
          }
        )
      )
    )
    .componentDidMount(x =>
      x.modState(s => s.copy(clusterForm = x.props.clusterForm))
    )
    .build

  def apply(store: ClusterService,
    editHandler: ClusterForm => Callback,
    closeForm: Callback): ReactComponentU[EditClusterProps, State, Backend, TopNode] =
    component(EditClusterProps(store.getClusterForm(), editHandler, closeForm))

}
