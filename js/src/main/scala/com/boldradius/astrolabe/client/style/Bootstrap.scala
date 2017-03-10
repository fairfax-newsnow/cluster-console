package com.boldradius.astrolabe.client.style

import japgolly.scalajs.react.vdom.all._
import japgolly.scalajs.react._
import scala.language.implicitConversions
import scala.scalajs.js
import scalacss.Defaults._
import scalacss.ScalaCssReact._

/**
 * Common Bootstrap components for scalajs-react
 */
object Bootstrap {

  // shorthand for styles
  @inline private def bss = GlobalStyles.bootstrapStyles

  @js.native
  trait BootstrapJQuery extends JQuery {
    def modal(action: String): BootstrapJQuery = js.native
    def modal(options: js.Any): BootstrapJQuery = js.native
  }

  implicit def jq2bootstrap(jq: JQuery): BootstrapJQuery = jq.asInstanceOf[BootstrapJQuery]

  // Common Bootstrap contextual styles
  object CommonStyle extends Enumeration {
    val default, primary, success, info, warning, danger = Value
  }

  object Button {

    case class Props(onClick: Callback, style: CommonStyle.Value = CommonStyle.default, addStyles: Seq[StyleA] = Seq())

    val component = ReactComponentB[Props]("Button")
      .render { scope =>
        button(bss.buttonOpt(scope.props.style), scope.props.addStyles, tpe := "button", onClick --> scope.props.onClick)
      }.build

    def apply(props: Props, children: ReactNode*) = component(props, children)
    def apply() = component
  }

  object Panel {

    case class Props(heading: String, style: CommonStyle.Value = CommonStyle.default)

    val component = ReactComponentB[Props]("Panel")
      .renderC { (scope, children) =>
        div(bss.panelOpt(scope.props.style))(
          div(bss.panelHeading)(scope.props.heading),
          div(bss.panelBody)(children)
        )
      }.build

    def apply(props: Props, children: ReactNode*) = component(props, children)
    def apply() = component
  }

  object Modal {

    // header and footer are functions, so that they can get access to the the hide() function for their buttons
    case class Props(header: (Backend) => ReactNode, footer: (Backend) => ReactNode, closed: Callback, backdrop: Boolean = true,
      keyboard: Boolean = true)

    class Backend(t: BackendScope[Props, Unit]) {
      def hide(): Callback = Callback {
        // instruct Bootstrap to hide the modal
        jQuery(t.getDOMNode()).modal("hide")
      }

      // jQuery event handler to be fired when the modal has been hidden
      def hidden(e: JQueryEventObject): js.Any = {
        t.props.flatMap(props ⇒
          // inform the owner of the component that the modal was closed/hidden
          props.closed
        ).runNow()
      }
    }

    val component = ReactComponentB[Props]("Modal")
      .stateless
      .backend(new Backend(_))
      .renderC((scope, children) => {
        val modalStyle = bss.modal
        div(modalStyle.modal, modalStyle.fade, role := "dialog", aria.hidden := true,
          div(modalStyle.dialog,
            div(modalStyle.content, backgroundColor := GlobalStyles.modalBackground, color := "black",
              div(modalStyle.header, scope.props.header(scope.backend)),
              div(modalStyle.body)(children),
              div(modalStyle.footer, scope.props.footer(scope.backend))
            )
          )
        )
      })
      .componentDidMount(scope ⇒
        Callback {
          val P = scope.props
          // instruct Bootstrap to show the modal
          jQuery(scope.getDOMNode()).modal(js.Dynamic.literal("backdrop" -> P.backdrop, "keyboard" -> P.keyboard, "show" -> true))
          // register event listener to be notified when the modal is closed
          jQuery(scope.getDOMNode()).on("hidden.bs.modal", null, null, scope.backend.hidden _)
        }
      )
      .build

    def apply(props: Props, children: ReactNode*) = component(props, children)
    def apply() = component
  }
}
