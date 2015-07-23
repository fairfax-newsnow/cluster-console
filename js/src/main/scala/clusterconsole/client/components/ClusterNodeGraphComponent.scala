package clusterconsole.client.components

import rx._
import clusterconsole.client.d3.Layout.{ GraphLinkForce, GraphNodeForce }
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.all._
//import japgolly.scalajs.react.vdom.prefix_<^._
import scala.scalajs.js
import clusterconsole.client.d3._
import js.JSConverters._
import clusterconsole.client.services.Logger._

//object ActionAssignment {
//
//  import clusterconsole.client.style.CustomTags._
//
//
//  val component = ReactComponentB[Unit]("ActionAssignment")
//    .render(P => {
//    svgtag(
//      path(^.key := "acg", d := "M19 3h-4.18C14.4 1.84 13.3 1 12 1c-1.3 0-2.4.84-2.82 2H5c-1.1 0-2 .9-2 2v14c0 1.1.9 2 2 2h14c1.1 0 2-.9 2-2V5c0-1.1-.9-2-2-2zm-7 0c.55 0 1 .45 1 1s-.45 1-1 1-1-.45-1-1 .45-1 1-1zm2 14H7v-2h7v2zm3-4H7v-2h10v2zm0-4H7V7h10v2z")
//    )
//  }).buildU
//
//  def apply() = component()
//}

object ClusterNodeGraphComponent {

  case class Props()

  val test: Var[Double] = Var(1)

  def getTest: Rx[Double] = test

  case class State(test: Var[Double]) {
    def getTest: Rx[Double] = test
  }

  class Backend(t: BackendScope[Props, State]) {

    def tick(): Unit = {

      test() = test() + 1

      log.debug("^^^^ ticked")
    }

    def start(): Unit = {
      js.timers.setInterval(1000)(tick())
    }

  }

  def component = ReactComponentB[Props]("ClusterNodeGraph")
    .initialStateP(P => {
      State(Var(2))
    }).backend(new Backend(_))
    .render((P, S, B) => {

      val nodes: List[GraphNodeForce] =
        List(Node("1"), Node("2"), Node("3")).zipWithIndex.map {
          case (node, i) =>
            js.Dynamic.literal(
              "index" -> i,
              "x" -> 0,
              "y" -> 0,
              "px" -> 0,
              "py" -> 0,
              "fixed" -> false,
              "weight" -> 0
            ).asInstanceOf[GraphNodeForce]
        }

      val links: List[GraphLinkForce] =
        List(Link(1, 2), Link(2, 3), Link(1, 3)).zipWithIndex.map {
          case (link, i) =>
            js.Dynamic.literal("source" -> link.source, "target" -> link.target).asInstanceOf[GraphLinkForce]
        }
      div(
        button(cls := "btn btn-default", onClick --> B.tick)("tick"),
        Graph(600, 1000, nodes, links, getTest)
      )

    }).build

  def apply() = component(Props())

}