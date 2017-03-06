package com.boldradius.astrolabe.client.components.graph

import com.boldradius.astrolabe.client.d3.Layout._
import com.boldradius.astrolabe.client.d3._
import com.boldradius.astrolabe.client.domain._
import com.boldradius.astrolabe.client.modules._
import com.boldradius.astrolabe.client.services.ClusterService
import com.boldradius.astrolabe.client.services.Logger._
import com.boldradius.astrolabe.http.{ ClusterMember, DiscoveredCluster, RoleDependency }
import japgolly.scalajs.react.extra.OnUnmount
import japgolly.scalajs.react.vdom.SvgAttrs
import japgolly.scalajs.react.vdom.all.svg._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{ ReactComponentB, ReactNode, _ }

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

object Graph {

  import com.boldradius.astrolabe.client.style.CustomTags._

  case class Props(system: String, mode: Mode, width: Double, height: Double,
    store: ClusterService, fixedMap: Boolean)

  case class State(nodes: Seq[ClusterGraphNode],
    links: Seq[ClusterGraphLink],
    force: ForceLayout)

  def drawLinks(links: Seq[ClusterGraphLink], mode: Mode): ReactNode =
    g(links.zipWithIndex.map { case (eachLink, i) => GraphLink(eachLink, i, mode) })

  def drawNodes(nodes: Seq[ClusterGraphNode], force: ForceLayout, mode: Mode): Seq[ReactNode] =
    nodes.zipWithIndex.map {
      case (node, i) =>
        GraphNode(node, force, mode)
    }

  def drawDeps(roles: Seq[(RoleDependency, Boolean)], select: (RoleDependency, Boolean) => Callback): Seq[ReactNode] =
    roles.zipWithIndex.map {
      case ((dep, selected), i) => ClusterDependencyLegend(dep, i, selected, select)
    }

  case class Backend(t: BackendScope[Props, State]) extends RxObserver(t) {

    def mounted(): Callback = {
      t.props.map { props ⇒
        react(props.store.getSelectedCluster, (v: Option[DiscoveredCluster]) ⇒ updateGraph(v).runNow())
        react(props.store.getSelectedDeps, (v: Map[String, List[RoleDependency]]) ⇒ updateLinkDeps(v).runNow())
      }
        .void
    }

    def selectDep(rd: RoleDependency, selected: Boolean): CallbackTo[CallbackTo.MapGuard[Unit]#Out] = {
      t.props.map { props ⇒
        log.debug("selectDep " + rd.tpe.name + " " + selected)

        props.store.getSelectedCluster().foreach(cluster =>
          ClusterService.selectRoleDependency(cluster.system, rd, selected)
        )
      }
    }

    def updateLinkDeps(c: Map[String, List[RoleDependency]]): CallbackTo[CallbackTo.MapGuard[Unit]#Out] = {
      t.props.flatMap(props ⇒
        t.state.map(state ⇒
          props.store.getSelectedCluster().foreach(cluster =>
            c.get(cluster.system).foreach(deps =>
              t.modState { s =>
                val links: Seq[ClusterGraphLink] = getLinks(state.nodes, props.mode, cluster, deps)
                val nodeUpdateState = s.copy(nodes = state.nodes, force = s.force.nodes(state.nodes.toJsArray).start())
                nodeUpdateState.copy(links = links)
                s.copy(links = links)
              }
            )
          )
        )
      )
    }

    def getFixedList(system: String): CallbackTo[List[ClusterGraphNode]] =
      t.props.map(props ⇒
        props.store.getFixedNodePositions()
          .getOrElse(system, Map.empty[Mode, List[ClusterGraphNode]])
          .getOrElse(props.mode, Nil)
      )

    def fixedHostPosition(system: String, host: String, cn: ClusterGraphNode): CallbackTo[CallbackTo.MapGuard[ClusterGraphNode]#Out] =
      getFixedList(system).map(list ⇒
        list.find(e => e.host == host && e.port == 0).fold(
          ClusterGraphNode.host(host, cn.index, cn.x, cn.y, cn.px, cn.py, cn.fixed, cn.weight)
        )(fixedNode => {
            js.Dynamic.literal(
              "host" -> cn.host,
              "port" -> cn.port,
              "roles" -> cn.roles,
              "status" -> "Up",
              "name" -> cn.name,
              "index" -> cn.index,
              "x" -> cn.x,
              "y" -> cn.y,
              "px" -> cn.px,
              "py" -> cn.py,
              "fixed" -> true,
              "weight" -> cn.weight
            ).asInstanceOf[ClusterGraphNode]
          })
      )

    /**
     * ClusterMember => ClusterGraphNode, checking if this has a fixed position from dragging
     */
    def fixNodePosition(system: String, member: ClusterMember, cn: ClusterGraphNode)(implicit ev: MemberLike[ClusterGraphNode, ClusterMember]) =
      getFixedList(system).map(list ⇒
        list.find(e => ev.nodeEq(e, member)).fold(
          ClusterGraphNode(member, cn.index, cn.x, cn.y, cn.px, cn.py, cn.fixed, cn.weight)
        )(fixedNode => {
            js.Dynamic.literal(
              "host" -> cn.host,
              "port" -> cn.port,
              "roles" -> cn.roles,
              "status" -> member.state.toString,
              "name" -> cn.name,
              "index" -> cn.index,
              "x" -> cn.x,
              "y" -> cn.y,
              "px" -> cn.px,
              "py" -> cn.py,
              "fixed" -> true,
              "weight" -> cn.weight
            ).asInstanceOf[ClusterGraphNode]
          })
      )

    def updateGraph(c: Option[DiscoveredCluster]) =
      t.state.flatMap { state ⇒
        t.props.map { props ⇒
          c.fold[Unit](Callback.log("Empty cluster data"))(cluster =>
            Callback.log("updateGraph")
              .flatMap { _ ⇒
                val existingIndexes = state.nodes.map(_.index).toSet

                val incomingNodes: Seq[ClusterGraphNode] =
                  props.mode match {
                    case Nodes =>

                      // get a node map of what is currently on screen
                      val currentNodesMap = state.nodes.map(e => (ClusterGraphNode.label(e), e)).toMap

                      log.debug("currentNodesMap " + currentNodesMap.toList.map(e => (e._1, e._2.host + ":" + e._2.port)))

                      // add host nodes in
                      val hostMap = cluster.members.toSeq.groupBy(m => m.address.host)

                      // this is actual cluster state from server, nodes could have been added there,
                      // must be added here. check existence in current map, if not there, add one, else
                      // check fixed position
                      val ports = cluster.members.toSeq.map { node =>
                        currentNodesMap.get(node.address.label).fold(
                          ClusterGraphNode(node, getNewIndex(existingIndexes, 1), 450, 450, 450, 450, false, 0)
                        )(cn => fixNodePosition(props.system, node, cn).runNow())
                      }

                      val hosts = hostMap.keys.toSeq.map(hostName =>
                        currentNodesMap.get(hostName + ":0").fold(
                          ClusterGraphNode.host(hostName, getNewIndex(existingIndexes, 1), 450, 450, 450, 450, false, 0)
                        )(cn => fixedHostPosition(props.system, hostName, cn).runNow())

                      )

                      hosts ++: ports

                    case _ =>

                      // get a node map of what is currently on screen
                      val currentNodesMap = state.nodes.map(e => (ClusterGraphNode.label(e), e)).toMap

                      // this is actual cluster state from server, nodes could have been added there,
                      // must be added here. check existence in current map, if not there, add one, else
                      // check fixed position
                      val res = cluster.members.toSeq.map { node =>
                        currentNodesMap.get(node.address.label).fold(
                          ClusterGraphNode(node, getNewIndex(existingIndexes, 1), 450, 450, 450, 450, false, 0)
                        )(cn => fixNodePosition(props.system, node, cn).runNow())
                      }
                      res

                  }

                log.debug("********** incomingNodes = " + incomingNodes.map(e => e.port + " " + e.index))

                log.debug("********** cluster deps = " + cluster.dependencies)

                t.modState { s =>
                  val links: Seq[ClusterGraphLink] = getLinks(incomingNodes,
                    props.mode, cluster, props.store.getSelectedDeps().getOrElse(cluster.system, Nil))
                  s.copy(nodes = incomingNodes, links = links, force = s.force.nodes(incomingNodes.toJsArray).start())
                }

                initDrag()
              }
          )
        }
      }

    def renderTick(): Callback =
      t.state.flatMap { state ⇒
        val newNodes: List[ClusterGraphNode] = state.force.nodes().toList
        //      val notFixed = newNodes.filter(_.fixed == false)
        //      val fixed = t.state.nodes.filter(_.fixed == true)
        t.modState(s => s.copy(nodes = newNodes))
      }
        .void

    def startfixed() =
      t.state.flatMap(state ⇒
        t.modState { s =>
          val firstState = s.copy(force = s.force.nodes(state.nodes.toJsArray).start())
          (1 until 150).foreach(i => state.force.tick())
          firstState.copy(force = s.force.on("tick", renderTick().toScalaFn))
        }
      )
        .void

    def initDrag(): Callback =
      t.state.map { state ⇒
        val drag = state.force.drag().on("dragend", (a: js.Any, b: Double) => dragEnd[ClusterGraphNode](a, b))
        d3.select("svg").
          selectAll(".node").
          data(state.nodes.toJSArray).
          call(drag)
      }
        .void

    def dragEnd[T: NodeLike](d: js.Any, x: Double): Callback =
      t.state.flatMap { state ⇒
        t.props.flatMap { props ⇒
          val node = d.asInstanceOf[ClusterGraphNode]

          t.modState { s =>
            val newNodes =
              s.nodes.map { e =>
                if (implicitly[NodeLike[ClusterGraphNode]].nodeEq(e, node)) {
                  js.Dynamic.literal(
                    "virtualHost" -> e.name,
                    "host" -> e.host,
                    "port" -> e.port,
                    "roles" -> e.roles,
                    "status" -> e.status,
                    "name" -> e.name,
                    "index" -> e.index,
                    "x" -> e.x,
                    "y" -> e.y,
                    "px" -> e.px,
                    "py" -> e.py,
                    "fixed" -> true,
                    "weight" -> e.weight
                  ).asInstanceOf[ClusterGraphNode]
                } else {
                  e
                }
              }
            s.copy(nodes = newNodes, force = s.force.nodes(newNodes.toJSArray).start())
          }

          state.nodes.find(e => implicitly[NodeLike[ClusterGraphNode]].nodeEq(e, node)).foreach { node =>
            log.debug("ClusterService.updateNodePosition node: " + node.host + ":" + node.port)

            ClusterService.updateNodePosition(props.system, props.mode, node)
          }

          updateGraph(props.store.getSelectedCluster())
        }
      }
        .void
  }

  val component: ReactComponentC.ReqProps[Props, State, Backend, TopNode] = ReactComponentB[Props]("Graph")
    .initialState_P { props ⇒

      val force = d3.layout.force()
        .size(List[Double](props.width, props.height).toJsArray)
        .charge(-1500)
        .linkDistance(1000)
        .friction(0.9)

      val (nodes, links) = props.store.getSelectedCluster().map(cluster => {
        getNodesAndLink(cluster,
          props.mode,
          props.store.getFixedNodePositions().getOrElse(cluster.system, Map.empty[Mode, List[ClusterGraphNode]])
            .getOrElse(props.mode, Nil),
          props.store.getSelectedDeps().getOrElse(cluster.system, Nil))
      }).getOrElse((Nil, Nil))

      State(nodes, links, force)

    }
    .backend(Backend)
    .render(scope => {

      val selectedDeps = scope.props.store.getSelectedDeps().getOrElse(scope.props.system, Nil)

      val roles: Seq[(RoleDependency, Boolean)] =
        if (scope.props.mode == Roles) {
          scope.props.store.getSelectedCluster().map(_.dependencies).getOrElse(Nil)
            .map(eachDep => (eachDep, selectedDeps.exists(_.tpe.name == eachDep.tpe.name)))
        } else {
          Nil
        }

      svgtag(SvgAttrs.width := scope.props.width, SvgAttrs.height := scope.props.height)(
        drawDeps(roles, (rd, selected) ⇒ scope.backend.selectDep(rd, selected)),
        drawLinks(scope.state.links, scope.props.mode),
        drawNodes(scope.state.nodes, scope.state.force, scope.props.mode)
      )
    })
    .componentWillReceiveProps { scope =>
      val (nodes, links) = scope.nextProps.store.getSelectedCluster().map(cluster => {
        getNodesAndLink(cluster, scope.nextProps.mode,
          scope.nextProps.store.getFixedNodePositions().getOrElse(cluster.system, Map.empty[Mode, List[ClusterGraphNode]])
            .getOrElse(scope.nextProps.mode, Nil),
          scope.nextProps.store.getSelectedDeps().getOrElse(cluster.system, Nil))
      }).getOrElse((Nil, Nil))

      val newState = State(nodes, links, scope.currentState.force)

      scope.$.backend.t.modState { s =>
        val firstState = s.copy(nodes = nodes, links = links, force = s.force.nodes(nodes.toJsArray).start())
        (1 until 150).foreach(i => scope.currentState.force.tick())
        firstState.copy(force = s.force.on("tick", () ⇒ scope.$.backend.renderTick.runNow()))
      }
    }
    .componentWillMount { scope =>
      Callback.log("componentWillMount").flatMap(_ ⇒ scope.backend.startfixed())
    }
    .componentDidMount { scope =>
      Callback.log("componentDidMount")
        .flatMap(_ ⇒ scope.backend.mounted())
        .flatMap(_ ⇒ scope.backend.initDrag())
        .void
    }
    .componentWillUnmount { scope =>
      Callback {
        scope.state.force.stop()
      }
    }
    .configure(OnUnmount.install).build

  def apply(system: String, mode: Mode, width: Double, height: Double, store: ClusterService, fixedMap: Boolean) = {

    component(Props(system, mode, width, height, store, fixedMap))
  }

  def getNodesAndLink(cluster: DiscoveredCluster,
    mode: Mode,
    fixedList: List[ClusterGraphNode],
    selectedDeps: List[RoleDependency])(implicit ev: MemberLike[ClusterGraphNode, ClusterMember]): (Seq[ClusterGraphNode], Seq[ClusterGraphLink]) = {
    val nodes: Seq[ClusterGraphNode] =

      mode match {
        case Nodes =>

          // group by host
          val map = cluster.members.toSeq.groupBy(m => m.address.host)

          var newKeyIndex = 1000

          map.keys.toSeq.zipWithIndex.flatMap {
            case (key, keyIndex) =>

              val ports: Seq[ClusterMember] = map.getOrElse[Seq[ClusterMember]](key, Seq.empty[ClusterMember])

              val portNodes: Seq[ClusterGraphNode] = ports.zipWithIndex.map {
                case (pNode, pIndex) =>
                  fixedList.find(e => ev.nodeEq(e, pNode)).fold(
                    ClusterGraphNode.port(pNode, newKeyIndex + keyIndex + 1 + pIndex, 450, 450, 450, 450, false, 1)
                  )(found =>
                      ClusterGraphNode.port(pNode, newKeyIndex + keyIndex + 1 + pIndex, found.x, found.y, found.px, found.py, true, 1)
                    )

              }

              val hostNode: Option[ClusterGraphNode] =
                ports.headOption.map(firstPort =>
                  fixedList.find(e => e.host == firstPort.address.host && e.port == 0).fold(
                    ClusterGraphNode.host(firstPort.address.host, newKeyIndex + keyIndex, 450, 450, 450, 450, false, ports.length)
                  )(found =>
                      ClusterGraphNode.host(firstPort.address.host, newKeyIndex + keyIndex, found.x, found.y, found.px, found.py, true, ports.length)
                    ))

              val res = hostNode.fold(Seq.empty[ClusterGraphNode])(hn => hn +: portNodes)

              newKeyIndex = newKeyIndex + ports.length + 1

              res
          }

        case _ =>
          cluster.members.toSeq.zipWithIndex.map {
            case (node, i) =>
              fixedList.find(e => ev.nodeEq(e, node)).fold(
                ClusterGraphNode(node, i, 450, 450, 450, 450, false, 0)
              )(fixedNode => {
                  ClusterGraphNode(node,
                    fixedNode.index, fixedNode.x, fixedNode.y, fixedNode.px, fixedNode.py, fixedNode.fixed, 0)
                })
          }
      }
    (nodes.toSeq, getLinks(nodes.toSeq, mode, cluster, selectedDeps))
  }

  def getNewIndex(set: Set[Double], v: Double): Double =
    if (set.contains(v)) {
      getNewIndex(set, v + 1)
    } else {
      v
    }

  def getLinks(nodes: Seq[ClusterGraphNode],
    mode: Mode,
    cluster: DiscoveredCluster,
    roleDependencies: List[RoleDependency] = Nil)(implicit ev: MemberLike[ClusterGraphNode, ClusterMember]) = {

    def makeLinks(conns: Seq[(Double, Double)]) =
      conns.flatMap {
        case (a, b) =>
          for {
            source <- nodes.find(_.index == a)
            target <- nodes.find(_.index == b)
          } yield {
            js.Dynamic.literal(
              "source" -> source,
              "target" -> target,
              "sourceHost" -> source.host,
              "targetHost" -> target.host).asInstanceOf[ClusterGraphLink]
          }
      }

    mode match {
      case Members =>
        val indexes = nodes.filter(_.status == "Up").map(_.index)
        val res: Seq[(Double, Double)] =
          indexes.flatMap(index => indexes.filter(_ > index).map((index, _)))
        makeLinks(res)

      case Roles =>
        val allDeps = cluster.dependencies
        roleDependencies.zipWithIndex.flatMap {
          case (rd, index) =>
            val sourcesIndexes = rd.roles.flatMap { eachRole =>
              cluster.getNodesByRole(eachRole).toSeq.flatMap(e =>
                nodes.filter(n => ev.nodeEq(n, e)).map(_.index))
            }

            val targetsIndexes = rd.dependsOn.flatMap { eachRole =>
              cluster.getNodesByRole(eachRole).toSeq.flatMap(e =>
                nodes.filter(n => ev.nodeEq(n, e)).map(_.index))
            }

            val indexes = sourcesIndexes.flatMap(eachSource =>
              targetsIndexes.map(eachTarget =>
                (eachSource, eachTarget)))

            // get index of RoleDep

            indexes.flatMap {
              case (a, b) =>
                for {
                  source <- nodes.find(_.index == a)
                  target <- nodes.find(_.index == b)
                } yield {
                  js.Dynamic.literal(
                    "index" -> allDeps.indexOf(rd),
                    "source" -> source,
                    "target" -> target,
                    "sourceHost" -> source.host,
                    "targetHost" -> target.host).asInstanceOf[ClusterGraphRoleLink]
                }
            }

        }

      case Nodes =>

        //join ports to hosts
        val hostPortMap: Map[String, Seq[ClusterGraphNode]] = nodes.groupBy(n => n.host)

        val hostToPortIndexes = hostPortMap.foldLeft[Seq[(Double, Double)]](Seq.empty[(Double, Double)])((a, b) => a ++ {
          nodes.find(e => e.host == b._1 && e.port == 0).map(host =>
            b._2.flatMap(e => if (e.port != 0) {
              Some((host.index, e.index))
            } else None)).getOrElse(Nil)
        }
        )
        makeLinks(hostToPortIndexes)
    }

  }

}