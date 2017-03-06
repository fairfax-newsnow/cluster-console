package com.boldradius.astrolabe.client.services

import autowire._
import com.boldradius.astrolabe.client.domain.{ NodeLike, ClusterGraphNode }
import com.boldradius.astrolabe.client.modules.Mode
import com.boldradius.astrolabe.client.services.Logger._
import com.boldradius.astrolabe.client.ukko.Actor
import com.boldradius.astrolabe.http._
import rx._

import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

case object RefreshClusterMembers

case class UpdateClusterForm(clusterForm: ClusterForm)

case class SelectRoleDependency(system: String, rd: RoleDependency, selected: Boolean)

case class SelectedCluster(c: DiscoveredCluster)

case class RefreshCluster(c: DiscoveredCluster)

case class UpdatedCluster(c: DiscoveredCluster)

case class UpdateNodePosition(system: String, mode: Mode, node: ClusterGraphNode)

trait ClusterService extends Actor {

  import com.boldradius.astrolabe.client.domain.NodeLike._

  private val discoveredClusters = Var(Map.empty[String, DiscoveredCluster])

  private val fixedNodePositions = Var(Map.empty[String, Map[Mode, List[ClusterGraphNode]]])

  private val selectedDeps = Var(Map.empty[String, List[RoleDependency]])

  private val selectedCluster = Var(Option.empty[DiscoveredCluster])

  private val discoveringClusters = Var(Map.empty[String, DiscoveryBegun])

  private val events = Var(Seq.empty[ClusterEvent])

  private val clusterForm = Var(ClusterForm.initial)

  def getDiscoveredClusters: Rx[Map[String, DiscoveredCluster]] = discoveredClusters

  def getFixedNodePositions: Rx[Map[String, Map[Mode, List[ClusterGraphNode]]]] = fixedNodePositions

  def getDiscoveringClusters: Rx[Map[String, DiscoveryBegun]] = discoveringClusters

  def getSelectedCluster: Rx[Option[DiscoveredCluster]] = selectedCluster

  def getSelectedDeps: Rx[Map[String, List[RoleDependency]]] = selectedDeps

  def getClusterForm: Rx[ClusterForm] = clusterForm

  def name: String = "ClusterService"

  def receive: ClusterService.Receive = {
    case m @ ClusterMemberUp(system, clusterMember) =>
      ClusterService.refreshCluster(system)

    case m @ ClusterMemberUnreachable(system, clusterMember) =>
      ClusterService.refreshCluster(system)

    case m @ ClusterMemberRemoved(system, clusterMember) =>
      ClusterService.refreshCluster(system)

    case m: ClusterMetricCPU =>
      ClusterService.refreshCluster(m.system)

    case m: ClusterMetricMemory =>
      ClusterService.refreshCluster(m.system)

    case m @ DiscoveryBegun(system, seedNodes) =>
      clusterForm() = ClusterForm.initial
      discoveringClusters() = discoveringClusters() + (system -> m)

    case m @ DiscoveredCluster(system, seeds, status, members, _) =>
      discoveringClusters() = discoveringClusters() - system
      discoveredClusters() = discoveredClusters() + (system -> m)

    case m @ SelectedCluster(DiscoveredCluster(system, seeds, status, members, _)) =>
      selectedCluster() = Some(m.c)

    case m @ RefreshCluster(DiscoveredCluster(system, seeds, status, members, deps)) =>
      discoveringClusters() = discoveringClusters() - system
      discoveredClusters() = discoveredClusters() + (system -> m.c)
      if (selectedCluster().isEmpty)
        selectedCluster() = Some(m.c)
      else {
        selectedCluster().foreach(c =>
          if (c.system == system)
            selectedCluster() = Some(m.c)
        )
      }

    case SelectRoleDependency(system, roleDependency, selected) =>
      if (selected) {
        selectedDeps() =
          selectedDeps() + (system -> selectedDeps().get(system).fold(List(roleDependency))(deps =>
            (deps.toSet + roleDependency).toList))
      } else {
        selectedDeps() =
          selectedDeps() + (system -> selectedDeps().get(system).fold(List.empty[RoleDependency])(deps =>
            (deps.toSet - roleDependency).toList))
      }

    case m @ UpdatedCluster(DiscoveredCluster(system, seeds, status, members, _)) =>
      selectedCluster().foreach(c =>
        if (c.system == system)
          selectedCluster() = Some(m.c)
      )

    case UpdateNodePosition(system, mode, node) =>
      fixedNodePositions() = updateFixedNodePositions(fixedNodePositions(), system, mode, node)
    //      ClusterService.refreshCluster(system)

    //    case clusterUnjoin: ClusterUnjoin =>
    //      events() = events() :+ clusterUnjoin

    case x: UpdateClusterForm =>
      clusterForm() = x.clusterForm

    case other => log.debug("other " + other)

  }

  def updateFixedNodePositions(map: Map[String, Map[Mode, List[ClusterGraphNode]]],
    system: String,
    mode: Mode,
    node: ClusterGraphNode)(implicit ev: NodeLike[ClusterGraphNode]): Map[String, Map[Mode, List[ClusterGraphNode]]] =
    {
      log.debug("updateFixedNodePositions" + node.host + ":" + node.port)
      map + (system ->
        map.get(system).fold(Map(mode -> List(node)))(modeMap => modeMap + (mode -> {
          modeMap.get(mode).fold(List(node))(nodes =>
            nodes.find(e => ev.nodeEq(e, node)).fold({
              log.debug("updateFixedNodePositions adding " + node.host + ":" + node.port)
              node :: nodes
            })(found => {
              // this node is already fixed
              nodes.map(n =>
                if (ev.nodeEq(n, node)) {
                  log.debug("updateFixedNodePositions found " + node.host + ":" + node.port)
                  node
                } else n)

            }
            ))
        })))

    }

}

object ClusterService extends ClusterService {
  MainDispatcher.register(this)

  import Json._

  def subscribeToCluster(name: String, selfHost: String, seedNodes: List[HostPort]) =
    AjaxClient[Api].discover(name, selfHost, seedNodes).call()
      .foreach(_.foreach(MainDispatcher.dispatch)) // DiscoveryBegun

  def findDiscoveringClusters() =
    AjaxClient[Api].getDiscovering().call().foreach { discoveringSeq =>
      discoveringSeq.foreach(MainDispatcher.dispatch)
    }

  def findDiscoveredClusters() =
    AjaxClient[Api].getDiscovered().call().foreach { discoveredSet =>
      discoveredSet.foreach(MainDispatcher.dispatch)
    }

  def selectCluster(system: String) =
    AjaxClient[Api].getCluster(system).call().foreach { discovered =>
      discovered.foreach(c => MainDispatcher.dispatch(SelectedCluster(c)))
    }

  def refreshCluster(system: String) =
    AjaxClient[Api].getCluster(system).call().foreach { discovered =>
      discovered.foreach(c => MainDispatcher.dispatch(RefreshCluster(c)))
    }

  def updateClusterDependencies(cluster: DiscoveredCluster) =
    AjaxClient[Api].updateClusterDependencies(cluster).call().foreach { updated =>
      MainDispatcher.dispatch(UpdatedCluster(updated))
    }

  def selectRoleDependency(system: String, rd: RoleDependency, selected: Boolean) =
    MainDispatcher.dispatch(SelectRoleDependency(system, rd, selected))

  def updateClusterForm(clusterForm: ClusterForm) =
    MainDispatcher.dispatch(UpdateClusterForm(clusterForm))

  def updateNodePosition(system: String, mode: Mode, node: ClusterGraphNode) =
    MainDispatcher.dispatch(UpdateNodePosition(system, mode, node))

}

