package com.boldradius.astrolabe.clustertracking

import akka.actor._
import akka.cluster.ClusterEvent._
import akka.cluster.{ Cluster, Member }
import akka.cluster.metrics.StandardMetrics.{ Cpu, HeapMemory }
import akka.cluster.metrics.{ ClusterMetricsExtension, NodeMetrics }
import com.boldradius.astrolabe.core.LogF
import com.boldradius.astrolabe.http._
import com.typesafe.config.ConfigFactory

import scala.collection.immutable

case class IsDiscovered(system: DiscoveredCluster)

object ClusterAware {

  def props(systemName: String, selfHost: String, seedNodes: List[HostPort], parent: ActorRef): Props =
    Props(new ClusterAware(systemName, selfHost, seedNodes, parent))

  def toClusterMember(m: Member, nodeState: NodeState): ClusterMember =
    ClusterMember(
      HostPort(m.uniqueAddress.address.host.getOrElse("Unknown"), m.uniqueAddress.address.port.getOrElse(0)),
      m.roles,
      nodeState
    )

}

class ClusterAware(systemName: String,
    selfHost: String,
    seedNodes: List[HostPort],
    parent: ActorRef) extends Actor with ActorLogging {

  val selfPort = 0

  val akkaConf =
    s"""akka.remote.netty.tcp.hostname="$selfHost"
        |akka.remote.netty.tcp.port=$selfPort
        |auto-down-unreachable-after = 5s
        |akka.cluster.roles = [clusterconsole]
        |""".stripMargin

  val config = ConfigFactory.parseString(akkaConf).withFallback(ConfigFactory.load())

  lazy val newSystem = ActorSystem(systemName, config)

  lazy val cluster = Cluster(newSystem)

  /** subscribe to cluster events in order to track workers */
  override def preStart() = {
    val addresses: immutable.Seq[Address] =
      seedNodes.map(e => Address("akka.tcp", systemName, e.host, e.port))

    // todo - track cluster metrics
    cluster.subscribe(self,
      initialStateMode = InitialStateAsEvents,
      classOf[MemberUp],
      classOf[UnreachableMember],
      classOf[MemberRemoved],
      classOf[MemberExited],
      classOf[LeaderChanged]
    )

    ClusterMetricsExtension(newSystem).subscribe(self)

    cluster.joinSeedNodes(addresses)

  }

  override def postStop() = {
    cluster.unsubscribe(self)
    cluster.leave(
      Address("akka.tcp", systemName, selfHost, selfPort)
    )
  }

  def receive: Receive = trackingMembers(Set.empty[Member])

  def trackingMembers(members: Set[Member]): Receive = {

    case m @ CurrentClusterState(clusterMembers, _, _, _, _) =>
      context.become(
        trackingMembers(clusterMembers)
      )

    case MemberUp(m) =>
      // ignore ourself for console view
      if (m.roles != Set("clusterconsole")) {
        context.become(
          trackingMembers(members + m)
        )
        def clusterMember(m: Member) =
          ClusterAware.toClusterMember(m, Up)

        parent ! ClusterMemberUp(systemName, clusterMember(m))

        parent ! IsDiscovered(
          DiscoveredCluster(systemName, seedNodes, "", (members + m).map(clusterMember), Seq.empty[RoleDependency])
        )
      }

    case UnreachableMember(m) =>
      if (m.roles != Set("clusterconsole"))
        parent ! ClusterMemberUnreachable(systemName, ClusterAware.toClusterMember(m, Unreachable))

    case MemberRemoved(m, previousStatus) =>
      if (m.roles != Set("clusterconsole")) {
        parent ! ClusterMemberRemoved(systemName, ClusterAware.toClusterMember(m, Removed))
      }

    case MemberExited(m) =>
      if (m.roles != Set("clusterconsole")) {
        parent ! ClusterMemberExited(systemName, ClusterAware.toClusterMember(m, Exited))
      }

    case akka.cluster.metrics.ClusterMetricsChanged(clusterMetrics) =>
      // ignore my own metrics
      clusterMetrics.filter(_.address != Cluster(context.system).selfAddress)
        .foreach { nodeMetrics =>
          logHeap(nodeMetrics)
          logCpu(nodeMetrics)
        }
  }

  def logHeap(nodeMetrics: NodeMetrics): Unit = nodeMetrics match {
    case HeapMemory(address, timestamp, used, committed, max) =>
      log.debug("Address: {} Used heap: {} MB", address, used.doubleValue / 1024 / 1024)
      val date = new java.util.Date(timestamp)
      val usedMB = used.doubleValue / 1024 / 1024
      val committedMB = committed.doubleValue / 1024 / 1024
      val maxHeapMB = max.map { n =>
        n.doubleValue / 1024 / 1024
      }
      parent ! ClusterMetricMemory(systemName,
        HostPort(address.host.getOrElse("0.0.0.0"), address.port.getOrElse(0)),
        date.toString,
        usedMB,
        committedMB,
        maxHeapMB)
    case _ => // No heap info.
  }

  def logCpu(nodeMetrics: NodeMetrics): Unit = nodeMetrics match {
    case Cpu(address, timestamp, Some(systemLoadAverage), cpuCombined, cpuStolen, processors) =>
      println(s"Address: $address Load: $systemLoadAverage ($processors processors)")
      val date = new java.util.Date(timestamp)
      parent ! ClusterMetricCPU(systemName,
        HostPort(address.host.getOrElse("0.0.0.0"), address.port.getOrElse(0)),
        date.toString,
        systemLoadAverage,
        cpuCombined,
        cpuStolen,
        processors)

    case _ => // No cpu info.
  }

}
