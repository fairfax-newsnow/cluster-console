package com.boldradius.astrolabe.http

sealed trait ClusterProtocol

trait ClusterEvent extends ClusterProtocol{
  val system:String
}

/*
** EVENTS
 */

case class CurrentClusterStateInitial(system: String, members: Set[ClusterMember]) extends ClusterEvent

//case class ClusterUnjoin(system: String, seedNodes: List[HostPort]) extends ClusterEvent

case class ClusterMemberUp(system: String,member: ClusterMember) extends ClusterEvent

case class ClusterMemberUnreachable(system: String,member: ClusterMember) extends ClusterEvent

case class ClusterMemberRemoved(system: String,member: ClusterMember) extends ClusterEvent

case class ClusterMemberExited(system: String,member: ClusterMember) extends ClusterEvent


case class ClusterMetricMemory(system: String,
                               memberAddress: HostPort,
                               date: String,
                               usedHeapMB: Double,
                               committedHeapMB: Double,
                               maxHeapMB: Option[Double]) extends ClusterEvent

case class ClusterMetricCPU(system: String,
                            memberAddress: HostPort,
                            date: String,
                            systemLoadAverage: Double,
                            cpuCombined: Option[Double],
                            cpuStolen: Option[Double],
                            processors: Int
                            ) extends ClusterEvent


object ClusterEventUtil{
  def label(e:ClusterEvent) = {
    e match {
      case ev:CurrentClusterStateInitial => s"CurrentClusterStateInitial[ ${ev.system}, ${ev.members}} ]"
//      case ev:ClusterUnjoin => s"ClusterUnjoin[ ${ev.system}]"
      case ev:ClusterMemberUp => s"MemberUp ${ev.system} ${ev.member.labelSimple}"
      case ev:ClusterMemberUnreachable => s"MemberUnreachable ${ev.system} ${ev.member.labelSimple}"
      case ev:ClusterMemberRemoved => s"MemberRemoved ${ev.system} ${ev.member.labelSimple}"
      case ev:ClusterMemberExited => s"MemberExited ${ev.system} ${ev.member.labelSimple}"
//      case ev: ClusterMetricCPU =>
//        s"Member CPU Stats ${ev.system} Load Avg: ${ev.systemLoadAverage} " +
//        s"CPU Combined: ${ev.cpuCombined.getOrElse("n/a")} CPU Stolen: ${ev.cpuStolen.getOrElse("n/a")}" +
//        s"# Processors: ${ev.processors} | ${ev.memberAddress.label} @ ${ev.date}"
//      case ev: ClusterMetricMemory =>
//        s"Member Memory Stats ${ev.system} Used Heap (MB): ${ev.usedHeapMB} " +
//          s"Committed Heap (MB): ${ev.committedHeapMB} Max Heap (MB): ${ev.maxHeapMB.getOrElse("n/a")}" +
//          s"| ${ev.memberAddress.label} @ ${ev.date}"
    }
  }
}


case class DiscoveryBegun(system: String, seedNodes: List[HostPort]) extends ClusterProtocol

case class DiscoveredCluster(
                              system: String,
                              seeds: List[HostPort],
                              status: String,
                              members: Set[ClusterMember],
                              dependencies: Seq[RoleDependency]) extends ClusterProtocol {

  def getRoles:Seq[String] = members.foldLeft[Set[String]](Set.empty[String])((a,b) => b.roles ++ a ).toSeq

  def getNodesByRole(role:String) = members.filter(_.roles.contains(role))

}

case class ClusterMember( address: HostPort, roles:Set[String], state:NodeState) {
  def label = address.label + s" roles[${roles.mkString(",").map(r => r)}] status[$state]"
  def labelSimple = address.label
}

case class HostPort(host: String, port: Int){
  def label = host +":"+port
}

object HostPortUtil {
  def apply(hp: HostPortForm): HostPort =
    HostPort(hp.host,
      try {
        hp.port.toInt
      } catch {
        case e: Throwable => 0
      }
    )
}

case class RoleDependency(roles:Seq[String], dependsOn:Seq[String], tpe:ClusterDependency)

case class HostPortForm(host: String, port: String)

case class ClusterForm(name: String, selfHost:String, seeds: List[HostPortForm])

object ClusterForm {
  def initial: ClusterForm = ClusterForm("", "127.0.0.1", List(HostPortForm("", "")))
}


