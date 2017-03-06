package com.boldradius.astrolabe.http

import akka.actor.{ Actor, ActorLogging, ActorRef, Props }
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.pattern.pipe
import akka.stream.{ ActorMaterializer, Materializer }
import akka.util.Timeout
import com.boldradius.astrolabe.core.LogF

import scala.concurrent.Future
import scala.util.{ Failure, Success }

class HttpServiceActor(host: String, port: Int, selfTimeout: Timeout, val socketPublisherRouter: ActorRef)(implicit val materializer: ActorMaterializer)
    extends Actor
    with ClusterConsoleRoutes
    with ActorLogging {

  import context.dispatcher

  def startHttpServer: Future[ServerBinding] = {
    // Boot the server
    Http(context.system).bindAndHandle(routes(materializer), host, port).pipeTo(self)
  }

  startHttpServer.onComplete {
    case Success(binding) =>
      log.info("\r\n\r\nHttpService started, ready to service requests on {}", binding.localAddress)
    case Failure(ex) ⇒
      log.error("Startup failed", ex)
      sys.exit(1)
  }

}

object HttpServiceActor {
  def props(host: String, port: Int, selfTimeout: Timeout, socketPublisherRouter: ActorRef)(implicit materializer: ActorMaterializer): Props =
    Props(new HttpServiceActor(host, port, selfTimeout, socketPublisherRouter))
}

