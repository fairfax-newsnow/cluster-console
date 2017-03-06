package com.boldradius.astrolabe.http

import akka.http.scaladsl.marshalling._
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling._
import akka.stream.Materializer
import upickle.default._

import scala.concurrent.{ Future, ExecutionContext }
import scala.concurrent.duration._

import Json._

/**
 * @author Dave Sugden
 *         Dmitri Carpov
 */

object UpickleMarshaller extends UpickleMarshaller

trait UpickleMarshaller {
  implicit def upickleFromRequestUnmarshaller[T: Reader]: FromRequestUnmarshaller[T] =
    new Unmarshaller[HttpRequest, T] {
      def apply(req: HttpRequest)(implicit ec: ExecutionContext, materializer: Materializer): Future[T] = {
        req.entity.withContentType(ContentTypes.`application/json`).toStrict(1.second)
          .map(_.data.toArray).map(x => read[T](new String(x)))
      }
    }

  implicit def upickleFromResponseUnmarshaller[T: Reader]: FromResponseUnmarshaller[T] =
    new Unmarshaller[HttpResponse, T] {
      def apply(res: HttpResponse)(implicit ec: ExecutionContext, materializer: Materializer): Future[T] = {
        res.entity.withContentType(ContentTypes.`application/json`).toStrict(1.second).map(_.data.toArray).map(x => read[T](new String(x)))
      }
    }

  implicit def upickleToResponseMarshaller[T: Writer](implicit fm: Materializer): ToResponseMarshaller[T] =
    Marshaller.withFixedContentType[T, MessageEntity](
      ContentType(MediaTypes.`application/json`, () ⇒ HttpCharset.custom("*"))
    )(tp => write[T](tp))

  implicit def upickleToEntityMarshaller[T: Writer]: ToEntityMarshaller[T] =
    Marshaller.withFixedContentType[T, MessageEntity](
      ContentType(MediaTypes.`application/json`, () ⇒ HttpCharset.custom("*"))
    )(tp => HttpEntity(write[T](tp)))
}
