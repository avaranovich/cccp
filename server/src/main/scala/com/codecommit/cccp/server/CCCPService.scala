package com.codecommit.cccp
package server

import akka.actor.Actor

import blueeyes._
import blueeyes.core.data._
import blueeyes.core.http._

import java.io.{ByteArrayOutputStream, CharArrayReader, OutputStreamWriter}

trait CCCPService extends BlueEyesServiceBuilder {
  import FilesActor._
  import MimeTypes._
  import OpChunkUtil._
  
  lazy val files = Actor.actorOf[FilesActor].start()
  
  // TODO content type for operation
  
  val cccpService = service("cccp", "0.1") {
    logging { log => context =>
      request {
        path("/'id/") {
          produce(text/plain) {
            path('version) {
              get { request: HttpRequest[ByteChunk] =>
                implicit val timeout = Actor.Timeout(2 * 60 * 1000)
                
                val agentId = request.headers.get("AgentId")
                log.info("request from " + agentId)

                log.info("accessing history at a version " + request.parameters('version))
                
                val response = files ? RequestHistory(request parameters 'id, request.parameters('version).toInt)
                val back = new blueeyes.concurrent.Future[Option[ByteChunk]]
              
                response.as[Seq[Op]] foreach { ops =>
                  log.info("delivering operations: " + new String(opToChunk(ops).data))
                  back deliver Some(opToChunk(ops))
                }
                
                response onTimeout { _ =>
                  back deliver None
                } 
                
                back map { d => HttpResponse(content = d) }
              }
            } ~
            post { request: HttpRequest[ByteChunk] =>
              for (content <- request.content) {
                log.info("applying operation(s): " + (content.data map { _.toChar } mkString))
                val ops = chunkToOp(content)
                ops foreach { op => files ! PerformEdit(request parameters 'id, op) }
              }
              
              blueeyes.concurrent.Future.sync(HttpResponse(content = None))
            }
          }
        }
      }
    }
  }
}
