package com.codecommit.cccp

import org.specs2.mutable.Specification
import net.liftweb.json._ 

import util._

object JsonSpecs extends Specification {
	val initConnectionJson = """{"swank":"init-connection", "args":[{"protocol": "http", "host": "localhost", "port": 123}], "callId" : 1}"""
	val linkFileJson = """{"swank":"link-file", "args":[{"id": "id", "file-name":"file-name" }], "callId": 1}"""
	val unlinkFileJson = """{"swank":"unlink-file", "args":[{ "file-name":"file-name" }], "callId": 1 }"""
	val editFileJson = """{"swank": "edit-file", "file-name":"log.txt", "args":[{"key1": "value1"}, {"insert": "foo"}], "callId": 1}"""
	val sExpr = "(swank:init-connection (:protocol protocol :host host :port port) 1)"
	//#(swank:init-connection (:protocol protocol :host host :port port))
	"agent json parser" should {
		"support init-connection json message" in {
			implicit val formats = DefaultFormats
			val obj = parse(initConnectionJson).extract[InitConnection]
			obj.swank mustEqual "init-connection"
			obj.args.length mustEqual 1
			obj.args(0).protocol mustEqual "http"
			obj.args(0).host mustEqual "localhost"
			obj.args(0).port mustEqual 123
		}
		"support link-file json message" in {
			implicit val formats = DefaultFormats
			val obj = parse(linkFileJson).extract[LinkFile]
			obj.swank mustEqual "link-file"
			obj.args.length mustEqual 1
		}
		// (swank:unlink-file file-name)
		"support unlink-file json message" in {
			implicit val formats = DefaultFormats
			val obj = parse(unlinkFileJson).extract[UnlinkFile]
			obj.swank mustEqual "unlink-file"
			obj.args.length mustEqual 1
		}
		//(swank:edit-file file-name ((:key1 value1 :key2 value2)))
		"support edit-file json message" in {
			implicit val formats = DefaultFormats
			val json = parse(editFileJson)
			val args = (json \ "args").extract[List[Map[String, String]]]
			val callId = (json \ "callId").extract[Int]
			val res = new EditFile((json \ "swank").extract[String], 
							       (json \ "file-name").extract[String],
							       args, callId)
			res.swank mustEqual "edit-file"
			res.`file-name` mustEqual "log.txt"
			res.args.length mustEqual 2
			res.args(0)("key1") mustEqual "value1"
			res.args(1)("insert") mustEqual "foo"
		}
	}

	"agent" should {
		"be able to create an InitConnection command from json" in {
			Command.read(initConnectionJson).asInstanceOf[AnyRef].getClass.getSimpleName mustEqual "InitConnection"
		}
		"be able to create an LinkFile command from json" in {
			Command.read(linkFileJson).asInstanceOf[AnyRef].getClass.getSimpleName mustEqual "LinkFile"
		}
		"be able to create an UnLinkFile command from json" in {
			Command.read(unlinkFileJson).asInstanceOf[AnyRef].getClass.getSimpleName mustEqual "UnlinkFile"
		}
		"be able to create an EditFile command from json" in {
			Command.read(editFileJson).asInstanceOf[AnyRef].getClass.getSimpleName mustEqual "EditFile"
		}
		"recognize non-json command" in {
			Command.read(sExpr).asInstanceOf[AnyRef].getClass.getSimpleName mustEqual "NonJsonCommand"
		}
		"generate a valid SWANK from json for EditFile command" in {
			Command.read(editFileJson).toSExpr mustEqual "(:swank-rpc (swank:edit-file \"log.txt\" (:key1 value1 :insert \"foo\")) 1)"
		}
		"pass default SWANK command as is" in {
			Command.read(sExpr).toSExpr mustEqual "(swank:init-connection (:protocol protocol :host host :port port) 1)"
		}
		"support conversion of edit-performed command SEXpr -> Json" in {
			Command.fromSExpr("(:edit-performed \"/Users/varanovich/Desktop/foo.txt\" (:retain 1 :insert \"ra\" :retain 4))") mustEqual "foo"
		}
	}
}