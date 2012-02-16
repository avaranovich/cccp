package com.codecommit.cccp

import org.specs2.mutable.Specification
import net.liftweb.json._ 

case class InitConnection(swank: String, args: List[Connection])
case class Connection(protocol: String, host: String)

case class InitFile(swank: String, args: List[InitFileArgs])
case class InitFileArgs(id: String, `file-name`: String)

case class UnlinkFile(swank: String, args: List[UnlinFileArgs])
case class UnlinFileArgs(`file-name`: String)

case class EditFile(swank: String, `file-name`: String, args: List[Map[String, String]])

object JsonSpecs extends Specification {
	//#(swank:init-connection (:protocol protocol :host host :port port))
	"agent" should {
		"support init-connection json message" in {
			implicit val formats = DefaultFormats
			val msg1 = """{"swank":"init-connection", "args":[{"protocol": "http", "host": "localhost"}]}"""
			val obj = parse(msg1).extract[InitConnection]
			obj.swank mustEqual "init-connection"
			obj.args.length mustEqual 1
			obj.args(0).protocol mustEqual "http"
			obj.args(0).host mustEqual "localhost"
		}
		"support link-file json message" in {
			implicit val formats = DefaultFormats
			val msg1 = """{"swank":"link-file", "args":[{"id": "id", "file-name":"file-name" }] }"""
			val obj = parse(msg1).extract[InitFile]
			obj.swank mustEqual "link-file"
			obj.args.length mustEqual 1
		}
		// (swank:unlink-file file-name)
		"support unlink-file json message" in {
			implicit val formats = DefaultFormats
			val msg1 = """{"swank":"unlink-file", "args":[{ "file-name":"file-name" }] }"""
			val obj = parse(msg1).extract[UnlinkFile]
			obj.swank mustEqual "unlink-file"
			obj.args.length mustEqual 1
		}
		//(swank:edit-file file-name ((:key1 value1 :key2 value2)))
		"support edit-file json message" in {
			implicit val formats = DefaultFormats
			val msg1 = """{"swank": "edit-file", "file-name":"log.txt", "args":[{"key1": "value1"}, {"key2": "value2"}] }"""
			val json = parse(msg1)
			val args = (json \ "args").extract[List[Map[String, String]]]
			val res = new EditFile((json \ "swank").extract[String], 
							       (json \ "file-name").extract[String],
							       args)
			res.swank mustEqual "edit-file"
			res.`file-name` mustEqual "log.txt"
			res.args.length mustEqual 2
			res.args(0)("key1") mustEqual "value1"
			res.args(1)("key2") mustEqual "value2"
		}

	}
}