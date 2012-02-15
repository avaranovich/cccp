package com.codecommit.cccp

import org.specs2.mutable.Specification
import net.liftweb.json._ 

case class InitConnection(swank: String, args: List[Connection])
case class Connection(protocol: String, host: String)

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
	}

/*
msg1 = {'swank':'init-connection', 'args': [{'protocol': 'http', 'host': 'localhost'}]}
print json.dumps(msg1)
 
#(swank:link-file id file-name)
msg2 = {'swank': 'link-file', 'args':['id', 'file-name'] }
print json.dumps(msg2)

#(swank:unlink-file file-name)
msg3 = {'swank': 'unlink-file', 'args':['file-name'] }
print json.dumps(msg3)

#(swank:edit-file file-name ((:key1 value1 :key2 value2)))
msg3 = {'swank': 'edit-file', 'args':['file-name', [{'key1': 'value1', 'key2': 'value2'}]] }
print json.dumps(msg3)
	*/


}