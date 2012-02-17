package com.codecommit.cccp
package util

import net.liftweb.json._ 

abstract class Command {
	def toSExpr: String = toString
}

case class NonJsonCommand(str: String) extends Command {
	override def toSExpr = {
		str
	}
} 

object Command {
	def read(str: String): Command = {
		implicit val formats = DefaultFormats
		try {
			val json = parse(str)
			val t = (json \ "swank").extract[String]

			t match {
    			case "init-connection" => json.extract[InitConnection]
    			case "link-file"  	   => json.extract[LinkFile]
    			case "unlink-file"     => json.extract[UnlinkFile]
    			case "edit-file"       => {
    				val args = (json \ "args").extract[List[Map[String, String]]]
    				val callId = (json \ "callId").extract[Int]
					val res = new EditFile((json \ "swank").extract[String], 
								       (json \ "file-name").extract[String],
								       args, callId)
					res		       
    			}
    			case _ => NonJsonCommand(str)	
			}
		}
		catch{
			case e: Exception => NonJsonCommand(str) 
		}
    }
}

case class InitConnection(swank: String, args: List[Connection], callId: Int) extends Command {
	override def toSExpr = {
		val res = "(:swank-rpc (swank:init-connection (:protocol \"%s\" :host \"%s\" :port %s)) %s)" format (args(0).protocol, args(0).host, args(0).port, callId)
		res
	}	
}
case class Connection(protocol: String, host: String, port: Int)
case class LinkFile(swank: String, args: List[LinkFileArgs], callId: Int) extends Command {
	override def toSExpr = {
		val res = "(:swank-rpc (swank:init-file %s) %s)" format (args(0).`file-name`, callId)
		"0000" + Integer.toString(res.length , 16) + res
	}	
}

case class LinkFileArgs(id: String, `file-name`: String)
case class UnlinkFile(swank: String, args: List[UnlinkFileArgs], callId: Int) extends Command {
	override def toSExpr = {
		val res = "(:swank-rpc (swank:unlink-file %s) %s)" format (args(0).`file-name`, callId)
		//"0000" + Integer.toString(res.length , 16) + res
		res
	}	
}
case class UnlinkFileArgs(`file-name`: String)
case class EditFile(swank: String, `file-name`: String, args: List[Map[String, String]], callId: Int) extends Command {
	override def toSExpr = {
		val listArgs = args map(a => (a.map{ case(k,v) => ":" + k + " " + v } toList).head)
		val strArgs = listArgs.aggregate("")(_ + " " + _, _ + " " + _).trim
		val res = "(:swank-rpc (swank:edit-file %s (%s)) %s)" format (`file-name`,strArgs, callId)	
		//"0000" + Integer.toString(res.length , 16) + res
		res
	}	
}
