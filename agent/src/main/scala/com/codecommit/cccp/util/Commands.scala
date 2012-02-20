package com.codecommit.cccp
package util

import scala.util.parsing.input.CharSequenceReader
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

    def fromSExpr(str: String){
    	 //(:edit-performed "/Users/varanovich/Desktop/foo.txt" (:retain 1 :insert "ra" :retain 4))
	   val r = SExp.read(new CharSequenceReader(str))
	   r.toReadableString
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
		"(:swank-rpc (swank:link-file \"%s\" \"%s\") %s)" format (args(0).id, args(0).`file-name`, callId)
	}	
}

case class LinkFileArgs(id: String, `file-name`: String)
case class UnlinkFile(swank: String, args: List[UnlinkFileArgs], callId: Int) extends Command {
	override def toSExpr = {
		"(:swank-rpc (swank:unlink-file \"%s\") %s)" format (args(0).`file-name`, callId)
	}	
}
case class UnlinkFileArgs(`file-name`: String)
case class EditFile(swank: String, `file-name`: String, args: List[Map[String, String]], callId: Int) extends Command {
	def escape(k: String, v: String) : String = {
		println (k + " " + v)
		if ((k == "insert") || (k == "delete")){
			k + " \""+v+"\""
		}
		else k + " " + v
	}
	override def toSExpr = {
		val listArgs = args map(a => (a.map{ case(k,v) => ":" + escape(k,v) } toList).head)
		val strArgs = listArgs.aggregate("")(_ + " " + _, _ + " " + _).trim
		"(:swank-rpc (swank:edit-file \"%s\" (%s)) %s)" format (`file-name`,strArgs, callId)	
	}	
}
