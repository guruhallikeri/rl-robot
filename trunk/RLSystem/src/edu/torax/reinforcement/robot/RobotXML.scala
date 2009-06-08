package edu.torax.reinforcement.robot
import java.io._

object RobotXML {
	def save(fileName: String, nodes: xml.Node) {
	  val file = new FileWriter(fileName)
	  file.write((new xml.PrettyPrinter(500, 2)).format(nodes))
	  file.close()
	}
}
