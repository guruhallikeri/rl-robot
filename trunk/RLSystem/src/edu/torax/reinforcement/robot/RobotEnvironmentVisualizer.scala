package edu.torax.reinforcement.robot

import java.awt.Graphics
import java.awt.Component
import java.awt.Graphics2D
import java.awt.Color
import java.awt.BasicStroke

class RobotEnvironmentVisualizer extends Component {
  var environment: RobotEnvironment = null
  
  override def paint(gr: Graphics)
  {
	val g = gr.asInstanceOf[Graphics2D]
    val offx = 5
    val offy = 5
    
    g.setColor(getBackground)
    g.fillRect(0, 0, size.width, size.height)
    
    val wid = size.width - 2*offx
    val hei = size.height - 2*offy
  
    g.setColor(Color.BLACK)
    g.setStroke(new BasicStroke(2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER))
   	g.drawRect(offx, offy, wid, hei)

    if (environment != null) {
      val xCoef: Double = (0.0 + wid) / environment.width
      val yCoef: Double = (0.0 + hei) / environment.height
     
      // draw obstcales
      g.setColor(Color.GRAY)
	    
      for (obs <- environment.obstacles) {
    	obs match {
	      case polyObs: PolygonalRobotObstacle =>
	        val arrX: Array[Int] = (polyObs.boundPoints map (x => offx + (x.x * xCoef).toInt)).toArray
	        val arrY: Array[Int] = (polyObs.boundPoints map (x => offy + (x.y * yCoef).toInt)).toArray
	        g.drawPolygon(arrX, arrY, arrX.size)
	      case _ =>
	        throw new Exception("Not known type of obstacle")
    	}
      }
      // draw goal position
      g.setColor(Color.red)
      val gPos = environment.goalPosition(environment)
      val gPosReal = (offx + (xCoef * gPos.x).toInt, offy + (yCoef * gPos.y).toInt)  
      g.drawLine(gPosReal._1 - 5, gPosReal._2 - 5, gPosReal._1 + 5, gPosReal._2 + 5)
      g.drawLine(gPosReal._1 - 5, gPosReal._2 + 5, gPosReal._1 + 5, gPosReal._2 - 5)
	    
      // draw model
      g.setColor(Color.blue)
      val modelX: Array[Int] = (environment.model.boundBox map (x => offx + (x.x * xCoef).toInt)).toArray
      val modelY: Array[Int] = (environment.model.boundBox map (x => offy + (x.y * yCoef).toInt)).toArray
      g.drawPolygon(modelX, modelY, modelX.size)
     
      val pos = environment.model.position
      val dir = environment.model.direction
      g.setStroke(new BasicStroke(1.0f))
      g.drawLine(offx + (xCoef * pos.x).toInt, 
                 offy + (yCoef * pos.y).toInt, 
                 offx + (xCoef * (pos.x + dir.x*environment.model.width)).toInt, 
                 offy + (yCoef * (pos.y + dir.y*environment.model.height)).toInt)
      
    }	
  }
}
