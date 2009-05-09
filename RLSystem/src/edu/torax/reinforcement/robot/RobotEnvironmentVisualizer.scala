package edu.torax.reinforcement.robot

import java.awt.Graphics
import java.awt.Component
import java.awt.Graphics2D
import java.awt.Color
import java.awt.BasicStroke
import java.awt.RenderingHints
import reinforcement.gutils.Vector
  
class RobotEnvironmentVisualizer extends Component {
  var environment: RobotEnvironment = null
  
  override def paint(gr: Graphics)
  {
		val g = gr.asInstanceOf[Graphics2D]
		val hints = new RenderingHints(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
		hints.add(new RenderingHints(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_SPEED))
		g.setRenderingHints(hints)

		val offx = 5
    val offy = 5
    
    g.setColor(getBackground)
    g.fillRect(0, 0, size.width, size.height)
    
    val wid = size.width - 2*offx
    val hei = size.height - 2*offy
  
    g.setColor(Color.BLACK)
    g.setStroke(new BasicStroke(1.3f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER))
   	g.drawRect(offx, offy, wid, hei)

    if (environment != null) {
      val xCoef: Double = (0.0 + wid) / environment.width
      val yCoef: Double = (0.0 + hei) / environment.height
      val model = environment.model
      
      def transX(v: Vector) = offx + (v.x * xCoef).toInt
      def transY(v: Vector) = hei + offy - (v.y * yCoef).toInt
      
      // draw obstcales
      g.setColor(Color.GRAY)
	    
      for (obs <- environment.obstacles) {
      	obs match {
      		case polyObs: PolygonalRobotObstacle =>
      			val arrX: Array[Int] = (polyObs.boundPoints map (v => transX(v))).toArray
      			val arrY: Array[Int] = (polyObs.boundPoints map (v => transY(v))).toArray
      			g.drawPolygon(arrX, arrY, arrX.size)
      		case _ =>
      			throw new Exception("Not known type of obstacle")
      	}
      }
      // draw goal position
      val gPos = environment.goal
      val gPosReal = (transX(gPos), transY(gPos))  
      g.setColor(Color.red)
      g.drawLine(gPosReal._1 - 5, gPosReal._2 - 5, gPosReal._1 + 5, gPosReal._2 + 5)
      g.drawLine(gPosReal._1 - 5, gPosReal._2 + 5, gPosReal._1 + 5, gPosReal._2 - 5)
	    
      //draw vision sectors
      val sectorAngle = environment.visionAngle / RobotState.visionSectorsNumber
      g.setColor(Color.white)
      g.setStroke(new BasicStroke(0.5f))
      for (i <- 0 until RobotState.visionSectorsNumber) {
        val p1 = RobotState.sectorStart(i, model)//model.position //+ model.direction*(model.height/2.0)
        val p2 = p1 + (model.direction rotate (environment.visionAngle/2.0 - i*sectorAngle))*8
        val p3 = p1 + (model.direction rotate (environment.visionAngle/2.0 - (i+1)*sectorAngle))*8
        g.drawLine(transX(p1), transY(p1), transX(p2), transY(p2))
        g.drawLine(transX(p1), transY(p1), transX(p3), transY(p3))
      }

      // draw model
      val modelBoundBox = model.boundBox                                                                         
      val modelX: Array[Int] = (modelBoundBox map (v => transX(v))).toArray
      val modelY: Array[Int] = (modelBoundBox map (v => transY(v))).toArray
      g.setColor(Color.blue)
      g.setStroke(new BasicStroke(1.2f))
      g.drawPolygon(modelX, modelY, modelX.size)

      // draw direction
      val pos = model.position
      val dir = pos + Vector(model.direction.x*model.width, model.direction.y*model.height)
      g.setStroke(new BasicStroke(1.5f))
      g.drawLine(transX(pos), transY(pos), transX(dir), transY(dir))  
      
    }	
  }
}
