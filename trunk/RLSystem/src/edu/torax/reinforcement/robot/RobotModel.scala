package edu.torax.reinforcement.robot
import edu.torax.reinforcement.gutils._
import edu.torax.reinforcement.gutils.Vector._

trait RobotModel {
  def position: Vector						// Position of the center
  def direction: Vector						// Heading vector
  def turn(angle: Double): Unit				// -180.0 <= angle <= 180.0, positive - to the left
  def move(distance: Double): Unit			// 
  def width: Double							// width of robot (perpendicular to the heading vector)
  def height: Double						// height of robot (parallel to the heading vector)
  def distanceTo(p: Vector): Double	// distance from model to the specified point
  def boundBox: List[Vector]				// bounding box of the model
}

class SimpleRobotModel (
  startX: Double, 
  startY: Double, 
  startDx: Double, 
  startDy: Double,
  val width: Double,
  val height: Double 
) extends RobotModel {
  private var pos: Vector = (startX, startY)				// current position
  private var dir: Vector = (startDx, startDy).normalize	// current moving direction
  def position: Vector = pos
  def direction: Vector = dir
  
  def turn(angle: Double): Unit = {		// -180.0 <= angle <= 180.0
    dir = dir rotate Math.toRadians(angle)
  }
  
  def move(distance: Double): Unit = {		
    //assert(Math.abs(force) <= 1.0)
    pos = pos + dir * distance
  }
  
  def distanceTo(p: Vector): Double = {
    val modelPoints = boundBox
    var lastPoint = modelPoints.last
    var res = Double.PositiveInfinity
    for (mp <- modelPoints) {
      res = Math.min(res, Segment(lastPoint, mp) distanceTo p)
      lastPoint = mp
    }
    res
  }
  
  def boundBox: List[Vector] = {
    val pDir = direction rotate 90.0	// perpendicular direction
    //println(direction + "   ----    " + pDir)
    val halfW = width / 2.0
    val halfH = height / 2.0
    val modelPoints = List(position - direction*halfH + pDir*halfW,
                           position + direction*halfH + pDir*halfW,
                           position + direction*halfH - pDir*halfW,
                           position - direction*halfH - pDir*halfW)
    //println(modelPoints)
    modelPoints
  }
}
		