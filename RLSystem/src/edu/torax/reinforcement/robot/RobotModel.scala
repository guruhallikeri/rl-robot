package edu.torax.reinforcement.robot
import edu.torax.reinforcement.gutils.Vector

trait RobotModel {
  def position: (Double, Double)		// Position of the center
  def direction: (Double, Double)		// Heading vector
  def turn(angle: Double): Unit		// -180.0 <= angle <= 180.0
  def move(force: Double): Unit		// -1.0 <= force <= 1.0
  def width: Double					// width of robot (perpendicular to the heading vector)
  def height: Double					// height of robot (parallel to the heading vector)
}

class SimpleRobotModel(startX: Double, startY: Double, startDx: Double, startDy: Double) extends RobotModel {
  private var pos: Vector = (startX, startY)			// current position
  private var dir: Vector = (startDx, startDy)		// current moving direction
  def position: (Double, Double) = pos
  def direction: (Double, Double) = dir
  
  def turn(angle: Double): Unit = {		// -180.0 <= angle <= 180.0
    dir = dir rotate Math.toRadians(angle)
  }
  def move(force: Double): Unit = {		// -1.0 <= force <= 1.0
    assert(Math.abs(force) <= 1.0)
    pos = pos + dir * force
  }
  val width = RobotEnvironment.robotWidth
  val height = RobotEnvironment.robotHeight
}
		