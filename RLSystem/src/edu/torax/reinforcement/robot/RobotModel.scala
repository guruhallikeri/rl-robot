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
  def makeClone: RobotModel
  def toXML: xml.Elem
}

object RobotModel {
  def fromXML(node: xml.NodeSeq): RobotModel = (node \ "@type").text match {
    case "simple" => new SimpleRobotModel(node)
    case unknown => throw new Exception("Unknown type of RobotModel: " + unknown)
  }
}

class SimpleRobotModel (
  startX: Double, 
  startY: Double, 
  startDx: Double, 
  startDy: Double,
  val width: Double,
  val height: Double 
) extends RobotModel {
  def toXML = 
    <RobotModel type="simple">
  		<startX>{startX}</startX>
  		<startY>{startY}</startY>
  		<startDx>{startDx}</startDx>
  		<startDy>{startDy}</startDy>
  		<width>{width}</width>
  		<height>{height}</height>
  		<pos>{pos.toXML}</pos>
      <dir>{dir.toXML}</dir>
    </RobotModel>
  
  def this(node: xml.NodeSeq) = {
    this(
      (node \ "startX").text.toDouble,
      (node \ "startY").text.toDouble,
      (node \ "startDx").text.toDouble,
      (node \ "startDy").text.toDouble,
      (node \ "width").text.toDouble,
      (node \ "height").text.toDouble
    )
    pos = Vector.fromXML(node \ "pos" \ "vector")
    dir = Vector.fromXML(node \ "dir" \ "vector")
  }
  
  def makeClone: RobotModel = {
    val r = new SimpleRobotModel(startX, startY, startDx, startDy, width, height)
    r.pos = this.pos
    r.dir = this.dir
    r
  }
  
  private var pos: Vector = (startX, startY)				// current position
  private var dir: Vector = (startDx, startDy).normalize	// current moving direction
  def position: Vector = pos
  def direction: Vector = dir
  
  def turn(angle: Double): Unit = {		// -180.0 <= angle <= 180.0
    dir = dir rotate angle
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