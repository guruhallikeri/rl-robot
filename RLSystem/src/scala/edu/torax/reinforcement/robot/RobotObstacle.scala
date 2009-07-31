package edu.torax.reinforcement.robot
import gutils._

trait RobotObstacle {
	def distanceTo(polygon: List[Vector]): Double	
	def distanceTo(sector: Sector): Double					// return Double.PositiveInfinity if does not overlap with Sector
	def distanceTo(point: Vector): Double
	def contains(point: Vector): Boolean						// tells wheter specified point lies inside the obstacle
  def makeClone: RobotObstacle
  def toXML: xml.Elem
}

class PolygonalRobotObstacle extends RobotObstacle
{
	private var points: List[Vector] = Nil 
	def boundPoints: List[Vector] = points

	def distanceTo(polygon: List[Vector]): Double = Polygon.distanceBetween(polygon, points)

	def contains(point: Vector): Boolean = Polygon.containsPoint(points, point)

	def distanceTo(sect: Sector): Double = {
		val (overlaps, dist) = sect overlapsWithPolygon points
		if (overlaps) dist else Double.PositiveInfinity
	}

	def distanceTo(point: Vector): Double = Polygon.distanceBetween(points, point)
 
	override def toString: String = "Obstacle<" + points.mkString(",") + ">\n"
 
	def makeClone = {
	  val r = new PolygonalRobotObstacle
	  r.points = this.points map (x => x.clone)
    r
	}
 
	def toXML = 
    <RobotObstacle type="polygonal">
		 	{points map (_.toXML)}
    </RobotObstacle>
}

object PolygonalRobotObstacle {
  val maxTries = 10
	def generate(obstacles: List[RobotObstacle], Xmax: Double, Ymax: Double, gap: Double,
						Rmin: Double, Rmax: Double, trial: Int): PolygonalRobotObstacle = {
		if (trial > maxTries) return null
	  
	  val center = Vector(Rmax + Math.random*(Xmax - 2*Rmax), Rmax + Math.random*(Ymax - 2*Rmax))
		val Rm = (Double.PositiveInfinity /: obstacles) ((x,y) => (x min y.distanceTo(center)) - 0.5*gap) min Rmax
		
		if (Rm < Rmin) {
			generate(obstacles, Xmax, Ymax, gap, Rmin, Rmax, trial+1)
		} else {
			val R = Rmin + Math.random*(Rm - Rmin)
			val obs = new PolygonalRobotObstacle
			var theta = Math.random * 2.0*Math.Pi/3.0
			var n = 0
			while (n<10 && theta < 2*Math.Pi) {
				n += 1
				obs.points = (center + Vector(R*Math.cos(theta), R*Math.sin(theta))) :: obs.points 
				theta += Math.random * 2.0*Math.Pi/3.0
			}
			obs
		}
	}
 
  def fromXML(node: xml.NodeSeq) = {
    if ((node \ "@type").text != "polygonal")
      throw new Exception("Unknown type of RobotObstacle: " + (node \ "@type").text)
    else {
      val r = new PolygonalRobotObstacle
      r.points = ((node \ "vector") map (Vector.fromXML(_))).toList
      r
    }
  }
}
	