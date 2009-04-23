package edu.torax.reinforcement.robot
import gutils._

trait RobotObstacle {
	def distanceTo(polygon: List[Vector]): Double	
	def distanceTo(sector: Sector): Double					// return Double.PositiveInfinity if does not overlap with Sector
	def distanceTo(point: Vector): Double
	def contains(point: Vector): Boolean						// tells wheter specified point lies inside the obstacle
}

class PolygonalRobotObstacle extends RobotObstacle
{
	private var area = Double.NaN
	private var points: List[Vector] = Nil 
	def boundPoints: List[Vector] = points

	def distanceTo(polygon: List[Vector]): Double = {
	  if (this contains polygon.head) return 0.0
   
		var lastPolP = polygon.last
		var lastObsP = points.last
		var res = Double.PositiveInfinity
		for (mp <- polygon) {
			for (p <- points) {
				res = res min (Segment(lastPolP, mp) distanceTo Segment(lastObsP, p))
				lastObsP = p
			}
			lastPolP = mp
		}
		res
	}

	def contains(point: Vector): Boolean = {
		var tmp = 0.0
		var last = points.last
		for (p <- points) {
			tmp += Math.abs((last - point)^(p - point))
			last = p
		}
		//println(2*area + "   -----   " + tmp)
		Math.abs(2*area - tmp) < Vector.eps
	}

	def distanceTo(sect: Sector): Double = {
		val (overlaps, dist) = sect.overlapsWithPolygon(points)
		if (overlaps) dist else Double.PositiveInfinity
	}

	def distanceTo(point: Vector): Double = {
		if (this contains point) return 0.0

		var last = points.last
		var res = Double.PositiveInfinity
		for (p <- points) {
			res = res min (Segment(last, p) distanceTo point)
			last = p
		}
		res
	}
}

object PolygonalRobotObstacle {
	def generate(obstacles: Array[RobotObstacle], Xmax: Double, Ymax: Double, gap: Double,
			Rmin: Double, Rmax: Double/*, maxAttempts: Int*/): /*Option[*/PolygonalRobotObstacle/*]*/ = {
		//	val center = Vector(gap + Math.random*(Xmax - 2*(Rmax+gap)), gap + Math.random*(Ymax - 2*(Rmax+gap)))
		val center = Vector(Rmax + Math.random*(Xmax - 2*Rmax), Rmax + Math.random*(Ymax - 2*Rmax))
		val Rm = (Double.PositiveInfinity /: obstacles) ((x,y) => x min y.distanceTo(center)) min Rmax
		//println("Obstacle generation: " + center + "   Rm: " + Rm)
		if (Rm < Rmin) {
			generate(obstacles, Xmax, Ymax, gap, Rmin, Rmax/*, maxAttempts-1*/)
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
			obs.area = Area of obs.points
			obs
		}
	}
}
