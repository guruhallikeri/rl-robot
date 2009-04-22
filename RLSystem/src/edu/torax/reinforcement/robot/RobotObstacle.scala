package edu.torax.reinforcement.robot
import gutils._

trait RobotObstacle {
  def collides(model: RobotModel): Boolean	
  def distanceToSector(sect: Sector): Double	// return Double.PositiveInfinity if does not overlap with Sector
  def distanceTo(p: Vector): Double
}

class PolygonalRobotObstacle extends RobotObstacle
{
  private var points: List[Vector] = Nil 
  def boundPoints: List[Vector] = points
  
  def distanceTo(polygon: List[Vector]): Double = {
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
   
	def collides(model: RobotModel): Boolean = (this distanceTo model.boundBox) < RobotEnvironment.MaxDistanceToGoal
  
  def distanceToSector(sect: Sector): Double = {
    val (overlaps, dist) = sect.overlapsWithPolygon(points)
    if (overlaps) dist else Double.PositiveInfinity
  }
  
  def distanceTo(point: Vector): Double = {
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
    // if we exceeded max number of attempts to generate obstacle
    // (maybe there is too much obstacles in the environment already) then just return no object
    //if (maxAttempts == 0) return None
    
//	val center = Vector(gap + Math.random*(Xmax - 2*(Rmax+gap)), gap + Math.random*(Ymax - 2*(Rmax+gap)))
	val center = Vector(Rmax + Math.random*(Xmax - 2*Rmax), Rmax + Math.random*(Ymax - 2*Rmax))
	val Rm = (Double.PositiveInfinity /: obstacles) ((x,y) => x min y.distanceTo(center)) min Rmax
	println("Obstacle generation: " + center + "   Rm: " + Rm)
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
	  obs
	  //Some(obs)
	}
  }
}
