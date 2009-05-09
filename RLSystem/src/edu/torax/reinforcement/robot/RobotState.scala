package edu.torax.reinforcement.robot
import framework.State
import gutils._

case class RobotState(env: RobotEnvironment) extends State {
	private val maxDist = Vector(env.width, env.height).length
	private val modelBound = env.model.boundBox
 
	private val sectorAngle = env.visionAngle / RobotState.visionSectorsNumber
	val ranges = (for (i <- 0 until RobotState.visionSectorsNumber) yield getClosestObstacleDistance(
			modelBound, env.obstacles,
			Sector(RobotState.sectorStart(i, env.model)/*, env.model.position *//*+ env.model.direction*(env.model.height/2.0)*/,					// beginning of vision sector
					env.model.direction rotate ((env.visionAngle-sectorAngle)/2.0 - i*sectorAngle), 	
					maxDist, sectorAngle)
	)).toList

	val goalDistance = Polygon.distanceBetween(modelBound, env.goal)
	val goalAngle = Vector.angleBetween(env.model.direction, env.goal - env.model.position)//Math.acos(env.model.direction * modelToGoalVec.normalize)

	def encode: List[Double] = coarseEncode(ranges, goalDistance, goalAngle)

	private def getClosestObstacleDistance(modelBound: List[Vector], obstacles: Array[RobotObstacle], sect: Sector): Double = {
	  if (obstacles exists (x => (x distanceTo modelBound) < RobotEnvironment.MaxDistanceToObs)) 
     return 0.0
   
		var minObsDist = (maxDist /: obstacles) { (x,y) => x min (y distanceTo sect) }
		val minBoundsDist = (maxDist /: env.envBounds) ((x,y) => { val t = sect overlapsWithSegment y; if (t._1) x min t._2 else x } )
		minObsDist min minBoundsDist
	}

	import RobotState._
	def encoder(x: Double, N: Int, r: Double): List[Double] = {
			val w = 4*N/r
			val res = for (i <- 1 to N) yield 1.0 / (1.0 + Math.exp(w*((2*i - 1)/2.0*r/N - x)))
			res.toList
	}
	private def coarseEncode(ranges: List[Double], goalDist: Double, goalAngle: Double): List[Double] = {
		
		(ranges flatMap (x => encoder(x, coarseRangeSensorN, coarseRangeSensorR))) ::: 
		encoder(goalDist, coarseGoalDistanceN, coarseGoalDistanceR) :::
		encoder(Math.Pi - goalAngle, coarseGoalAngleN, coarseGoalAngleR) :::
		encoder(goalAngle - Math.Pi, coarseGoalAngleN, coarseGoalAngleR)
//				(ranges map (x => Math.min(x / coarseRangeSensorR, 1.0))) ::: 
//        List(Math.min(goalDist/coarseGoalDistanceR, 1.0), goalAngle/2.0/Math.Pi)
	}  
	//println(this)
	//println(encode)
	override def toString = { "Ranges: " + ranges + ";\n GDist: " + goalDistance + "; GAngle: " + Math.toDegrees(goalAngle) + "(" + goalAngle + ")" +
		"\nGoalD: " + encoder(goalDistance, coarseGoalDistanceN, coarseGoalDistanceR) +
		"\nGoalA1: " + encoder(Math.Pi - goalAngle, coarseGoalAngleN, coarseGoalAngleR) +
		"\nGoalA2: " + encoder(goalAngle - Math.Pi, coarseGoalAngleN, coarseGoalAngleR)
  }
}

object RobotState {
	val coarseRangeSensorN = 3
	val coarseRangeSensorR = 8.0
	val coarseGoalDistanceN = 5
	val coarseGoalDistanceR = 25.0
	val coarseGoalAngleN = 3
	val coarseGoalAngleR = Math.Pi

	val visionSectorsNumber = 5
	val dimensionality = coarseRangeSensorN*visionSectorsNumber + 2*coarseGoalAngleN + coarseGoalDistanceN
	//val dimensionality = visionSectorsNumber + 2

	def sectorStart(i: Int, model: RobotModel): Vector = {
	  val d = model.direction
	  val pd = d rotate 90
    
	  model.position - d*(model.height/2.0) + 
     	pd*(model.width/2.0 - i/(RobotState.visionSectorsNumber-1.0)*model.width) 
	}
}