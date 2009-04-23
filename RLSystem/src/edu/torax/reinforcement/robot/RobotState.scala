package edu.torax.reinforcement.robot
import framework.State
import gutils._

case class RobotState(env: RobotEnvironment) extends State {
  private val maxDist = Vector(env.width, env.height).length
  
//  def encode: List[Double] = {
//    val sectorAngle = env.visionAngle / RobotState.visionSectorsNumber
//    val res = for (i <- 0 until RobotState.visionSectorsNumber)
//      yield getClosestObstacleDistance(
//    	env.obstacles,																// obstacles
//    	Sector(env.model.position + env.model.direction*(env.model.height/2.0),	// beginning of vision sector
//    		   env.model.direction rotate ((env.visionAngle-sectorAngle)/2.0 - i*sectorAngle), // left ray
//    		   maxDist, sectorAngle)) / maxDist
//    val modelToGoalVec = env.goalPosition(env) - env.model.position
//    val distToGoal = modelToGoalVec.length
//    val angleToGoal = Math.acos(env.model.direction * modelToGoalVec.normalize)
//    // add angle to goal and distance to goal
//    res.toList ::: distToGoal :: angleToGoal :: Nil
//  }
  private val sectorAngle = env.visionAngle / RobotState.visionSectorsNumber
  val ranges = (for (i <- 0 until RobotState.visionSectorsNumber) yield getClosestObstacleDistance(
    env.obstacles,																			// obstacles
    Sector(env.model.position + env.model.direction*(env.model.height/2.0),					// beginning of vision sector
    	   env.model.direction rotate ((env.visionAngle-sectorAngle)/2.0 - i*sectorAngle), 	// left ray
    	   maxDist, sectorAngle)
  	)).toList
  private val modelToGoalVec = env.goalPosition(env) - env.model.position
  val goalDistance = modelToGoalVec.length
  val goalAngle = Math.acos(env.model.direction * modelToGoalVec.normalize)

  def encode: List[Double] = coarseEncode(ranges, goalDistance, goalAngle)
  
  private def getClosestObstacleDistance(obstacles: Array[RobotObstacle], sect: Sector): Double = {
	var res = maxDist
	for (obs <- obstacles) res = Math.min(res, obs distanceTo sect)
	res
  }
  
  import RobotState._
  private def coarseEncode(ranges: List[Double], goalDist: Double, goalAngle: Double): List[Double] = {
    def encoder(x: Double, N: Int, r: Double): List[Double] = {
      val w = 4*N/r
      val res = for (i <- 1 to N) yield 1.0 / (1.0 + Math.exp(w*((2*i - 1)/2.0*r/N - x)))
      res.toList
    }
    
    (ranges flatMap (x => encoder(x, coarseRangeSensorN, coarseRangeSensorR))) ::: 
    encoder(goalDist, coarseGoalDistanceN, coarseGoalDistanceR) :::
    encoder(Math.Pi - goalAngle, coarseGoalAngleN, coarseGoalAngleR) :::
    encoder(goalAngle - Math.Pi, coarseGoalAngleN, coarseGoalAngleR)
  }  
}

object RobotState {
  val coarseRangeSensorN = 3
  val coarseRangeSensorR = 8.0
  val coarseGoalDistanceN = 5
  val coarseGoalDistanceR = 25.0
  val coarseGoalAngleN = 3
  val coarseGoalAngleR = Math.Pi
  
  val visionSectorsNumber = 3
  val dimensinality = coarseRangeSensorN*visionSectorsNumber + 2*coarseGoalAngleN + coarseGoalDistanceN 
}