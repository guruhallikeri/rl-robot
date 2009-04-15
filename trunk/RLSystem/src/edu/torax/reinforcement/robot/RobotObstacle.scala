package edu.torax.reinforcement.robot

trait RobotObstacle {
  def collides(model: RobotModel): Boolean
}
