package edu.torax.reinforcement.framework

abstract class Action {
  def encode: List[Double]
}