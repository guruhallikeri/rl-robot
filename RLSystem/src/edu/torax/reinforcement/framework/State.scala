package edu.torax.reinforcement.framework

abstract class State {
  def encode: List[Double]
}
