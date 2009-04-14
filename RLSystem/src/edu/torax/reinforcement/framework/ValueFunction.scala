package edu.torax.reinforcement.framework

trait ValueFunction {
  def update(state: Environment#State, action: Environment#Action, delta: Double): Unit
  def apply(state: Environment#State, action: Environment#Action): Double
}
