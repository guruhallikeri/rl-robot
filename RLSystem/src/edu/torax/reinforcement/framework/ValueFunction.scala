package edu.torax.reinforcement.framework

trait ValueFunction {
  def update(state: State, action: Action, delta: Double): Unit
  def apply(state: State, action: Action): Double
}
