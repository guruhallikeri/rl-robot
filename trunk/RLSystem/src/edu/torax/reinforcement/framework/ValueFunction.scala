package edu.torax.reinforcement.framework

trait ValueFunction[A <: Action, S <: State] {
  def update(state: S, action: A, delta: Double): Unit
  def apply(state: S, action: A): Double
}
