package edu.torax.reinforcement.tests
import framework._
import gutils._

object ReinforcementTests {
  def main(args : Array[String]) : Unit = {
    (new NeuralNetworkTests).execute()
    (new VectorTests).execute()
    (new SectorTests).execute()
    (new SegmentTests).execute()
    println("All Tests Done!")
  }
}
