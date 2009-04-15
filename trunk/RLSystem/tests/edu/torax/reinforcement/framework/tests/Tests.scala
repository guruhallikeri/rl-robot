package edu.torax.reinforcement.framework.tests
import Math._

object Tests {
  def main(args : Array[String]) : Unit = {
    (new NeuralNetworkTests).execute()
    println("All")

    val net = new NeuralNetwork (
      List(2, 3, 1), 
      List((x:Double) => 1.0 / (1.0 + exp(-x)), (x:Double) => 1.0 / (1.0 + exp(-x))), // (x:Double) => x),
      List((x:Double) => exp(-x) / pow(1.0 + exp(-x), 2.0), (x:Double) => exp(-x) / pow(1.0 + exp(-x), 2.0)) // (x:Double) => 1.0)
    ) {
      val gamma = 0.9
      val lambda = 0.85
      val alpha = 0.3
      val initializer = 0.5
    }
    
    println("Output: " + net.calculate(List(0.6, 0.3)))
    for (i <- 0 to 50) {
      net.tuneUp(List(0.6, 0.3), 0.5);
      println("Output #" + i + ": " + net.calculate(List(0.6, 0.3)))
    }
    println("Output: " + net.calculate(List(0.5, 0.4)))
  } 
}
