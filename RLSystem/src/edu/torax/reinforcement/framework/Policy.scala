package edu.torax.reinforcement.framework
import util.Random

trait Policy[A <: Action, S <: State] {
  def valueFunction: ValueFunction[A,S]
  def environment: Environment[A,S]
  
  // return best action according to a policy AND
  // its approximated value
  protected def chooseAction(state: S, inLearning: Boolean): (A, Double)
}

trait EpsGreedyPolicy[A <: Action, S <: State] extends Policy[A,S] {
  def eps: Double
  // return best action according to epsilon-greedy policy AND
  // its approximated value
  protected def chooseAction(state: S, inLearning: Boolean): (A, Double) = {
    val rnd = new Random
    if (inLearning && rnd.nextDouble <= eps) {
      val action = environment.prepareAction(rnd.nextInt(environment.actionsCount))
      val value = valueFunction(state, action)
      (action, value)
    }
    else {
      val act = environment.prepareAction(0)
      var best: (A, Double) = (act, valueFunction(state,act)) 
      for (i <- 1 until environment.actionsCount; val action = environment.prepareAction(i)) {
        val actionValue = valueFunction(state, action)
        best = if (actionValue > best._2) (action, actionValue) else best
      }	
      best
    }	
  }
}

trait SoftmaxPolicy[A <: Action, S <: State] extends Policy[A,S] {
  def T: Double
  protected def chooseAction(state: S, inLearning: Boolean): (A, Double) = {
    val actionList = for (i <- 0 until environment.actionsCount; val action = environment.prepareAction(i)) 
      yield (action, valueFunction(state, action))
    val curT = T
    val sum = (0.0 /: actionList) ((x,y) => x + Math.exp(y._2 / curT))
    var curSum = 0.0
    val rand = Math.random * sum//(new Random).nextDouble * sum
    for ((action,value) <- actionList) {
      curSum += Math.exp(value / curT)
      if (curSum >= rand) return (action, value)
    }
    throw new Exception("Softmax policy failed to produce (action, value) pair")
  }
  
}