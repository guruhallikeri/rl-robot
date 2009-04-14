package edu.torax.reinforcement.framework
import util.Random

trait Policy extends ActionProvider {
  def valueFunction: ValueFunction  
  // return best action according to a policy AND
  // its approximated value
  def chooseAction(state: State, inLearning: Boolean): (Action, Double)
}

trait EpsGreedyPolicy extends Policy {
  def eps: Double
  // return best action according to epsilon-greedy policy AND
  // its approximated value
  def chooseAction(state: State, inLearning: Boolean): (Action, Double) = {
    val rnd = new Random
    if (inLearning && rnd.nextDouble <= eps) {
      val action = prepareAction(rnd.nextInt(actionsCount))
      val value = valueFunction(state, action)
      (action, value)
    }
    else {
      var best: (Action, Double) = (null, Double.NegativeInfinity) 
      for (i <- 0 until actionsCount; val action = prepareAction(i)) {
        val actionValue = valueFunction(state, action)
        best = if (actionValue > best._2) (action, actionValue) else best
      }	
      best
    }	
  }
}

trait SoftmaxPolicy extends Policy {
  def T: Double
  def chooseAction(state: State, inLearning: Boolean): (Action, Double) = {
    val actionList = for (i <- 0 until actionsCount; val action = prepareAction(i)) yield (action, valueFunction(state, action))
    val curT = T
    val sum = (0.0 /: actionList) ((x,y) => x + Math.exp(y._2 / curT))
    var curSum = 0.0
    val rand = (new Random).nextDouble * sum
    for ((action,value) <- actionList) {
      curSum += Math.exp(value / curT)
      if (curSum >= rand) return (action, value)
    }
    throw new Exception("Softmax policy failed to produce (action, value) pair")
  }
  
}