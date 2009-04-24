package edu.torax.reinforcement.robot

import java.awt.event._
import javax.swing._
import net.miginfocom.swing.MigLayout

import framework._
import gutils._

class RobotGUI extends JFrame("Robot Control Problem (c) Andrii Nakryiko") {
  val gamma = 0.95
  val alpha = 0.1
  val lambda = 0.0
  val greedyEps = 0.1
  
  var env = createEnvironment
  val vfunc = createValueFunction
  val actor = createActor
  actor.beginEpisode(env, true)

  private def createEnvironment: RobotEnvironment = {
    val envWidth = 40.0
    val envHeight = 40.0
    val envTurnAngle = 15.0
    val envTimeOut = 200
    val envMoveDistance = 0.9
    val envVisionAngle = 60.0
    val modelWidth = 1.0
    val modelHeight = 1.0

    new RobotEnvironment(envWidth, envHeight, envTimeOut, envTurnAngle, envMoveDistance, envVisionAngle)
  }
  
  private def createValueFunction = {
    new SeparateNNValueFunction[RobotAction, RobotState] (
    	RobotState.dimensionality,
    	env.actionsCount,
    	alpha,
    	gamma,
    	lambda,
    	() => (2.0*Math.random - 1.0)*0.1,
    	List(7),
    	NeuralNetwork.logisticFunction,
    	NeuralNetwork.logisticDerivative, 
    	NeuralNetwork.logisticFunction,
    	NeuralNetwork.logisticDerivative 
    )
  }
  private def createActor = {
  	new SarsaActor(vfunc, gamma, processMessage) with EpsGreedyPolicy[RobotAction,RobotState] {
  		val eps = greedyEps
  		protected var action: RobotAction = null
  		protected var state: RobotState = null
  	}
  }

  private var episodesDone = 0
  private var episodeStartStep = 0
  private var stepsDone = 0
  private var reachedCount = 0
  private var crashedCount = 0
  private var timedoutCount = 0
  
  def updateTitle(numer: Int, denom: Int) {
    if (numer == -1)
    	setTitle("Robot Control Problem (c) Andrii Nakryiko - [EpisodesDone: " + 
                episodesDone + ", Steps Done: " + stepsDone + "]" +
                " ..:R: " + reachedCount + "; C: " + crashedCount + "; T: " + timedoutCount + "::.."
    	)
    else
    	setTitle("Robot Control Problem (c) Andrii Nakryiko | <" + numer + "/" + denom + 
                "> | [EpisodesDone: " + episodesDone + ", Steps Done: " + stepsDone + "]" +
                " ..:R:" + reachedCount + " C:" + crashedCount + " T: " + timedoutCount + "::.."
    	)
  }

  private def processMessage(event: Actor.Event): Unit = event match {
    case ev: Actor.EpisodeStarted[_,_] =>
      println("Episode " + episodesDone + " Started")
      episodeStartStep = stepsDone
      
    case ev: Actor.EpisodeFinished[_,_] =>
      println("Episode Finished with " + (stepsDone - episodeStartStep + 1) + " steps")

      if (env.timedOut) timedoutCount += 1
      if (env.isModelCrashed) crashedCount += 1
      if (env.isGoalReached) reachedCount += 1
      
      env = createEnvironment
      actor.beginEpisode(env, true)
      envVisualizer.environment = env
      
      episodeStartStep = stepsDone
      episodesDone += 1
      //updateTitle(-1, -1)
      
    case ev: Actor.StepFinished[_,_] =>
      //println("Step " + stepsDone + " Finished")
      stepsDone += 1
  }

  // ----   Visual components creation ----
  private val robotPane = new JPanel(new MigLayout("", "[grow,fill][180px]", "[grow,fill]"))
  val envVisualizer = new RobotEnvironmentVisualizer
  envVisualizer.environment = env
  
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  setContentPane(robotPane);
  setSize(800, 600);

  robotPane.add(envVisualizer)
  
  private val controlPanel = new JPanel(new MigLayout("wrap 1", "[grow,fill]", "[]"))
  robotPane.add(controlPanel)
  
  private val resetButton = new JButton("Reset environment")
  resetButton.addActionListener(new ActionListener { def actionPerformed(event: ActionEvent) {
  	env = createEnvironment
  	envVisualizer.environment = env
  	envVisualizer.repaint
  	actor.beginEpisode(env, true)
  }})
  controlPanel.add(resetButton)
  
  private val doStepButton = new JButton("Do one step")
  doStepButton.addActionListener(new ActionListener { def actionPerformed(event: ActionEvent) {
    actor.doStep()
    updateTitle(-1,-1)
    envVisualizer.repaint()
// 	RobotLearningProcess(actor, 1, RobotGUI.this).execute
  }})
  controlPanel.add(doStepButton)

  private val doThousandButton = new JButton("Do 1'000 step")
  doThousandButton.addActionListener(new ActionListener { def actionPerformed(event: ActionEvent) {
  //	RobotLearningProcess(actor, 1000, RobotGUI.this).execute
    (new Thread(new Runnable { 
      def run {
      	for (i <- 0 until 1000) {
      	  actor.doStep()
      	  if ((i+1) % 100 == 0) {
      	    updateTitle(i+1, 1000)
      	    envVisualizer.repaint
      	  }
      	}
      }
    })).start()
    updateTitle(-1,-1)
    envVisualizer.repaint
  }})
  controlPanel.add(doThousandButton)

  private val do100ThousandButton = new JButton("Do 100'000 step")
  do100ThousandButton.addActionListener(new ActionListener { def actionPerformed(event: ActionEvent) {
  	//RobotLearningProcess(actor, 100000, RobotGUI.this).execute
    (new Thread(new Runnable { 
      def run {
      	for (i <- 0 until 100000) {
      	  actor.doStep()
      	  if ((i+1) % 1000 == 0) {
      	    updateTitle(i+1, 100000)
      	    envVisualizer.repaint
      	  }
      	}
      }
    })).start()
    updateTitle(-1,-1)
    envVisualizer.repaint
  }})
  controlPanel.add(do100ThousandButton)

  private val doMillButton = new JButton("Do million steps")
  doMillButton.addActionListener(new ActionListener { def actionPerformed(event: ActionEvent) {
    (new Thread(new Runnable { 
      def run {
      	for (i <- 0 until 1000000) {
      	  actor.doStep()
      	  if ((i+1) % 1000 == 0) {
      	    updateTitle(i+1, 1000000)
      	    envVisualizer.repaint
      	  }
      	}
      }
    })).start()
    updateTitle(-1,-1)
    envVisualizer.repaint
  }})
  controlPanel.add(doMillButton)

  private val do10MillButton = new JButton("Do 10 million steps")
  do10MillButton.addActionListener(new ActionListener { def actionPerformed(event: ActionEvent) {
    (new Thread(new Runnable { 
      def run {
      	for (i <- 0 until 10000000) {
      	  actor.doStep()
      	  if ((i+1) % 1000 == 0) {
      	    updateTitle(i+1, 10000000)
      	    envVisualizer.repaint
      	  }
      	}
      }
    })).start()
    updateTitle(-1,-1)
    envVisualizer.repaint
  }})
  controlPanel.add(do10MillButton)

  robotPane.requestFocusInWindow()
  setVisible(true);
}