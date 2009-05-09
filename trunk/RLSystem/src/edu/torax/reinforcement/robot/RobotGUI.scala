package edu.torax.reinforcement.robot

import java.awt.event._
import javax.swing._
import net.miginfocom.swing.MigLayout

import framework._
import gutils._

class RobotGUI extends JFrame("Robot Control Problem (c) Andrii Nakryiko") {
  val gamma = 0.99
//  val alpha = 0.1
//  val lambda = 0.8
//  val greedyEps = 0.1
  
  var env = createEnvironment
  val vfunc = createValueFunction
  val actor = createActor
  actor.beginEpisode(env, true)

  private def createEnvironment: RobotEnvironment = {
    val envWidth = 40.0
    val envHeight = 40.0
    val envTurnAngle = 10.0
    val envTimeOut = 200
    val envMoveDistance = 0.9
    val envVisionAngle = 75.0
    val modelWidth = 1.0
    val modelHeight = 1.0

    new RobotEnvironment(envWidth, envHeight, envTimeOut, envTurnAngle, envMoveDistance, envVisionAngle)
  }
  
  private def createValueFunction = {
 		val minAlpha = 0.01
 		val maxAlpha = 0.3
 		val iterCnt = 500000
 		var iCnt = 0
    new UsualNNValueFunction[RobotAction, RobotState] (
    	env.actionsCount,
    	RobotState.dimensionality,
      () => {
        iCnt += 1
        val r = Math.max(minAlpha, maxAlpha*(iterCnt-iCnt)/iterCnt)
        //println("--- " +r)
        r
      },
    	gamma,
    	Double.NaN,
    	() => 0.0,
    	Array(13,8),
    	NeuralNetwork.logistic,
    	NeuralNetwork.identity
    )
  }
  private def createActor = {
  	new SarsaActor(vfunc, gamma, processMessage) with EpsGreedyPolicy[RobotAction,RobotState] {
  	  private val minEps = 0.01
  	  private val maxEps = 0.2
  	  private val iterCnt = 500000
  	  private var iterDone = 0
  	  //val eps = greedyEps
  		def eps = {
  		  iterDone += 1
        minEps max (iterCnt - iterDone + 0.0)/iterCnt*maxEps
  		}
  	}
//  	new SarsaActor(vfunc, gamma, processMessage) with SoftmaxPolicy[RobotAction,RobotState] {
//  	  private var stepsDone = 0;
//      private val maxSteps = 1000000;
//      private val minT = 0.01;
//      private val maxT = 0.05;
//  		def T() = {
//  		  stepsDone += 1;
//  		  val t = minT max maxT*(maxSteps - stepsDone)/(0.0+maxSteps)
//  		  //println(" -- " + t)
//  		  t
//  		}
//  	}
  }

  private var episodesDone = 0
  private var episodeStartStep = 0
  private var stepsDone = 0
  private var reachedCount = 0
  private var crashedCount = 0
  private var timedoutCount = 0
//  private var lastReachedCount = 0
//  private var lastCrashedCount = 0
//  private var lastTimedoutCount = 0
  
  def updateTitle(numer: Int, denom: Int) {
    val sum = reachedCount + crashedCount + timedoutCount
//    val lastSum = lastReachedCount + lastCrashedCount + lastTimedoutCount
    if (numer == -1) {
    	setTitle("Robot Control - [E: " + 
                episodesDone + ", S: " + stepsDone + "]" +
                " (R: " + reachedCount + "; C: " + crashedCount + "; T: " + timedoutCount + ") " +
                "{ " + ((0.0 + reachedCount)/sum*100.0).toInt + "-" + 
                ((0.0 + crashedCount)/sum*100.0).toInt + "-" +
                ((0.0 + timedoutCount)/sum*100.0).toInt + "}"
    	)
    } else {
    	setTitle("Robot Control | <" + numer + "/" + denom + 
                "> | [E: " + episodesDone + ", S: " + stepsDone + "]" +
                " (R:" + reachedCount + " C:" + crashedCount + " T: " + timedoutCount + ") " +
                "{ " + ((0.0 + reachedCount)/sum*100.0).toInt + "-" + 
                ((0.0 + crashedCount)/sum*100.0).toInt + "-" +
                ((0.0 + timedoutCount)/sum*100.0).toInt + "}"
    	)
    }
  }

  val lastMax = 5000
  val lasts = new Array[Int](lastMax)
  var lastB = 0
  var lastE = 0
  var lastCnt = 0
  private def processMessage(event: Actor.Event): Unit = event match {
    case ev: Actor.EpisodeStarted[_,_] =>
      println("Episode " + episodesDone + " Started")
      episodeStartStep = stepsDone
      
    case ev: Actor.EpisodeFinished[_,_] =>
      println("Episode Finished with " + (stepsDone - episodeStartStep + 1) + " steps")

      if (lastCnt == lastMax) {
        lasts(lastB) match {
          case 0 => timedoutCount -= 1
          case 1 => crashedCount -= 1
          case 2 => reachedCount -= 1
        }
        lastB = if (lastB == lastMax-1 ) 0 else lastB + 1
        lastCnt -= 1
      }
      if (env.timedOut) {
        timedoutCount += 1
        lasts(lastE) = 0
      } else if (env.isModelCrashed) {
        crashedCount += 1
        lasts(lastE) = 1
      } else if (env.isGoalReached) {
        reachedCount += 1
        lasts(lastE) = 2
      }
      lastE = if (lastE == lastMax-1 ) 0 else lastE + 1
      lastCnt += 1
      
      env = createEnvironment
      actor.beginEpisode(env, true)
      envVisualizer.environment = env
      
      episodeStartStep = stepsDone
      episodesDone += 1
      //updateTitle(-1, -1)
      
    case ev: Actor.StepFinished[_,_] =>
      //println("Step " + stepsDone + " Finished")
  //    println("--------------------------------------")
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
  
  private val debugButton = new JButton("Debug information")
  debugButton.addActionListener(new ActionListener { def actionPerformed(event: ActionEvent) {
    println("Environment obstacles:")
    println(env.obstacles.toList)
    println("Model position: " + env.model.position + "; direction: " + env.model.direction)
    println("Model bound box: " + env.model.boundBox)
    println("Env state: " + env.state)
    println("Env goal: " + env.goal)
  }})
  controlPanel.add(debugButton)

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

  private val do500ThousandButton = new JButton("Do 500000 steps")
  do500ThousandButton.addActionListener(new ActionListener { def actionPerformed(event: ActionEvent) {
    (new Thread(new Runnable { 
      def run {
      	for (i <- 0 until 500000) {
      	  actor.doStep()
      	  if ((i+1) % 1000 == 0) {
      	    updateTitle(i+1, 500000)
      	    envVisualizer.repaint
      	  }
      	}
      }
    })).start()
    updateTitle(-1,-1)
    envVisualizer.repaint
  }})
  controlPanel.add(do500ThousandButton)

  private val doMillButton = new JButton("Do 1 million steps")
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

  private val do3MillButton = new JButton("Do 3 million steps")
  do3MillButton.addActionListener(new ActionListener { def actionPerformed(event: ActionEvent) {
    (new Thread(new Runnable { 
      def run {
      	for (i <- 0 until 3000000) {
      	  actor.doStep()
      	  if ((i+1) % 1000 == 0) {
      	    updateTitle(i+1, 3000000)
      	    envVisualizer.repaint
      	  }
      	}
      }
    })).start()
    updateTitle(-1,-1)
    envVisualizer.repaint
  }})
  controlPanel.add(do3MillButton)

  private val do5MillButton = new JButton("Do 5 million steps")
  do5MillButton.addActionListener(new ActionListener { def actionPerformed(event: ActionEvent) {
    (new Thread(new Runnable { 
      def run {
      	for (i <- 0 until 5000000) {
      	  actor.doStep()
      	  if ((i+1) % 1000 == 0) {
      	    updateTitle(i+1, 5000000)
      	    envVisualizer.repaint
      	  }
      	}
      }
    })).start()
    updateTitle(-1,-1)
    envVisualizer.repaint
  }})
  controlPanel.add(do5MillButton)

  robotPane.requestFocusInWindow()
  setVisible(true);
}