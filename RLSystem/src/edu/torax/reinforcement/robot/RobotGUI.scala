package edu.torax.reinforcement.robot

//import java.awt.event._
import javax.swing._
import net.miginfocom.swing.MigLayout

import framework._
import gutils._

class RobotGUI extends JFrame("Robot GUI") {
  val gamma = 0.95
  val alpha = 0.1
  val lambda = 0.9
  val greedyEps = 0.1

  var env = createEnvironment
  val vfunc = createValueFunction
  val actor = createActor

  private def createEnvironment: RobotEnvironment = {
    val envWidth = 40.0
    val envHeight = 40.0
    val envTurnAngle = 15.0
    val envTimeOut = 200
    val envMoveDistance = 0.9
    val envVisionAngle = 60.0
    val obsMinNumber = 4
    val obsMaxNumber = 10
    val obsMinRadius = 2.5
    val obsMaxRadius = 5.0
    val obsGap = 2.0
    val modelWidth = 1.0
    val modelHeight = 1.0
    def envObstacleGenerator(env: RobotEnvironment): Array[RobotObstacle] = {
      val N = obsMinNumber + (Math.random * (obsMaxNumber - obsMinNumber)).toInt
      println("Obstacle count: " + N)
      var obs = new Array[RobotObstacle](0)
      for (i <- 0 until N)
        obs = obs ++ Array(PolygonalRobotObstacle.generate(obs, env.width, env.height, obsGap, obsMinRadius, obsMaxRadius))
      obs
    }
    def envModelGenerator(env: RobotEnvironment): RobotModel = {
      new SimpleRobotModel(20.0, 20.0, 0.0, 1.0, 1.0, 1.0) { }
      //throw new Exception("Not Implemeted")
    }
    def envGoalPosition(env: RobotEnvironment): Vector = Vector(37.0, 37.0) //throw new Exception("Not Implemeted")
    new RobotEnvironment(
      envWidth, envHeight, envTimeOut, envTurnAngle, envMoveDistance, envVisionAngle,
      envObstacleGenerator, envModelGenerator, envGoalPosition
    )
  }
  
  private def createValueFunction = null//throw new Exception("Not Implemented")
  private def createActor = null //throw new Exception("Not Implemented")
  
  private def processMessage(event: Actor.Event): Unit = event match {
    case ev: Actor.EpisodeStarted[_,_] =>
    case ev: Actor.EpisodeFinished[_,_] =>
    case ev: Actor.StepFinished[_,_] =>
  }

  // ----   Visual components creation ----
  private val robotPane = new JPanel(new MigLayout("", "[grow,fill][180px]", "[grow,fill]"))
  private val envVisualizer = new RobotEnvironmentVisualizer
  
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  setContentPane(robotPane);
  setSize(800, 600);

  robotPane.add(envVisualizer)
  
  private val controlPanel = new JPanel(new MigLayout("wrap 1", "[grow,fill]", "[]"))
  robotPane.add(controlPanel)
  controlPanel.add(new JButton("OPA1"), "pushx")
  controlPanel.add(new JButton("OPA2"))
  controlPanel.add(new JButton("OPA3"))
  controlPanel.add(new JButton("OPA4"))
  
  setVisible(true);
  robotPane.requestFocusInWindow()
  
  envVisualizer.environment = env
}