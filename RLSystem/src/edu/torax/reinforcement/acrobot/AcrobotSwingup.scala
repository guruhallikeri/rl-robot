package edu.torax.reinforcement.acrobot
import java.awt.Graphics
import java.awt.Color
import javax.swing.JComponent
import scala.swing._
import scala.swing.event._
import framework._

class AcrobotDrawer(var model: AcrobotModel) extends swing.Component {
  override def paint(g: Graphics)
  {
    val offx = 5
    val offy = 5
    val w = size.width - 2*offx
    val h = size.height - 2*offy
    
    import AcrobotModel._
    import Math._
    val x1 = l1 * sin(model.theta1)
    val y1 = l1 * cos(model.theta1)
    val x2 = x1 + l2 * sin(model.theta1 + model.theta2)
    val y2 = y1 + l2 * cos(model.theta1 + model.theta2)
   
    //println(x1 + "   " + y1 + "   " + x2 + "    " + y2)
    
    g.setColor(background)
    g.fillRect(0, 0, size.width, size.height)
    g.setColor(Color.BLACK)
    val p0 = (offx + w/2.0, offy + h/2.0)
    val p1 = (offx + w/2.0 + w/4.0*x1, offy + h/2.0 + h/4.0*y1)
    val p2 = (offx + w/2.0 + w/4.0*x2, offy + h/2.0 + h/4.0*y2)
    g.drawLine(p0._1.toInt, p0._2.toInt, p1._1.toInt, p1._2.toInt)
    g.drawLine(p1._1.toInt, p1._2.toInt, p2._1.toInt, p2._2.toInt)
  }
}

object AcrobotSwingup extends SimpleGUIApplication {
  var env = new AcrobotEnvironment
  val gamma = 0.95
  val alpha = 0.0001
  val lambda = 0.9
  val greedyEps = 0.000
  val vfunc = new SeparateNNValueFunction[AcrobotAction, AcrobotState] (
    AcrobotState.dimensionality,
    env.actionsCount,
    alpha,
    gamma,
    lambda,
    () => (Math.random - 0.5)*2.0*0.2,
    //List(5, 8),
    List(8,3),
    //List(12, 7), //-- currenty best choice
    //List(20),
    //List(20,8),
    NeuralNetwork.logisticNegFunction,
    NeuralNetwork.logisticNegDerivative
  )
  private var episodesDone = 0
  private var episodeStartStep = 0
  private var stepsDone = 0
  private def updateTitle {
    top.title = "Acrobot Swingup Demo - [EpisodesDone: " + episodesDone + ", Steps Done: " + stepsDone + "]"
  }
  private def processMessage(event: Actor.Event): Unit = event match {
    case ev: Actor.EpisodeStarted[_,_] =>
      println("Episode " + episodesDone + " Started")
      episodeStartStep = stepsDone
      
    case ev: Actor.EpisodeFinished[_,_] =>
      println("Episode Finished with " + (stepsDone - episodeStartStep + 1) + " steps")
      env = new AcrobotEnvironment
      actor.beginEpisode(env, true)
      drawer.model = env.model
      episodeStartStep = stepsDone
      episodesDone += 1
      updateTitle
      
    case ev: Actor.StepFinished[_,_] =>
      //println("Step " + stepsDone + " Finished")
      stepsDone += 1
  }
  val actor = new SarsaActor(vfunc, gamma, processMessage) with EpsGreedyPolicy[AcrobotAction,AcrobotState] {
    val eps = greedyEps
    protected var action: AcrobotAction = null
    protected var state: AcrobotState = null
  }
  
  val drawer = new AcrobotDrawer(env.model)
  val buttonPlus = new Button { text = "Torque +1" }
  val buttonZero = new Button { text = "Torque 0" }
  val buttonMinus = new Button { text = "Torque -1" }
  val buttonReset = new Button { text = "Reset acrobot!" }
  val button1Step = new Button { text = "1 step" }
  val button10Steps = new Button { text = "10 steps" }
  val button100Steps = new Button { text = "100 steps" }
  val button1000Steps = new Button { text = "1000 steps" }
  val button10000Steps = new Button { text = "10000 steps" }
  val buttonMillionSteps = new Button { text = "Million steps" }
  val top = new MainFrame {
    title = "Acrobot Swingup Demo"
    contents = new BoxPanel(Orientation.Vertical) {
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += buttonMinus
        contents += buttonZero
        contents += buttonPlus
        contents += buttonReset
        border = Swing.EmptyBorder(10, 10, 10, 10)
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += button1Step 
        contents += button10Steps 
        contents += button100Steps 
        contents += button1000Steps 
        contents += button10000Steps 
        contents += buttonMillionSteps
        border = Swing.EmptyBorder(10, 10, 10, 10)
      }
      contents += drawer
      border = Swing.EmptyBorder(30, 30, 10, 30)
    }
    listenTo(buttonPlus, buttonMinus, buttonZero, buttonReset)
    listenTo(button1Step, button10Steps, button100Steps, button1000Steps, button10000Steps, buttonMillionSteps)
    
    actor.beginEpisode(env, true)
    reactions += {
      case ButtonClicked(`buttonZero`) =>
        println("click zero")
        env.model.torque(0.0)
        drawer.repaint
      case ButtonClicked(`buttonPlus`) =>
        env.model.torque(1.0)
        drawer.repaint
      case ButtonClicked(`buttonMinus`) =>
        env.model.torque(-1.0)
        drawer.repaint
      case ButtonClicked(`buttonReset`) =>
        env = new AcrobotEnvironment
        drawer.model = env.model
        actor.beginEpisode(env,true)
        drawer.repaint
      case ButtonClicked(`button1Step`) =>
        actor.doStep()
        //updateTitle
        drawer.repaint
      case ButtonClicked(`button10Steps`) =>
        for (i <- 0 until 10) actor.doStep()
        updateTitle
        drawer.repaint
      case ButtonClicked(`button100Steps`) =>
        for (i <- 0 until 100) actor.doStep()
        updateTitle
        drawer.repaint
      case ButtonClicked(`button1000Steps`) =>
        for (i <- 0 until 1000) actor.doStep()
        updateTitle
        drawer.repaint
      case ButtonClicked(`button10000Steps`) =>
        for (i <- 0 until 10000) actor.doStep()
        updateTitle
        drawer.repaint
      case ButtonClicked(`buttonMillionSteps`) =>
        for (i <- 0 until 1000000) { 
          actor.doStep()
         // println("--> " + i)
        }
        updateTitle
        drawer.repaint
    }	
  }
}
