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
  val gamma = 0.99
  val alpha = 0.1
  val lambda = 0.85
  val greedyEps = 0.000
  val minAlpha = 0.01
  val maxAlpha = 0.3
  val iterCnt = 300000
  var iCnt = 0
  val vfunc = new UsualNNValueFunction[AcrobotAction, AcrobotState] (
    env.actionsCount,
    AcrobotState.dimensionality,
    LinearDecreasingFunction(minAlpha, maxAlpha, iterCnt),
    NeuralNetwork.UniformRandomInitializer(-0.3, 0.3),
    Array(7, 3),
    NeuralNetwork.ActFunc.logistic,
    NeuralNetwork.ActFunc.identity
  )
  private var episodesDone = 0
  private var episodeStartStep = 0
  private var stepsDone = 0

  def updateTitle(numer: Int, denom: Int) {
    if (numer == -1)
    	top.title = "Acrobot Swingup Problem (c) Andrii Nakryiko - [EpisodesDone: " + episodesDone + ", Steps Done: " + stepsDone + "]"
    else
    	top.title = "Acrobot Swingup Problem (c) Andrii Nakryiko | <" + numer + "/" + denom + "> | [EpisodesDone: " + episodesDone + ", Steps Done: " + stepsDone + "]"
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
      //updateTitle(-1,-1)
      
    case ev: Actor.StepFinished[_,_] =>
      //println("Step " + stepsDone + " Finished")
      stepsDone += 1
  }
  val actor = new SarsaActor(vfunc, gamma, processMessage) with EpsGreedyPolicy[AcrobotAction,AcrobotState] {
    val eps = LinearDecreasingFunction(greedyEps, greedyEps, 1)
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
        updateTitle(-1,-1)
        drawer.repaint
      case ButtonClicked(`button100Steps`) =>
        (new Thread(new Runnable { 
        	def run {
        		for (i <- 0 until 100) {
        			actor.doStep()
        			if ((i+1) % 10 == 0) {
        				updateTitle(i+1, 100)
        				drawer.repaint
        			}
        		}
        	}
        })).start()
        updateTitle(-1,-1)
        drawer.repaint
      case ButtonClicked(`button1000Steps`) =>
        (new Thread(new Runnable { 
        	def run {
        		for (i <- 0 until 1000) {
        			actor.doStep()
        			if ((i+1) % 10 == 0) {
        				updateTitle(i+1, 1000)
        				drawer.repaint
        			}
        		}
        	}
        })).start()
        updateTitle(-1,-1)
        drawer.repaint
      case ButtonClicked(`button10000Steps`) =>
        (new Thread(new Runnable { 
        	def run {
        		for (i <- 0 until 10000) {
        			actor.doStep()
        			if ((i+1) % 100 == 0) {
        				updateTitle(i+1, 10000)
        				drawer.repaint
        			}
        		}
        	}
        })).start()
        updateTitle(-1,-1)
        drawer.repaint
      case ButtonClicked(`buttonMillionSteps`) =>
        (new Thread(new Runnable { 
        	def run {
        		for (i <- 0 until 1000000) {
        			actor.doStep()
        			if ((i+1) % 1000 == 0) {
        				updateTitle(i+1, 1000000)
        				drawer.repaint
        			}
        		}
        	}
        })).start()
        updateTitle(-1,-1)
        drawer.repaint
    }	
  }
}
